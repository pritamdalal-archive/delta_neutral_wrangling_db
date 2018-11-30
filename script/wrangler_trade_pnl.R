######################
## loading packages ##
######################
library(tidyverse)
library(tictoc)


########################
## sourcing functions ##
########################
source("function/column_specification.R")


#####################
## reading in data ##
#####################
df_chain <- 
    read_csv(
        "../delta_neutral_data_output/monthly_chain.csv"
        , col_types = cols()
    )

df_chain_hist <- 
    read_csv(
        "../delta_neutral_data_output/monthly_chain_hist.csv"
        , col_types = cols()
    )

df_opt_hist <-
    read_csv(
        "../delta_neutral_data_output/monthly_opt_hist.csv"
        , col_types = column_specification("opt_hist")
    )

df_underlying <- 
    read_csv(
        "data_input/monthly_underlying.csv", col_types = cols()    
    )


#############################
## df_trades initial setup ##
#############################
# creating the list of all options (by parameters) that I am going to trade
df_trade <-
    # cartesian product of all underlying, expirations, variations
    crossing(
        variation = c(0.5, 0.3, 0.1)
        , type = c("put", "call")
        , expiration = df_chain$expiration 
        , underlying = df_underlying$underlying    
    ) %>%  
    # this inner-join has the effect of adding back in the execution dates
    inner_join(
        df_chain %>% select(underlying, expiration, execution)
        , by = c("underlying", "expiration")
    )


############################################################
## creating df_trades dataframe by finding actual options ##
############################################################
# looping through cartesian product of all trade parameters and choosing 
# options from df_market_history that fits each parameter
# will hold all options.
# 2 min - 50 underlyings 1.5 years of monthly

df_all_options <- tibble()

tic()
# iterating through df_trades
for (ix in 1:nrow(df_trade)){
    # grabbing option from df_market_history
    df_option <-
        df_opt_hist %>% 
        dplyr::filter(underlying_symbol == df_trade$underlying[ix]) %>% 
        dplyr::filter(expiration == df_trade$expiration[ix]) %>% 
        dplyr::filter(data_date == df_trade$execution[ix]) %>% 
        dplyr::filter(type == df_trade$type[ix])  %>% 
        dplyr::filter(
            abs(my_delta - df_trade$variation[ix]) == 
                min(abs(my_delta - df_trade$variation[ix]))
        )
    
    # adding to dataframe that holds all options
    df_all_options <- bind_rows(df_all_options, df_option)
}
toc()


# adding columns of df_all_options to df_trades
df_trade <- 
    bind_cols(df_trade, df_all_options)


## checking the average min and max deltas
df_trade %>% 
    group_by(variation, type) %>%
    summarize(
        avg_delta = mean(my_delta)
        , min_delta = min(my_delta)
        , max_delta = max(my_delta)
    )



#################################################
## getting option history and calculating PNLs ##
#################################################
cp <- function(type){
    if (type == "put") { return(-1) }
    if (type == "call") { return(1) }
}

df_pnl <- tibble()

tic()
# iterating through all trades and grabbing all histories 
# (this takes 11 minutes)
for (ix in 1:(nrow(df_trade))){
    
    # querying option history for price data
    df_opt <-
        df_opt_hist %>% 
        dplyr::filter(underlying_symbol == df_trade$underlying[ix]) %>% 
        dplyr::filter(expiration == df_trade$expiration[ix]) %>% 
        dplyr::filter(type == df_trade$type[ix]) %>% 
        dplyr::filter(strike == df_trade$strike[ix]) %>% 
        dplyr::filter(data_date >= df_trade$execution[ix]) %>% 
        arrange(data_date) %>% 
        mutate(
            variation = df_trade$variation[ix]
            , dly_opt_pnl = 0
            #, ttd_opt_pnl = 0
            , dly_dh_pnl = 0
            #, ttd_dh_pnl = 0
            , dly_opt_mid_pnl = 0
            #, ttd_opt_mid_pnl = 0
        )
    
    # iterating through price data to calculate PNLs
    for (ix_td in 1:nrow(df_opt)) {
        
        ## daily PNL
        if (ix_td == 1) {
            # selling at bid
            df_opt$dly_opt_pnl[1] <- df_opt$bid[1] - df_opt$ask[1]
            # delta-hedge
            df_opt$dly_dh_pnl[1] <- 0
            # selling at mid
            df_opt$dly_opt_mid_pnl[1] <- 0
        } else {
            # selling at bid
            df_opt$dly_opt_pnl[ix_td] <- 
                df_opt$ask[ix_td - 1] - df_opt$ask[ix_td]
            # delta-hedge
            df_opt$dly_dh_pnl[ix_td] <-
                cp(df_opt$type[ix_td]) * df_opt$my_delta[ix_td - 1] *
                (df_opt$underlying_price[ix_td] - df_opt$underlying_price[ix_td - 1])
            # selling at mid    
            df_opt$dly_opt_mid_pnl[ix_td] <- 
                df_opt$mid[ix_td - 1] - df_opt$mid[ix_td]
        }
        
        ## cummulative PNL
        # selling at bid
        #df_opt$ttd_opt_pnl[ix_td] <- sum(df_opt$dly_opt_pnl[1:ix_td])
        # delta-hedge
        #df_opt$ttd_dh_pnl[ix_td] <- sum(df_opt$dly_dh_pnl[1:ix_td])
        # selling at mid
        #df_opt$ttd_opt_mid_pnl[ix_td] <- sum(df_opt$dly_opt_mid_pnl[1:ix_td])
    }
    df_pnl <- bind_rows(df_pnl, df_opt)
    
}
toc()

# adding total_pnl columms (option plus delta-hedge)
df_pnl <-
    df_pnl %>% 
    mutate(
        dly_tot_pnl = dly_opt_pnl + dly_dh_pnl
        , dly_tot_mid_pnl = dly_opt_mid_pnl + dly_dh_pnl
        #, ttd_tot_pnl = ttd_opt_pnl + ttd_dh_pnl
        #, ttd_tot_mid_pnl = ttd_opt_mid_pnl + ttd_dh_pnl
    )


######################################
## preliminary graphing and testing ##
######################################
df_pnl %>% 
    group_by(variation) %>% 
    summarize(
        total = sum(dly_opt_pnl)
    )


# cummulative PNL graph
df_pnl %>% 
    group_by(variation, data_date) %>% 
    summarize(
        daily = sum(dly_opt_pnl)
    ) %>% 
    mutate(
        ttd = cumsum(daily)
    ) %>% 
    ggplot() +
        geom_line(aes(x = data_date, y = ttd, color = factor(variation)))
 

# sharpe in 2017
df_pnl %>% 
    #dplyr::filter(data_date < "2018-01-01") %>% 
    group_by(variation) %>% 
    summarize(
        sharpe_ratio = (mean(dly_opt_pnl) / sd(dly_opt_pnl)) * sqrt(252)
    )


# writing files to CSV
# write_csv(df_pnl, "monthly_trade_pnl_master.csv")
# write_csv(df_trade, "monthly_trade.csv")
























