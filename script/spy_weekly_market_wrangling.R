# clearing shit out
rm(list=ls())
cat("\014")

# loading packages
library(tidyverse)
library(lubridate)
library(bizdays)
library(fOptions)
library(tictoc)


source("function/db_connection.R")
source("function/option_chain.R") 
source("function/implied_forward.R")
source("function/otm_all.R") 
source("function/otm_clean.R")
source("function/missing_data.R")
source("function/greeks.R")
source("function/greeks_exp.R")
source("function/swap_rate.R")


# initializing bizdays libraries
load_rmetrics_calendars(2015:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")




# creating the dataframe that will capture the chain history.
# start with every sunday and friday (probably can just start with Friday)
df_chain <-
    tibble(
        friday = seq(ymd(20161216), ymd(20180727), "weeks")
    ) %>% 
    mutate(
        # check if Friday is a business day
        fri_biz_day = is.bizday(friday) 
        # initializing expiration column
        , expiration = seq(ymd(20161216), ymd(20180727), "weeks") 
    )


# if Friday is not a business day, make expiration the Thursday prior
for (ix_exp in 1:nrow(df_chain)){
    if(df_chain$fri_biz_day[ix_exp]){
        df_chain$expiration[ix_exp] <- 
            df_chain$friday[ix_exp]
    } else {
        df_chain$expiration[ix_exp] <- 
            add.bizdays(df_chain$friday[ix_exp], -1)
    }
}

# excution for a given expiration is the previous weekly expiration
df_chain$execution <- df_chain$expiration %>% dplyr::lag(1) 
# removing the first row
df_chain <- df_chain[-1,]

tic()
df_chain_hist = tibble()
df_opt_hist = tibble()
df_chain$num_opts = NA_integer_
df_chain$exec_day_volume = NA_integer_
#df_chain$mid_swap_rate = NA_real_
for (ix_exp in 1:nrow(df_chain)){
    
    # grabbing the execution and expriation
    dt_execution <- df_chain$execution[ix_exp]
    dt_expiration <- df_chain$expiration[ix_exp]
    
    # calculuating days to expiration
    d2x = bizdays(dt_execution, dt_expiration)   
    
    # grabbing the option chain on execution from database
    df_opt_all <- 
        option_chain(
            db_conn = db_connection()
            , trade_date = dt_execution
            , underlying = "SPY"
            , expiration = dt_expiration
            , exclude_zero_bid = TRUE
        )
    
    
    
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, d2x, dbl_implied_forward)
    
    
    #-------------------#
    # updating df_chain #
    #-------------------#
    #recording execution day volume in df_chain
    df_chain$exec_day_volume[ix_exp] = df_opt_all$volume %>% sum()
    # recording number of options
    df_chain$num_opts[ix_exp] = df_otm %>% nrow()
    
    
    # updating updating df_chain_hist
    dbl_swap_rate <- swap_rate(df_otm, d2x) #change this to named vector
    df_chain_hist <- 
        df_chain_hist %>% 
            bind_rows(
                tibble(
                    underlying = "SPY"
                    , expiration = dt_expiration
                    , trade_date = dt_execution
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )

            
    # updating option history
    df_opt_hist <- bind_rows(df_opt_hist, df_otm)
    
    # sequence of post-execution business days
    dt_post_exec_td <- 
        bizseq(add.bizdays(dt_execution, 1), dt_expiration)

    #loop through the trading days and grab
    #the price history for all the options in df_otm
    for (ix_td in 1:(length(dt_post_exec_td))){
        
        dt_trade <- dt_post_exec_td[ix_td]
        
        # calculuating days to expiration
        d2x = bizdays(dt_trade, dt_expiration)
        
        df_opt_px_all <-
           option_chain(
               db_conn = db_connection()
               , trade_date = dt_trade
               , underlying = "SPY"
               , expiration = dt_expiration
               , exclude_zero_bid = FALSE
           )
        
        
        # calculating the implied forward price
        if(dt_trade == dt_expiration){
            dbl_implied_forward <- 
                mean(df_opt_px_all$underlying_price[1], rm.na = TRUE)
        } else {
            dbl_implied_forward <- implied_forward(df_opt_px_all)    
        }
        
        # updating chain hist
        if(dt_trade == dt_expiration){
            dbl_swap_rate = c(0, 0, 0)
        } else {
            swap_rate(df_otm, d2x)
        }
        df_chain_hist <- 
            df_chain_hist %>% 
            bind_rows(
                tibble(
                    underlying = "SPY"
                    , expiration = dt_expiration
                    , trade_date = dt_trade
                    , implied_forward = dbl_implied_forward
                    , bid_swap_rate = dbl_swap_rate[1]
                    , ask_swap_rate = dbl_swap_rate[2]
                    , mid_swap_rate = dbl_swap_rate[3]
                ) 
            )
        
        
        df_opt_px <-
           df_otm %>%
               select(underlying_symbol, expiration, type, strike) %>%
               left_join(
                   df_opt_px_all
                   , by = c("underlying_symbol", "type", "strike", "expiration")
               )
        
        # filling in some missing data in case of empty prices
        df_opt_px <- missing_data(df_opt_px_all, df_opt_px, dt_trade)
        
        if(dt_trade != dt_expiration){
            # recalculating greeks
            df_opt_px <- greeks(df_opt_px, d2x, dbl_implied_forward)    
        } else {
           df_opt_px <- greeks_exp(df_opt_px) 
        }
        
        # updating df_opt_hist
        df_opt_hist <- bind_rows(df_opt_hist, df_opt_px)
    }
    
    print(dt_execution)
}
toc()

write_csv(df_chain, "data_output/spy_weekly_chain.csv")
write_csv(df_chain_hist, "data_output/spy_weekly_chain_hist.csv")
write_csv(df_opt_hist, "data_output/spy_weekly_opt_hist.csv")
