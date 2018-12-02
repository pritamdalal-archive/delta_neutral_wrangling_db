## The purpose of this script is to scale the PNLs correctly and then
## come up with the correct CSV files for the FM 5990 students to ultimately
## do their project with.

######################
## loading packages ##
######################
library(tidyverse)



#####################
## reading in data ##
#####################
df_chain <- 
    read_csv("../delta_neutral_data_output/spy_weekly_chain.csv")
df_trade_master <- 
    read_csv("../delta_neutral_data_output/spy_weekly_trade_master.csv")
df_pnl_master <- 
    read_csv("../delta_neutral_data_output/spy_weekly_pnl_master.csv")



##################################
## creating df_position_scaling ##
##################################
df_call <- df_trade_master %>% dplyr::filter(type == "call")
df_put <- df_trade_master %>% dplyr::filter(type == "put")    

# The main idea behind this section is that we want to sell about $1 in premium
# every month to make sure that all the position sizes are similar. I decided
# to use the mid prices to scale the positions, rather than the bid, because
# I didn't want to take especially large positions in an option, just because
# it has a very wide bid/ask spread.
df_position_scaling <-
    df_chain %>% 
        left_join(
            df_put
            , by = c("underlying", "expiration", "execution")
            , suffix = c(".put", "put1")
        ) %>%
        select(
            variation, underlying, expiration, execution, d2x
            , bid, ask, mid
        ) %>% 
        inner_join(
            df_call
            , by = c("variation", "expiration", "underlying", "execution")
            , suffix = c(".put", ".call")
        ) %>% 
        select(
            variation, underlying, expiration, execution, d2x
            , bid.put, ask.put, mid.put
            , bid.call, ask.call, mid.call
        ) %>% 
        mutate(
            strangle_mult = 
                (d2x * 0.05) / (mid.put + mid.call)
            
            , strangle_prem_sold = 
                (bid.put + bid.call) * ((d2x * 0.05) / (mid.put + mid.call)) 
            
            , put_mult = (d2x * 0.05) / mid.put
            
            , put_prem_sold = 
                bid.put * ((d2x * 0.05) / mid.put)
            
            , call_mult = (d2x * 0.05) / mid.call
            
            , call_prem_sold = 
                bid.call * ((d2x * 0.05) / mid.call)
        )


# testing
df_position_scaling %>% 
    group_by(variation) %>% 
    summarize(
        tot_put = sum(put_prem_sold)
        , tot_all = sum(call_prem_sold)
        , tot_strangle = sum(strangle_prem_sold)
    )


#############################################################
## adding unity_mult position size scalar factor to df_pnl ##
#############################################################
# strangles
df_strangle_pnl_scaled <- 
    df_pnl_master %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, strangle_mult
        )

# puts
df_put_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "put") %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, put_mult
        )

# calls
df_call_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "call") %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, call_mult
        )



#######################
## writing CSV files ##
#######################
write_csv(df_position_scaling, "spy_weekly_position_scaling.csv")
write_csv(df_strangle_pnl_scaled, "spy_weekly_strangle_pnl.csv")
write_csv(df_put_pnl_scaled, "spy_weekly_put_pnl.csv")
write_csv(df_call_pnl_scaled, "spy_weekly_call_pnl.csv")


#############
## TESTING ##
#############
# df_pnl_scaled %>% 
#     dplyr::filter(underlying %in% c("SPY", "IWM", "QQQ", "DIA")) %>% 
#     mutate(
#         scaled_naked = ttd_opt_pnl * unity_mult
#         , scaled_dh = ttd_tot_pnl * unity_mult
#     ) %>% 
#     dplyr::filter(variation == 0.3) %>%
#     group_by(trade_date) %>% 
#     summarize(
#         naked = sum(scaled_naked)
#         , delta_hedge = sum(scaled_dh)) %>% 
#     ggplot() +
#         geom_line(aes(x = trade_date, y = naked), color = 'green') +
#         geom_line(aes(x = trade_date, y = delta_hedge), color = 'blue')


