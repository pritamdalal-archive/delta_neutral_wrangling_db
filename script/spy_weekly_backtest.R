# loading packages
library(tidyverse)
library(lubridate)
library(bizdays)
library(fOptions)


source("function/db_connection.R")
source("function/option_chain.R") 
source("function/implied_forward.R")
source("function/otm_all.R") 
source("function/otm_clean.R")
source("function/greeks.R")
source("function/swap_rate.R")


# initializing bizdays libraries
load_rmetrics_calendars(2015:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")




# creating the dataframe with the expriation dates
# start with every sunday and friday (probably can just start with Friday)
df_exp <-
    tibble(
        sunday = seq(ymd(20170101), ymd(20180722), "weeks")
        , friday = seq(ymd(20170106), ymd(20180727), "weeks")
    ) 
# check if Friday is a business day
# initializing expiration column
df_exp <- 
    df_exp %>% 
    mutate(
        fri_biz_day = is.bizday(friday)
        , expiration = seq(ymd(20170106), ymd(20180727), "weeks")
    )


# if Friday is not a business day, make expiration the Thursday prior
for (ix in 1:nrow(df_exp)){
    if(df_exp$fri_biz_day[ix]){
        df_exp$expiration[ix] <- df_exp$friday[ix]
    } else {
        df_exp$expiration[ix] <- add.bizdays(df_exp$friday[ix], -1)
    }
}
df_exp 


df_market_history = tibble()
df_exp$num_opts = NA_integer_
df_exp$swap_rate = NA_real_
for (ix in 2:nrow(df_exp)){
    
    # grabbing the execution and expriation
    dt_execution <- df_exp$expiration[ix - 1]
    dt_expiration <- df_exp$expiration[ix]
    
    # calculuating days to expiration
    d2x = bizdays(dt_execution, dt_expiration)   
    
    # grabbing the option chain on execution from database
    df_chain <- 
        option_chain(
            db_conn = db_connection()
            , trade_date = dt_execution
            , underlying = "SPY"
            , expiration = dt_expiration
        )
    
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_chain)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_chain, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, d2x, dbl_implied_forward)
    # recording number of options
    df_exp$num_opts[ix] = df_otm %>% nrow()
    # recording swap - rate
    df_exp$swap_rate[ix] = swap_rate(df_otm, d2x)[3]

    # appending the market history
    df_market_history %>% bind_rows(df_market_history, df_otm)
    
    # sequence of post-execution business days
    dt_post_exec_td <- 
        bizseq(add.bizdays(dt_execution, 1), dt_expiration)
    
    
    ###################################################################
    ## SOMETHING IS BROKEN IN HERE - I THINK IT'S THE IV CALCULATION ##
    ###################################################################
    # loop through the trading days and grab
    # the price history for all the options in df_otm
    # for (ix_td in 1:(length(dt_post_exec_td)-1)){
    #    dt_trade <- dt_post_exec_td[ix_td]
    #    
    #    # calculuating days to expiration
    #    d2x = bizdays(dt_trade, dt_expiration) 
    #    
    #    df_options <- 
    #        option_chain(
    #            db_conn = db_connection()
    #            , trade_date = dt_trade
    #            , underlying = "SPY"
    #            , expiration = dt_expiration
    #        )
    #    
    #    dbl_implied_forward <- implied_forward(df_options)
    #    
    #    df_opt_hist <- 
    #        df_otm %>% 
    #            select(underlying_symbol, expiration, type, strike) %>% 
    #            left_join(
    #                df_options 
    #                , by = c("underlying_symbol", "type", "strike", "expiration")
    #            )
    #    
    #    df_opt_hist <- greeks(df_opt_hist, d2x, dbl_implied_forward)
    #    
    #    df_market_history <- bind_rows(df_market_history, df_opt_hist)
    # }
}
