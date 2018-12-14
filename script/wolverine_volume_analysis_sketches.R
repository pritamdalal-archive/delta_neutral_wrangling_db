# loading packages
library(tidyverse)
library(bizdays)
library(tictoc)


# initializing bizdays libraries
load_rmetrics_calendars(2015:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


# sourcing functions
source("function/db_connection.R")
source("function/option_all.R")

# getting option prices for several days via brute force
dt_trade <- as.Date("2018-05-01")
df_may1 <- option_all(db_connection(), dt_trade)
dt_trade <- as.Date("2018-05-02")
df_may2 <- option_all(db_connection(), dt_trade)
dt_trade <- as.Date("2018-05-03")
df_may3 <- option_all(db_connection(), dt_trade)
dt_trade <- as.Date("2018-05-04")
df_may4 <- option_all(db_connection(), dt_trade)


# checking what percentage of options had some volume
df_may1 %>% filter(volume > 0) %>% nrow() /
    df_may1 %>% nrow()
df_may2 %>% filter(volume > 0) %>% nrow() /
    df_may2 %>% nrow()
df_may3 %>% filter(volume > 0) %>% nrow() /
    df_may3 %>% nrow()
df_may4 %>% filter(volume > 0) %>% nrow() /
    df_may4 %>% nrow()



# checking the overlap between supports
df_may1 %>% filter(volume > 0) %>% 
    inner_join(
        df_may2 %>% filter(volume > 0)
        , by = c("underlying_symbol", "type", "strike", "expiration")
    )


# concepts to consider
# 1) n-day support
# 2) support percentage
# 3) rank correlation between the days in an n-day support
# 4) within in n-day support, the share of volume the 1-5,6-30, 31-100 
# 5) on any given day, the set of all option that traded the previous 5 days
#    but that did not expire on any of the days