# loading packages
library(tidyverse)
library(bizdays)
library(tictoc)
library(lubridate)

# initializing bizdays libraries
load_rmetrics_calendars(2015:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


# sourcing functions
source("function/db_connection.R")
source("function/option_all.R")




#####################################
## grabbing data from the database ##
#####################################
# current trade date
dt_current <- ymd(20180504)
df_lag00 <- option_all(db_connection(), dt_current)

# lag01
dt_trade <- offset(dt_current, -1)
df_lag01 <- option_all(db_connection(), dt_trade)

# lag02
dt_trade <- offset(dt_current, -2)
df_lag02 <- option_all(db_connection(), dt_trade)

# lag03
dt_trade <- offset(dt_current, -3)
df_lag03 <- option_all(db_connection(), dt_trade)

# lag04
dt_trade <- offset(dt_current, -4)
df_lag04 <- option_all(db_connection(), dt_trade)


# number of options listed on current day
int_one_day_listed <- df_lag00 %>% nrow()

# one day support
int_one_day_support <- 
    df_lag00 %>% 
    filter(volume > 0) %>% 
    nrow()


#####################
## volume analysis ##
#####################
# total volume
int_total_volume <- 
    df_lag00 %>% .$volume %>% sum()

# otm volume 
int_otm_volume <- 
    df_lag00 %>% 
    filter(
        (type == "call" & underlying_price <= strike) |
            (type == "put" & underlying_price >= strike)
    ) %>% 
        .$volume %>% 
        sum()

# itm volume
int_itm_volume <- 
    df_lag00 %>% 
    filter(
        (type == "call" & underlying_price > strike) |
        (type == "put" & underlying_price < strike)
    ) %>% 
    .$volume %>% 
    sum()

# top 1% of options
int_00_01_opt_vol <- 
    df_lag00 %>% 
        filter(volume > 0) %>% 
        filter(percent_rank(volume) > .99) %>% 
        .$volume %>% 
        sum()

# top 2-5% of options
int_02_05_opt_vol <-
    df_lag00 %>% 
        filter(volume > 0) %>% 
        filter(
            (percent_rank(volume) > 0.95) &
            (percent_rank(volume) <= 0.99)
        ) %>% 
        .$volume %>% 
        sum()


# top 6-10% of options
int_06_10_opt_vol <-
    df_lag00 %>% 
        filter(volume > 0) %>% 
        filter(
            (percent_rank(volume) > 0.90) &
            (percent_rank(volume) <= 0.95)
        ) %>% 
        .$volume %>% 
        sum()

# top 11-20%
int_11_20_opt_vol <-
    df_lag00 %>% 
        filter(volume > 0) %>% 
        filter(
            (percent_rank(volume) > 0.80) &
            (percent_rank(volume) <= 0.90)
        ) %>% 
        .$volume %>% 
        sum()


# creating the dataframe that contains all the days together
df_five_days <- tibble()
# filtering out all the early expiring options
df_five_days <- 
    df_five_days %>% 
    bind_rows(df_lag00) %>% 
    bind_rows(df_lag01 %>% filter(expiration >= dt_current)) %>% 
    bind_rows(df_lag02 %>% filter(expiration >= dt_current)) %>% 
    bind_rows(df_lag03 %>% filter(expiration >= dt_current)) %>% 
    bind_rows(df_lag04 %>% filter(expiration >= dt_current))
    

# number of distinct, non-expiring options listed over five days
int_five_day_listed <- 
    df_five_days %>% 
        distinct(
            underlying_symbol, expiration
            , type, strike
        ) %>% 
        nrow()


# 5-day support dataframe
df_five_day_support <-
    df_five_days %>% 
        group_by(
            underlying_symbol, expiration
            , type, strike
        ) %>% 
        summarize(volume = sum(volume)) %>% 
        filter(volume > 0 )


# number of options in 5-day support
int_five_day_support <- df_five_day_support %>% nrow()



# creating the lagged volume report
# NOTE: I want to rewrite this later so that the ordering of the lags
#       is consistent with above, and so all the column names make sense.
# Notice that this already has early expiring options filtered out of the
# lags because we are starting with df_five_day_support which is inturn
# calculated from df_five_day_report, which already has all the early 
# expiring options removed
df_lag_report <- 
    df_five_day_support %>% 
        left_join(
            df_lag04 
            , by = c("underlying_symbol", "expiration", "type", "strike")
            , suffix = c("_nday", "_lag4")
        ) %>% 
        select(underlying_symbol:volume_nday, volume_lag4) %>% 
        left_join(
            df_lag03 
            , by = c("underlying_symbol", "expiration", "type", "strike")
        ) %>% 
        select(underlying_symbol:volume_lag4, volume_lag3 = volume) %>%
        left_join(
            df_lag02 
            , by = c("underlying_symbol", "expiration", "type", "strike")
        ) %>% 
        select(underlying_symbol:volume_lag3, volume_lag2 = volume) %>%
        left_join(
            df_lag01 
            , by = c("underlying_symbol", "expiration", "type", "strike")
        ) %>% 
        select(underlying_symbol:volume_lag2, volume_lag1 = volume) %>% 
        left_join(
            df_lag00 
            , by = c("underlying_symbol", "expiration", "type", "strike")
        ) %>% 
        select(underlying_symbol:volume_lag1, volume_lag0 = volume)



## calculation option level lag correlations
dbl_opt_lag_corr <- c(0, 0, 0, 0)
names(dbl_opt_lag_corr) <- c("lag01", "lag02", "lag03", "lag04")
# lag01
dbl_opt_lag_corr["lag01"] <- 
    cor(
        x = replace_na(df_lag_report$volume_lag0, 0)
        , y = replace_na(df_lag_report$volume_lag1, 0)
        , method = "spearman"
    )

# lag02
dbl_opt_lag_corr["lag02"] <- 
    cor(
        x = replace_na(df_lag_report$volume_lag0, 0)
        , y = replace_na(df_lag_report$volume_lag2, 0)
        , method = "spearman"
    )

# lag03
dbl_opt_lag_corr["lag03"] <-
    cor(
        x = replace_na(df_lag_report$volume_lag0, 0)
        , y = replace_na(df_lag_report$volume_lag3, 0)
        , method = "spearman"
    )

# lag04
dbl_opt_lag_corr["lag04"] <-
cor(
    x = replace_na(df_lag_report$volume_lag0, 0)
    , y = replace_na(df_lag_report$volume_lag4, 0)
    , method = "spearman"
)



######################################
## volume by underlying calculation ##
######################################
# calculating the volume by underlying
df_lag00_und <- 
    df_lag00 %>% 
        group_by(data_date, underlying_symbol) %>% 
        summarize(volume = sum(volume))
df_lag01_und <- 
    df_lag01 %>% 
    group_by(data_date, underlying_symbol) %>% 
    summarize(volume = sum(volume))
df_lag02_und <- 
    df_lag02 %>% 
    group_by(data_date, underlying_symbol) %>% 
    summarize(volume = sum(volume))
df_lag03_und <- 
    df_lag03 %>% 
    group_by(data_date, underlying_symbol) %>% 
    summarize(volume = sum(volume))
df_lag04_und <- 
    df_lag04 %>% 
    group_by(data_date, underlying_symbol) %>% 
    summarize(volume = sum(volume))


# total underlyings
int_und <- df_lag00_und %>% nrow()


# underlying support
int_und_support <-
    df_lag00_und %>% filter(volume > 0) %>% nrow()


# calculating lagged volume by underlying report
df_underlying_report <-
    df_lag00_und %>% 
        full_join(
            df_lag01_und
            , by = "underlying_symbol"
            , suffix = c("_lag0", "_lag1")
        ) %>% 
        select(data_date_lag0, underlying_symbol, volume_lag0
               , volume_lag1) %>% 
        full_join(
            df_lag02_und
            , by = "underlying_symbol"
        ) %>% 
        select(data_date_lag0, underlying_symbol, volume_lag0, 
               volume_lag1, volume_lag2 = volume) %>% 
        full_join(
            df_lag03_und
            , by = "underlying_symbol"
        ) %>% 
        select(data_date_lag0, underlying_symbol, volume_lag0, volume_lag1
               , volume_lag2, volume_lag3 = volume) %>% 
        full_join(
            df_lag04_und
            , by = "underlying_symbol"
        ) %>% 
        select(data_date_lag0, underlying_symbol, volume_lag0, volume_lag1
               , volume_lag2, volume_lag3, volume_lag4 = volume)



# calculating underlying volume-rank correlations
dbl_und_lag_corr <- c(0, 0, 0, 0)
names(dbl_und_lag_corr) <- c("lag01", "lag02", "lag03", "lag04")
# lag01
dbl_und_lag_corr["lag01"] <-
    cor(
        x = replace_na(df_underlying_report$volume_lag0, 0)
        , y = replace_na(df_underlying_report$volume_lag1, 0)
        , method = "spearman"
    )
# lag02
dbl_und_lag_corr["lag02"] <- 
    cor(
        x = replace_na(df_underlying_report$volume_lag0, 0)
        , y = replace_na(df_underlying_report$volume_lag2, 0)
        , method = "spearman"
    )
# lag03
dbl_und_lag_corr["lag03"] <- 
    cor(
        x = replace_na(df_underlying_report$volume_lag0, 0)
        , y = replace_na(df_underlying_report$volume_lag3, 0)
        , method = "spearman"
    )
# lag04
dbl_und_lag_corr["lag04"] <- 
    cor(
        x = replace_na(df_underlying_report$volume_lag0, 0)
        , y = replace_na(df_underlying_report$volume_lag4, 0)
        , method = "spearman"
    )





df_lag00_und %>% filter(volume > 0) %>% nrow() /
    df_lag00_und %>% nrow()
 
df_lag00_und %>% 
    filter(min_rank(volume)) %>% .$volume %>% sum() /   
    df_lag00_und %>% .$volume %>% sum()



df_lag00_und %>%
    mutate(vol_rank = min_rank(desc(volume))) %>% 
    arrange(vol_rank)


# concepts to consider

# 3) rank correlation between the days in an n-day support
# 4) within in n-day support, the share of volume the 1-5,6-30, 31-100 
# 5) on any given day, the set of all option that traded the previous 5 days
#    but that did not expire on any of the days


##############
## OLD CODE ##
##############
# checking the overlap between supports
# df_lag03 %>% filter(volume > 0) %>% 
#     inner_join(
#         df_lag02 %>% filter(volume > 0)
#         , by = c("underlying_symbol", "type", "strike", "expiration")
#     )


# # checking what percentage of options had some volume
# df_lag04 %>% filter(volume > 0) %>% nrow() /
#     df_lag04 %>% nrow()
# df_lag03 %>% filter(volume > 0) %>% nrow() /
#     df_lag03 %>% nrow()
# df_lag02 %>% filter(volume > 0) %>% nrow() /
#     df_lag02 %>% nrow()
# df_lag01 %>% filter(volume > 0) %>% nrow() /
#     df_lag01 %>% nrow()
# df_lag00 %>% filter(volume > 0) %>% nrow() /
#     df_lag00 %>% nrow()
#