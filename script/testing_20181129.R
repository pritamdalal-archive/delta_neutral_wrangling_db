##################
# weekly testing #
##################
df_chain_old <- 
    read_csv("data_output/spy_weekly_chain.csv", col_types = cols())

df_chain_hist_old <- 
    read_csv("data_output/spy_weekly_chain_hist.csv", col_types = cols())

df_opt_hist_old <- 
    read_csv("data_output/spy_weekly_opt_hist.csv", col_types = cols())


# chain hist
df_chain_hist$implied_forward %>% sum() ==
    df_chain_hist_old$implied_forward %>% sum()

df_chain_hist$bid_swap_rate %>% sum() ==
    df_chain_hist_old$bid_swap_rate %>% sum()

df_chain_hist$ask_swap_rate %>% sum() ==
    df_chain_hist_old$ask_swap_rate %>% sum()


# opt hist
df_opt_hist$my_implied_vol %>% sum() ==
    df_opt_hist_old$my_implied_vol %>% sum()


df_opt_hist$my_delta %>% sum(na.rm = TRUE) ==
    df_opt_hist_old$my_delta %>% sum(na.rm = TRUE)



##################
# monthly testing #
##################
df_market_hist_old <- 
    read_csv(
        "~/files/R/fm5990_group_project/data/project_market_history.csv"
        , col_types = cols()
    )

df_exp <- read_csv("data_input/monthly_expiration.csv", col_types = cols())

df_market_hist_old %>%
    #dplyr::filter(underlying %in% c("SPY", "QQQ")) %>% 
    inner_join(
        df_exp %>% select(expiration, execution)
        , by =  "expiration"
    ) %>% 
    dplyr::filter(trade_date == execution) %>% 
    .$my_implied_vol %>% 
    summary()


df_opt_hist %>% 
    inner_join(
        df_exp %>% select(expiration, execution)
        , by =  "expiration"
    ) %>% 
    dplyr::filter(data_date == execution) %>% 
    .$my_implied_vol %>% 
    summary()



