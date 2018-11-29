df_mkt_hist %>% 
    dplyr::filter(expiration == data_date) %>% View()




df_mkt_hist %>% dplyr::filter(is.na(option_symbol)) %>% View()
