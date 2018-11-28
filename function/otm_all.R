otm_all <- function(df_full_chain, implied_forward){
  
  # puts
  df_otm_all_put <- 
    df_full_chain %>%  
    dplyr::filter(type == "put") %>% 
    dplyr::filter(strike <= implied_forward)
  
  # calls
  df_otm_all_call <- 
      df_full_chain %>%  
      dplyr::filter(type == "call") %>% 
      dplyr::filter(strike > implied_forward)
  
  # combining into one dataframe
  df_otm_all <-
      bind_rows(df_otm_all_put, df_otm_all_call)
  
  df_otm_all
}



