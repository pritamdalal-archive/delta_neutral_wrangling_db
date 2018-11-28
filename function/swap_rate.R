swap_rate <- function(df_otm, d2x){
  
  #question/challenge/exercise - can you create a functional to this so
  #that you don't have to repeat the numerical integration code
  dbl_time_to_expiration <- d2x/252
  
  dbl_strike_intervals <- 
      df_otm$strike[2:nrow(df_otm)] -
      df_otm$strike[1:nrow(df_otm)-1]
  
  # using bid prices
  left_points_bid <- 
      df_otm$bid[1:nrow(df_otm)-1] * 
      (df_otm$strike[1:nrow(df_otm)-1])^-2
  
  left_sum_bid <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * left_points_bid)
  
  right_points_bid <- 
      df_otm$bid[2:nrow(df_otm)] * 
      (df_otm$strike[2:nrow(df_otm)])^-2
  
  right_sum_bid <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * right_points_bid)
  
  dbl_swap_rate_bid <- mean(c(left_sum_bid,right_sum_bid))
  dbl_swap_rate_bid <- sqrt(dbl_swap_rate_bid)
  
  # using ask prices
  left_points_ask <- 
      df_otm$ask[1:nrow(df_otm)-1] * 
      (df_otm$strike[1:nrow(df_otm)-1])^-2
  
  left_sum_ask <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * left_points_ask)
  
  right_points_ask <- 
      df_otm$ask[2:nrow(df_otm)] * 
      (df_otm$strike[2:nrow(df_otm)])^-2
  
  right_sum_ask <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * right_points_ask)
  
  dbl_swap_rate_ask <- mean(c(left_sum_ask,right_sum_ask))
  dbl_swap_rate_ask <- sqrt(dbl_swap_rate_ask)
  
  #using mid prices
  left_points_mid <- 
      df_otm$mid[1:nrow(df_otm)-1] * 
      (df_otm$strike[1:nrow(df_otm)-1])^-2
  
  left_sum_mid <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * left_points_mid)
  
  right_points_mid <- 
      df_otm$mid[2:nrow(df_otm)] * 
      (df_otm$strike[2:nrow(df_otm)])^-2
  
  right_sum_mid <- 
      (2/dbl_time_to_expiration) * 
      sum(dbl_strike_intervals * right_points_mid)
  
  dbl_swap_rate_mid <- mean(c(left_sum_mid,right_sum_mid))
  dbl_swap_rate_mid <- sqrt(dbl_swap_rate_mid)
  
  
  # adding swap rates to option-prices tibble
  # bid
  # df_otm_with_swap_rates <- 
  #     df_otm %>% 
  #     mutate(bid_swap_rate = dbl_swap_rate_bid)
  # # mid
  # df_otm_with_swap_rates <-
  #     df_otm_with_swap_rates %>%  
  #     mutate(ask_swap_rate = dbl_swap_rate_ask)
  # # ask
  # df_otm_with_swap_rates <- 
  #   df_otm_with_swap_rates %>% 
  #     mutate(mid_swap_rate = dbl_swap_rate_mid)
  
  dbl_swap_rate = c(dbl_swap_rate_bid, dbl_swap_rate_ask, dbl_swap_rate_mid)
  
}


