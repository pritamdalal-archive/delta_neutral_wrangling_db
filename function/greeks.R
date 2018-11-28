greeks <- function(df_otm, d2x, impl_fwd){
    
  ########################    
  ## implied volatility ##
  ########################
  # creating the tibble containing the inputs for calculating 
  # implied volatility with fOption::GBSVolatility
  df_implied_vol_inputs <- 
        tibble(
          price = df_otm$mid,
          TypeFlag = str_sub(df_otm$type,1,1),
          S = impl_fwd,
          X = df_otm$strike,
          Time = d2x/252,
          r = 0,
          b = 0,
          tol = 0.0001,
          maxiter = 1000  
      )
  
  # calculating implied volatilities iteratively using purrr:mpap_dbl
  # appending them onto df_otm using dplyr::mutate
  df_otm_with_greeks <- 
    df_otm %>%  
    mutate(
        my_implied_vol = df_implied_vol_inputs %>% pmap_dbl(GBSVolatility)
    )
  
  ###########
  ## delta ##
  ###########
  # creating the tibble containing the inputs for calculating 
  # delta with fOption::GBSGreeks
  df_delta_inputs <- 
      tibble(
        Selection = "delta",
        TypeFlag = str_sub(df_otm$type,1,1),
        S = impl_fwd,
        X = df_otm$strike,
        Time = d2x/252,
        r = 0,
        b = 0,
        sigma = df_otm_with_greeks$my_implied_vol
      )
  
  # calculating delta iteratively using purrr:mpap_dbl
  # appending them onto df_otm using dplyr::mutate
  df_otm_with_greeks <- 
      df_otm_with_greeks %>%  
      mutate(
          my_delta = df_delta_inputs %>% pmap_dbl(GBSGreeks)
      )
  
  # changing delta to abs(delta)
  df_otm_with_greeks <- 
      df_otm_with_greeks %>% 
      mutate(my_delta = abs(my_delta))
  
  
  ##########
  ## vega ##
  ##########
  # creating the tibble containing the inputs for calculating 
  # delta with fOption::GBSGreeks
  df_vega_inputs <- 
      tibble(
          Selection = "vega",
          TypeFlag = str_sub(df_otm$type,1,1),
          S = impl_fwd,
          X = df_otm$strike,
          Time = d2x/252,
          r = 0,
          b = 0,
          sigma = df_otm_with_greeks$my_implied_vol
      )
  
  
  # calculating vega iteratively using purrr:mpap_dbl
  # appending them onto df_otm using dplyr::mutate
  df_otm_with_greeks <- 
      df_otm_with_greeks %>%  
      mutate(
          my_vega = df_vega_inputs %>% pmap_dbl(GBSGreeks)
      )
  
  # changing to 1% delta
  df_otm_with_greeks <- 
      df_otm_with_greeks %>% 
      mutate(my_vega = my_vega * 0.01)
  
  
  ###########
  ## theta ##
  ###########
  # creating the tibble containing the inputs for calculating 
  # delta with fOption::GBSGreeks
  df_theta_inputs <- 
      tibble(
          Selection = "theta",
          TypeFlag = str_sub(df_otm$type,1,1),
          S = impl_fwd,
          X = df_otm$strike,
          Time = d2x/252,
          r = 0,
          b = 0,
          sigma = df_otm_with_greeks$my_implied_vol
      )
  
  # calculating theta iteratively using purrr:mpap_dbl
  # appending them onto df_otm using dplyr::mutate
  df_otm_with_greeks <- 
      df_otm_with_greeks %>%  
      mutate(
          my_theta = df_theta_inputs %>% pmap_dbl(GBSGreeks)
      )
  
  # changing to one trade-day delta
  df_otm_with_greeks <- 
      df_otm_with_greeks %>% 
      mutate(my_theta = my_theta / 252)
  
  
  # ####################
  # ## ATM Volatility ##
  # ####################
  # # ATM Vol is the interpolated value of the ATM put volatility and 
  # # the ATM call volatility
  # df_atm_put <- df_otm_with_greeks %>%
  #   dplyr::filter(strike <= impl_fwd & type=="put") %>% 
  #   arrange(strike) %>% 
  #   top_n(1, strike)
  # 
  # df_atm_call <- df_otm_with_greeks %>%
  #   dplyr::filter(strike >= impl_fwd & type=="call") %>% 
  #   arrange(strike) %>% 
  #   top_n(-1, strike)
  # 
  # dbl_atm_vol <- 
  #   (
  #   # put contribution
  #   ((df_atm_call$strike[1] - df_atm_call$impl_fwd[1]) * 
  #   df_atm_put$my_implied_vol[1]) +
  #   # call contribution
  #   ((df_atm_put$impl_fwd[1] - df_atm_put$strike[1]) * 
  #   df_atm_call$my_implied_vol[1])
  #   ) /
  #   # normalize by distance between the two strikes
  #   (df_atm_call$strike[1] - df_atm_put$strike[1])
  # 
  # df_otm_with_greeks <- 
  #   df_otm_with_greeks %>% 
  #   mutate(atm_volatility = dbl_atm_vol)              
  
  df_otm_with_greeks
}


