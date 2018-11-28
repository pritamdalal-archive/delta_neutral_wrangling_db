otm_clean <- function(df_otm_all){
  #This function removes all the low information options.
  #I define low information options as ones who's bid or ask 
  #is the same as an option who's value *should* be higher:
  #for puts, as higher strike put, for calls a lower strike call.
  ############
  ### PUTS ###
  ############
  #separating out puts
  df_otm_puts_all <- 
      df_otm_all %>% 
      dplyr::filter(type=="put")
  df_otm_puts_all <- arrange(df_otm_puts_all,desc(strike))
  
  
  if (nrow(df_otm_puts_all) >= 2)
  {
    
    bln_no_information <- FALSE
    ix_strike <- 2
    
    while(
          bln_no_information == FALSE 
          & ix_strike <= length(df_otm_puts_all$strike)
         )
    {
      dbl_lowest_good_put_strike <- df_otm_puts_all$strike[ix_strike]
      
      #check if bids are equal
      if(df_otm_puts_all$bid[ix_strike] >= df_otm_puts_all$bid[ix_strike - 1])
      {
        bln_no_information<-TRUE
        dbl_lowest_good_put_strike <- df_otm_puts_all$strike[ix_strike - 1]
      }
      
      #check if asks are equal
      if(df_otm_puts_all$ask[ix_strike] >= df_otm_puts_all$ask[ix_strike - 1])
      {
        bln_no_information<-TRUE
        dbl_lowest_good_put_strike <- df_otm_puts_all$strike[ix_strike - 1]
      }
      
      ix_strike=ix_strike+1
    }
    
    if (dbl_lowest_good_put_strike > min(df_otm_puts_all$strike))
    {
      df_otm_puts <- 
         df_otm_puts_all %>% 
         dplyr::filter(strike >= dbl_lowest_good_put_strike)
    } else
    {
      df_otm_puts <- df_otm_puts_all
    }  
  }
  else
  {
    df_otm_puts = df_otm_puts_all
  }
  
  #############
  ### CALLS ###
  #############
  #separating out calls
  df_otm_calls_all <- 
      df_otm_all %>% 
      dplyr::filter(type == "call")
  df_otm_calls_all <- arrange(df_otm_calls_all, strike)
  
  if (nrow(df_otm_calls_all) >= 2)
  {
    bln_no_information<-FALSE
    ix_strike<-2
    while(
          bln_no_information == FALSE 
          & ix_strike <= length(df_otm_calls_all$strike)
         )
    {
      dbl_highest_good_call_strike <- df_otm_calls_all$strike[ix_strike]
      
      #check if bids are greater than or equal
      if(df_otm_calls_all$bid[ix_strike] >= df_otm_calls_all$bid[ix_strike - 1])
      {
        bln_no_information<-TRUE
        dbl_highest_good_call_strike <- df_otm_calls_all$strike[ix_strike - 1]
      }
      #check if asks are greater than or equal
      if(df_otm_calls_all$ask[ix_strike] >= df_otm_calls_all$ask[ix_strike - 1])
      {
        bln_no_information<-TRUE
        dbl_highest_good_call_strike <- df_otm_calls_all$strike[ix_strike-1]
      }
      
      ix_strike=ix_strike+1
    }
    
    if (dbl_highest_good_call_strike < max(df_otm_calls_all$strike))
    {
      df_otm_calls <- 
        df_otm_calls_all %>% 
        dplyr::filter(strike <= dbl_highest_good_call_strike)
    } else
    {
      df_otm_calls <- df_otm_calls_all
    }  
  }
  else
  {
    df_otm_calls <- df_otm_calls_all
  }
  
  #rearranging rows
  df_otm <- bind_rows(df_otm_puts, df_otm_calls)
  df_otm <- arrange(df_otm, desc(type),strike)
  
  df_otm
}

