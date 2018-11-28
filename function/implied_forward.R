implied_forward <- function(df_full_chain){
    
    df_forward_calc_data <- 
        df_full_chain %>% 
        select(type, strike, mid)
    
    # separating calls
    df_forward_calc_calls <- 
        df_forward_calc_data %>%
        select(type, strike, mid) %>% 
        dplyr::filter(type == "call") %>% 
        rename(call_price = mid)
    
    # separating puts
    df_forward_calc_puts <- 
        df_forward_calc_data %>%
        select(type, strike, mid) %>% 
        dplyr::filter(type == "put") %>%
        rename(put_price = mid)
    
    # calculating implied forward at each strike price
    df_forward_calc <- 
        df_forward_calc_puts %>% 
        inner_join(df_forward_calc_calls, by = "strike") %>% 
        mutate(price_diff = call_price - put_price) %>% 
        mutate(implied_forward = strike + call_price - put_price) %>% 
        mutate(abs_price_diff = abs(price_diff)) %>% 
        arrange(abs_price_diff) %>% 
        top_n(-5,abs_price_diff)
    
    # may want to put a check here to see if df_forward_calc 
    # has no elements in it 
    # (this is what was happening to TTT before I was allowing 
    # for options with zero open interest)
    
    dbl_implied_forward <- mean(df_forward_calc$implied_forward)  
    
    dbl_implied_forward

}