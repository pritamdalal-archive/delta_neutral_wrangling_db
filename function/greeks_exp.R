greeks_exp <- function(df_opt_hist){
    # option payoff function
    payoff <- function(type, strike , underlying_price){
        if(type == "put"){return(max(strike - underlying_price, 0))}
        if(type == "call"){return(max(underlying_price -strike, 0))}
    }
    
    
    # calculating option payoffs    
    df_payoff_inputs <- df_opt_hist %>% select(type, strike, underlying_price)
    # setting the expiration bid, ask, and mid
    df_opt_hist$bid <- 
        pmap_dbl(df_payoff_inputs, payoff)
    df_opt_hist$ask <- 
        pmap_dbl(df_payoff_inputs, payoff)
    df_opt_hist$mid <- 
        pmap_dbl(df_payoff_inputs, payoff)

    
    # setting greek values
    df_opt_hist <- 
        df_opt_hist %>% 
            mutate(
                my_implied_vol = 0
                ,my_delta = delta
                , my_vega = 0
                , my_theta = 0
            )
        
    df_opt_hist
}