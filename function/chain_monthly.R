chain_monthly <- function(underlying_in){
    # reading in the list of expiration from csv
    df_expiration <- 
        read_csv("data_input/monthly_expiration.csv", col_type = cols())
    
    
    # creating the dataframe that will capture the chain history.
    df_chain <-
        crossing(
            underlying = underlying_in
            , expiration = df_expiration$expiration
        ) %>% 
        left_join(
            df_expiration %>% select(expiration, execution)
            , by = "expiration"
        )
    
    # initializing a couple of columns
    df_chain$d2x = NA_integer_
    df_chain$num_opts = NA_integer_
    df_chain$exec_day_volume = NA_integer_
    
    df_chain
}


