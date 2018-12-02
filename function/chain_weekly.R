chain_weekly <- function(underlying_in){
    # creating the dataframe that will capture the chain history.
    # start with every sunday and friday (probably can just start with Friday)
    df_chain <-
        crossing(
            underlying = underlying_in
            ,friday = seq(ymd(20161216), ymd(20180720), "weeks")
        ) %>% 
        mutate(
            # check if Friday is a business day
            fri_biz_day = is.bizday(friday) 
            # initializing expiration column
            , expiration = seq(ymd(20161216), ymd(20180720), "weeks") 
        )
    
    
    # if Friday is not a business day, make expiration the Thursday prior
    for (ix_exp in 1:nrow(df_chain)){
        if(df_chain$fri_biz_day[ix_exp]){
            df_chain$expiration[ix_exp] <- 
                df_chain$friday[ix_exp]
        } else {
            df_chain$expiration[ix_exp] <- 
                add.bizdays(df_chain$friday[ix_exp], -1)
        }
    } 
    
    # excution for a given expiration is the previous weekly expiration
    df_chain$execution <- df_chain$expiration %>% dplyr::lag(1) 
    
    # removing the first row
    df_chain <- df_chain[-1,]
    
    # removing unneeded columns
    df_chain <- df_chain %>% select(-c(friday:fri_biz_day))
    
    # initializing a couple of columns
    df_chain$d2x = NA_integer_
    df_chain$num_opts = NA_integer_
    df_chain$exec_day_volume = NA_integer_
    
    df_chain
}


