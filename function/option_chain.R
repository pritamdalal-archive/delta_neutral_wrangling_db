option_chain <- function(db_conn
                         , trade_date
                         , underlying
                         , expiration
                         , exclude_zero_bid = FALSE){
    
    chr_query <- 
        paste0("select * from option_small where DataDate='", trade_date 
               ,"' and UnderlyingSymbol='",  underlying
               ,"' and Expiration='", expiration, "';")
    
    rs <- RMariaDB::dbSendQuery(db_conn, chr_query)
    data <- RMariaDB::dbFetch(rs, -1)
    RMariaDB::dbHasCompleted(rs)
    df_data <- tibble::as_tibble(data)
    RMariaDB::dbClearResult(rs)
    RMariaDB::dbDisconnect(db_conn)
    
    df_data <-
        df_data %>% 
        select(
            underlying_symbol = UnderlyingSymbol
            , underlying_price = UnderlyingPrice
            , flags = Flags
            , option_symbol = OptionSymbol
            , type = Type
            , expiration = Expiration
            , data_date = DataDate
            , strike = Strike
            , last = Last
            , bid = Bid
            , ask = Ask
            , volume = Volume
            , open_interest = OpenInterest
            , t1_open_interest = T1OpenInterest
            , iv_mean = IVMean
            , iv_bid = IVBid
            , iv_ask = IVAsk
            , delta = Delta
            , gamma = Gamma
            , theta = Theta
            , vega = Vega
            , aka = AKA
        ) %>% 
        mutate(
            mid = (bid + ask)/2
            , delta = abs(delta)
        )
    
    if (exclude_zero_bid){
        df_data <- 
            df_data %>% dplyr::filter(bid > 0)
    }
    
    df_data <- 
        df_data %>% select(underlying_symbol:ask, mid, volume:aka)
    
    df_data
}