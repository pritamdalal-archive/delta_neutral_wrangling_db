test <- option_chain(db_connection())


option_chain <- function(db_conn){
                         #, trade_date, underlying, expiration){
    
    chr_query <- 
        "select * 
         from option_small 
         where DataDate='2018-03-19'
         and UnderlyingSymbol='SPY'
         and Expiration='2018-04-20'
         ;
         "
    
    
    rs <- RMariaDB::dbSendQuery(db_conn, chr_query)
    data <- RMariaDB::dbFetch(rs, -1)
    RMariaDB::dbHasCompleted(rs)
    df_data <- tibble::as_tibble(data)
    RMariaDB::dbClearResult(rs)
    RMariaDB::dbDisconnect(db_conn)
    df_data
}