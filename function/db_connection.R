# the purpose of this function is simply to return a connection
# to the delta_neutral database
db_connection <- function(){
    db_conn <- 
        RMariaDB::dbConnect(
            drv = RMariaDB::MariaDB()
            ,user = 'root'
            , password = 'w0rthless1'
            , dbname = 'delta_neutral'
            , host='localhost'
        )
    db_conn
}