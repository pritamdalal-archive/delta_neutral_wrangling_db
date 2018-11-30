column_specification <- function(file_type){

col_spec <- cols()  

#------------------------------------------
# delta neutral data#######################
#------------------------------------------ 
# L3 Option Data
if (file_type=="delta_neutral_option"){
  col_spec <- cols(
    UnderlyingSymbol = col_character(),
    UnderlyingPrice = col_double(),
    Flags = col_character(),
    OptionSymbol = col_character(),
    Type = col_character(),
    Expiration = col_date(format="%m/%d/%Y"),
    DataDate = col_date(format="%m/%d/%Y"),
    Strike = col_double(),
    Last = col_double(),
    Bid = col_double(),
    Ask = col_double(),
    Volume = col_integer(),
    OpenInterest = col_integer(),
    T1OpenInterest = col_integer(),
    IVMean = col_double(),
    IVBid = col_double(),
    IVAsk = col_double(),
    Delta = col_double(),
    Gamma = col_double(),
    Theta = col_double(),
    Vega = col_double(),
    AKA = col_character()
  )
}  



## NOTE: For some reason this doesn't work when I use the correct col_date
#        format.  I'm not sure why, don't have time to look into it now
#        10/29/2018.
# L3 Stock Data
if (file_type=="delta_neutral_stock"){
    col_spec <- cols(
        symbol = col_character(),
        quotedate = col_character(), #col_date(format="%m/%d/%Y"),
        open = col_double(),
        high = col_double(),
        low = col_double(),
        close = col_double(),
        volume = col_integer(),
        adjustedclose = col_double()
    )
}  


#----------#
# opt_hist #
#----------#
if (file_type=="opt_hist"){
  col_spec <- cols(
    underlying_symbol = col_character(),
    underlying_price = col_double(),
    flags = col_character(),
    option_symbol = col_character(),
    type = col_character(),
    expiration = col_date(format="%Y-%m-%d"),
    data_date = col_date(format="%Y-%m-%d"),
    strike  = col_double(),
    last = col_double(),
    bid = col_double(),
    ask = col_double(),
    mid = col_double(),
    volume = col_integer(),
    open_interest = col_integer(),
    t1_open_interest = col_integer(),
    iv_mean = col_double(),
    iv_bid = col_double(), 
    iv_ask = col_double(),
    delta = col_double(),
    gamma = col_double(),
    theta = col_double(),
    vega = col_double(),
    aka = col_character(),
    my_implied_vol = col_double(),
    my_delta  = col_double(),
    my_vega = col_double(),
    my_theta = col_double()
  )
}

#---------------------------------------------
# option history CSV #########################
#---------------------------------------------
if (file_type=="option_history"){
  column_spec_option_history <- cols(
    trade_date  = col_date(format="%Y-%m-%d"),
    option_symbol = col_character(),
    underlying = col_character(),
    type = col_character(),
    expiration = col_date(format="%Y-%m-%d"),
    strike  = col_double(),
    upx = col_character(),
    bid = col_double(),
    ask = col_double(),
    implied_vol = col_double(),
    delta = col_double(),
    vega = col_double(),
    theta = col_double(),
    volume = col_integer(),
    open_interest = col_integer()
  )   
}

col_spec
}


