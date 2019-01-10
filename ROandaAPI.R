
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubROandaAPI ---------------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- ------------------------------------------------------------------------------ -- #
# -- List of available instruments ------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

InstrumentsList <- function(AccountType, Token, AccountID, ApiVersion) {

if(missing(ApiVersion) || ApiVersion == "v1"){

  ApiVersion <- "v1"
  
  # -- ------------------------------------------------------------------ Version 1.0 -- #
  # -- --------------------------------------------------------------------------------- #

  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/instruments?accountId=",sep="")
  QueryInst  <- paste(Queryhttp,AccountID,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson   <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  FinalData  <- data.frame(InstJson)
  colnames(FinalData) <- c("Instrument","DisplayName","PipSize","MaxTradeUnits")
  FinalData$MaxTradeUnits <- as.numeric(FinalData$MaxTradeUnits)

return(FinalData)

  # -- ---------------------------------------------------------------- Version 2.0 -- #
  # -- ------------------------------------------------------------------------------- #

} else if (ApiVersion == "v2"){
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else if (AccountType == "live"){
    httpaccount <- "https://api-fxtrade.oanda.com"
  } else {
    print("Account type error. Must be practice or live")
  }
    
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v3/accounts/",sep="")
  QueryInst  <- paste(Queryhttp,AccountID,"/","instruments", sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                                                    package="RCurl"),httpheader=auth)
  InstJson   <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  FinalData  <- data.frame(InstJson)

  return(FinalData)
}
  
  # -- ------------------------------------------------------------------ Test Code -- #
  # -- ------------------------------------------------------------------------------- #
  
  # p1_AccountType <- "practice"
  # p2_Token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" 
  # p3_AccountID   <- "XXX-XXX-XXXXXXX-XXX"
  # p4_ApiVersion  <- "v2"
  # oa_instruments <- InstrumentsList(AccountType = p1_AccountType,
  #                                   Token = p2_Token,
  #                                   AccountID = p3_AccountID,
  #                                   ApiVersion = p4_ApiVersion) 
  
}

# -- ------------------------------------------------------------------------------ -- #
# -- Actual Price Request --------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

ActualPrice <- function(AccountType,Token,Instrument)
{
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/prices?instruments=",sep="")
  QueryPrec <- paste(Queryhttp,Instrument,sep="")
  InstPrec  <- getURL(QueryPrec,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  DateTime  <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),
                  format = "%H:%M:%S")
  Date <- as.character(substr(DateTime,1,10))
  Time <- as.character(substr(DateTime,12,19))
  DataJSON  <- data.frame(paste(Date,Time,sep=" "),InstPrecjson[[1]]$bid,
                            InstPrecjson[[1]]$ask)
  colnames(DataJSON) <- c("TimeStamp","Bid","Ask")
  DataJSON$TimeStamp <- as.POSIXct(DataJSON$TimeStamp,origin="1970-01-01")

return(DataJSON)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Historical Spreads Request --------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

HisSpreads <- function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,
                       Start,End,Count)  {

  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount  <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  if(!is.null(Count)) {
    
    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qcount  <- paste("count=",Count,sep="")
    
    qcandleFormat <- "candleFormat=bidask"
    qgranularity  <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qcandleFormat,qgranularity,
                            qdailyalignment,qcount,sep="&")

  }

  else {

    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qstart <- paste("start=",Start,sep="")
    qend   <- paste("end=",End,sep="")
    
    qcandleFormat  <- "candleFormat=bidask"
    qgranularity   <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
                            qdailyalignment,sep="&")
  }

  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  
  colnames(Prices) <- c("TimeStamp", "Open_Bid", "Open_Ask", "High_Bid", "High_Ask",
                        "Low_Bid", "Low_Ask", "Close_Bid", "Close_Ask", "Volume", "Complete")
  Prices$TimeStamp <- as.POSIXct(strptime(Prices$TimeStamp, "%Y-%m-%d %H:%M:%OS"),
                                 origin="1970-01-01",tz = "UTC")
  attributes(Prices$TimeStamp)$tzone <- TimeAlign
  return(Prices)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Historical Prices Request ---------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

HisPrices  <- function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,
                       Start,End,Count){
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount  <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  if(!is.null(Count)) {
    
    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qcount  <- paste("count=",Count,sep="")
    
    qcandleFormat <- "candleFormat=midpoint"
    qgranularity  <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qcandleFormat,qgranularity,
                            qdailyalignment,qcount,sep="&")
    
  }
  
  else {
    
    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qstart <- paste("start=",Start,sep="")
    qend   <- paste("end=",End,sep="")
    
    qcandleFormat  <- "candleFormat=midpoint"
    qgranularity   <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
                            qdailyalignment,sep="&")
  }
  
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","TickVolume","Complete")
  Prices$TimeStamp <- as.POSIXct(strptime(Prices$TimeStamp, "%Y-%m-%d %H:%M:%OS"),
                                 origin="1970-01-01",tz = "UTC")
  attributes(Prices$TimeStamp)$tzone <- TimeAlign
  return(Prices)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Accounts per given username  ------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

UsersAccounts <- function(AccountType,Token,UserName)
{
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts?username=",sep="")
  QueryInst  <- paste(Queryhttp,UserName,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Account Information  --------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

AccountInfo   <- function(AccountType,AccountID,Token) {
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  QueryInfo <- paste(Queryhttp,AccountID,sep="")
  CtaInfo   <- getURL(QueryInfo,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"),httpheader=auth)
  CtaInfoJson <- fromJSON(CtaInfo, simplifyDataFrame = TRUE)
  
  CtaNombre <- CtaInfoJson$accountName     # Nombre de la cta que se esta consultando
  CtaBalanc <- CtaInfoJson$balance         # Balance de la cta
  Ctaunreal <- CtaInfoJson$unrealizedPl    # Ganancia/Perdida sin tomar
  Ctareal   <- CtaInfoJson$realizedPl      # Ganancia/Perdida ya tomada
  Ctamgenut <- CtaInfoJson$marginUsed      # Margen utilizado
  Ctamgendi <- CtaInfoJson$marginAvail     # Margen disponible
  Ctamgenrt <- CtaInfoJson$marginRate      # Tasa de margen utilizada
  CtaOperac <- CtaInfoJson$openTrades      # Operaciones abiertas
  CtaOrdens <- CtaInfoJson$openOrders      # Ordenes abiertas
  datos     <- data.frame(CtaNombre,CtaBalanc,Ctaunreal,Ctareal,Ctamgenut,
               Ctamgendi,Ctamgenrt,CtaOperac,CtaOrdens)

return(datos)
}

# -- ----------------------------------------------------------------------------------------- -- #
# -- Actual orders in the account ------------------------------------------------------------ -- #
# -- ----------------------------------------------------------------------------------------- -- #

AccountOrders  <- function(AccountType,AccountID,Token,Instrument,ApiVersion) {
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else if (AccountType == "live"){
    httpaccount <- "https://api-fxtrade.oanda.com"
  } else {
    print("Account type error. Must be practice or live")
  }
  
  if(missing(ApiVersion) || ApiVersion == "v1"){
    
    ApiVersion <- "v1"
    
  # -- --------------------------------------------------------------------------- Version 1.0 -- #
  # -- ------------------------------------------------------------------------------------------ #
  
    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/orders?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=2",sep="")
    QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)

  # -- --------------------------------------------------------------------------- Version 2.0 -- #
  # -- ------------------------------------------------------------------------------------------ #
  
  } else if (ApiVersion == "v2") {

    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v3/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/orders?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=2",sep="")
    QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
  }
  
  # -- ----------------------------------------------------------------------------- Test Code -- #
  # -- ------------------------------------------------------------------------------------------ #
  
  # p1_AccountType <- "practice"
  # p2_Token <- OA_Ak
  # p3_Instrument  <- "EUR_USD"
  # p4_AccountID   <- "101-004-2221697-001"
  # p5_ApiVersion  <- "v1"
  # oa_orders <- AccountOrders(AccountType = p1_AccountType,
  #                            Token = p2_Token,
  #                            Instrument = p3_Instrument,
  #                            AccountID = p4_AccountID,
  #                            ApiVersion = p5_ApiVersion)

}

# -- ----------------------------------------------------------------------------------------- -- #
# -- Place a new order ----------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

NewOrder <- function(AccountType,AccountID,Token,OrderType,Instrument,Count,Side,Expiry,
                     Price, SL, TP, TS, ApiVersion) {
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else if (AccountType == "live"){
    httpaccount <- "https://api-fxtrade.oanda.com"
  } else {
    print("Account type error. Must be practice or live")
  }
  
  if(missing(ApiVersion) || ApiVersion == "v1"){
    
    ApiVersion <- "v1"
    
  # -- --------------------------------------------------------------------------- Version 1.0 -- #
  # -- ------------------------------------------------------------------------------------------ #
  
  auth       <- c(Authorization = paste("Authorization: Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts/",sep="")
  Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
  Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
  
  if(OrderType == 'market'){
    Param <- c(instrument=Instrument, units=Count, side=Side, type=OrderType,
               stopLoss=SL, takeProfit=TP, trailingStop=TS)
  } else  if(OrderType == 'limit'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, expiry=Expiry, stopLoss=SL, takeProfit=TP, trailingStop=TS)
  } else  if(OrderType == 'stop'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, stopLoss=SL, takeProfit=TP, expiry=Expiry, trailingStop=TS)
  } else  if(OrderType == 'marketIfTouched'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, stopLoss=SL, takeProfit=TP, expiry=Expiry, trailingStop=TS)
  } else print("Order Type error. Must be: 'market', 'limit', 'stop', 'marketIfTouched'")

  PF <- postForm(Queryhttp2, style="POST", .params=Param,
          .opts=list(httpheader=auth,ssl.verifypeer = FALSE))
  InstJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(InstJson)
  
  } else {
    
    auth       <- paste0("Bearer ",Token)
    Queryhttp  <- paste(httpaccount,"/v3/accounts/",sep="")
    Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
    Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
    
    Count        <- 5
    Instrument   <- "EUR_USD"
    TimeInForce  <- "FOK"
    PositionFill <- "DEFAULT"
    OrderType    <- "MARKET"
    
    tradeID   <- 43
    OrderType <- "TAKE_PROFIT"
    Price <- 1.7000
    TimeInForce <- "GTC"
    
    if(OrderType == 'TAKE_PROFIT'){
      
      parse1 <- '{"order":{'
      parse2 <- paste0(parse1, paste0('"tradeID":'     , '"', tradeID,        '"', ','))
      parse3 <- paste0(parse2, paste0('"timeInForce":' , '"', TimeInForce,  '"', ','))
      parse4 <- paste0(parse3, paste0('"type":'        , '"', OrderType,    '"', ','))
      parse5 <- paste0(parse4, paste0('"price":'       , '"', Price,'"'))
      parse_body <- paste0(parse5, '}}')

    } else  if(OrderType == 'MARKET'){
      
      parse1 <- '{"order":{'
      parse2 <- paste0(parse1, paste0('"units":'       , '"', Count,        '"', ','))
      parse3 <- paste0(parse2, paste0('"instrument":'  , '"', Instrument,   '"', ','))
      parse4 <- paste0(parse3, paste0('"timeInForce":' , '"', TimeInForce,  '"', ','))
      parse5 <- paste0(parse4, paste0('"type":'        , '"', OrderType,    '"', ','))
      parse6 <- paste0(parse5, paste0('"positionFill":', '"', PositionFill, '"'))
      parse_body <- paste0(parse6, '}}')
      
    } else  if(OrderType == 'STOP_LOSS'){
    } else  if(OrderType == 'LIMIT'){
    } else  if(OrderType == 'TRAILING_STOP_LOSS'){
    } else  if(OrderType == 'FIXED_PRICE'){
    } else  if(OrderType == 'STOP'){
    } else  if(OrderType == 'MARKET_IF_TOUCHED'){
    } else print("Order Type error. Must be: 'FIXED_PRICE', 'LIMIT', 'STOP', 'MARKET_IF_TOUCHED',
                 'TAKE_PROFIT', 'STOP_LOSS', 'TRAILING_STOP_LOSS'")
    
    InstJson <- content(POST(url = Queryhttp2, encode = "raw", body = parse_body,
                             add_headers("Authorization" = auth,
                                         "Content-Type" = "application/json")), "parsed")
    
    return(InstJson)

  }

}

# -- ------------------------------------------------------------------------------ -- #
# -- Close an order --------------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

CloseOrder <- function(AccountType, AccountID, Token, OrderID)
{
  
  if(AccountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  Queryhttp  <- paste(httpaccount,"/v1/accounts/", sep = "")
  Queryhttp1 <- paste(Queryhttp,AccountID, sep = "")
  Queryhttp2 <- paste(Queryhttp1,"/orders/", sep = "")
  Queryhttp3 <- paste(Queryhttp2,OrderID, sep = "")
  
  auth  <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  
  DELETEOrder <- httpDELETE(Queryhttp3, cainfo=system.file("CurlSSL","cacert.pem",
  package="RCurl"), httpheader=auth)
  
return(DELETEOrder)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Modify parameters of an order ------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

ModifyOrder <- function(AccountType, AccountID, Token, OrderID, Units, Price,
                        Expiry, StopLoss, TakeProfit, TrailingStop)
{
  
  if(AccountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  Queryhttp  <- paste(httpaccount,"/v1/accounts/", sep = "")
  Queryhttp1 <- paste(Queryhttp,AccountID, sep = "")
  Queryhttp2 <- paste(Queryhttp1,"/orders/", sep = "")
  Queryhttp3 <- paste(Queryhttp2,OrderID, sep = "")
  
  auth  <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  Param <- c(units=Units, price=Price, expiry=Expiry, stopLoss=StopLoss, 
             takeProfit=TakeProfit, trailingStop=TrailingStop)
             
  PatchModifyOrder <- httpOPTIONS(Queryhttp3, cainfo=system.file("CurlSSL","cacert.pem",
                                      package="RCurl"), httpheader=auth,.params=Param)

return(PatchModifyOrder)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Order Book ------------------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

OrderBook <- function(AccountType,Token,Instrument,Period)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ") 
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/orderbook_data?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")  
  
  orderbook  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
  "cacert.pem",package="RCurl"),httpheader=auth)
  orderbook  <- fromJSON(orderbook)

return(orderbook)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Information about a particular order ----------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

OrderInfo  <- function(AccountType,AccountID,Token,OrderNum)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/orders/",sep="")
  Querythttp3 <- paste(Querythttp2,OrderNum,sep="")
  QueryInst1  <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ----------------------------------------------------------------------------------------- -- #
# -- List of open trades --------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

OpenTrades  <- function(AccountType,AccountID,Token,Instrument,ApiVersion) {
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else if (AccountType == "live"){
    httpaccount <- "https://api-fxtrade.oanda.com"
  } else {
    print("Account type error. Must be practice or live")
  }
  
  if(missing(ApiVersion) || ApiVersion == "v1"){
    
    ApiVersion <- "v1"
    
  # -- --------------------------------------------------------------------------- Version 1.0 -- #
  # -- ------------------------------------------------------------------------------------------ #
  
    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/trades?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=100",sep="")
    QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
  
  # -- --------------------------------------------------------------------------- Version 2.0 -- #
  # -- ------------------------------------------------------------------------------------------ #
  
  } else if (ApiVersion == "v2") {

    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v3/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/trades?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=100",sep="")
    QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  
return(InstJson)
  }
  
  # -- ----------------------------------------------------------------------------- Test Code -- #
  # -- ------------------------------------------------------------------------------------------ #

  # p1_AccountType <- "practice"
  # p2_Token       <- OA_Ak
  # p3_Instrument  <- "EUR_USD"
  # p4_AccountID   <- "XXX-XXX-XXXXXXX-XXX"
  # p5_ApiVersion  <- "v2"
  # OA_OpenTrades  <- OpenTrades(AccountType = p1_AccountType, AccountID = p4_AccountID,
  #                              Token = p2_Token, Instrument = p3_Instrument,
  #                              ApiVersion = p5_ApiVersion)
  
}

# -- ------------------------------------------------------------------------------ -- #
# -- A particular trade's Information  -------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

TradeInfo  <- function(AccountType,AccountID,Token,TradeNumber)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/trades/",sep="")
  Querythttp3 <- paste(Querythttp2,TradeNumber,sep="")
  QueryInst1  <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Close a particular trade ----------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

CloseTrade  <- function(AccountType,AccountID,Token,TradeID)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/trades/",sep="")
  Querythttp3 <- paste(Querythttp2,TradeID,sep="")
  
  auth  <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  
  DELETEOrder <- httpDELETE(Querythttp3, cainfo=system.file("CurlSSL","cacert.pem",
                                        package="RCurl"), httpheader=auth)
  FinalData <- fromJSON(DELETEOrder, simplifyDataFrame = TRUE)
  
return(FinalData)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Modify parameters of a trade ------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

ModifyTrade <- function(AccountType, AccountID, Token, 
                        TradeID, StopLoss, TakeProfit, TrailingStop)
{
  if(AccountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
  if(AccountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
  } else print("Account type error. Must be practice or live")
  
  Queryhttp  <- paste(httpaccount,"/v1/accounts/", sep = "")
  Queryhttp1 <- paste(Queryhttp,AccountID, sep = "")
  Queryhttp2 <- paste(Queryhttp1,"/orders/", sep = "")
  Queryhttp3 <- paste(Queryhttp2,TradeID, sep = "")
  
  auth   <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  params <- list(stopLoss=StopLoss, takeProfit=TakeProfit, trailingStop=TrailingStop)
  DataReturn <- PATCH(Queryhttp3, add_headers(auth),body = params)
  
return(DataReturn)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Account's Open Positions List ------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

AccountPositions <- function(AccountType,AccountID,Token)
{
  
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/positions",sep="")
  QueryInst1  <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Position respect a particular instrument ------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

InstrumentPositions  <- function(AccountType,AccountID,Token,Instrument)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/positions/",sep="")
  Querythttp3 <- paste(Querythttp2,Instrument,sep="")
  QueryInst1  <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ----------------------------------------------------------------------------------------- -- #
# -- Historical of transactions -------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

AccountHistTransactions  <- function(AccountType,AccountID,Token,Instrument,Count,ApiVersion) {
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else if (AccountType == "live"){
    httpaccount <- "https://api-fxtrade.oanda.com"
  } else {
    print("Account type error. Must be practice or live")
  }
  
  if(missing(ApiVersion) || ApiVersion == "v1") {
    
    ApiVersion <- "v1"
    
    # -- --------------------------------------------------------------------------- Version 1.0 -- #
    # -- ------------------------------------------------------------------------------------------ #
  
    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/transactions?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=",sep="")
    Querythttp5 <- paste(Querythttp4,Count,sep="")
    QueryInst1  <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

    return(InstJson)
    
    # -- --------------------------------------------------------------------------- Version 2.0 -- #
    # -- ------------------------------------------------------------------------------------------ #
    
  } else if (ApiVersion == "v2") {
    
    auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
    Queryhttp <- paste(httpaccount,"/v3/accounts/",sep="")
    Querythttp1 <- paste(Queryhttp,AccountID,sep="")
    Querythttp2 <- paste(Querythttp1,"/transactions?instrument=",sep="")
    Querythttp3 <- paste(Querythttp2,Instrument,sep="")
    Querythttp4 <- paste(Querythttp3,"&count=",sep="")
    Querythttp5 <- paste(Querythttp4,Count,sep="")
    QueryInst1  <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                                                         package="RCurl"),httpheader=auth)
    InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
    
    pagesize <- InstJson$pageSize
    count    <- InstJson$count
    Hist <- data.frame()
    
    curl_options(filter = "FUNDING")
    pag <- paste0(InstJson$pages[1])
    QI1 <- getURL(pag,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"), httpheader=auth)
    IJ  <- fromJSON(QI1, simplifyDataFrame = TRUE)
   
    return(IJ)

}

  # -- ----------------------------------------------------------------------------- Test Code -- #
  # -- ------------------------------------------------------------------------------------------ #
  
  # p1_AccountType <- "practice"
  # p2_Token       <- OA_Ak
  # p3_Instrument  <- "EUR_USD"
  # p4_AccountID   <- OA_A2
  # p5_ApiVersion  <- "v2"
  # p6_Count <- 2
  # OA_HistTrans  <- AccountHistTransactions(AccountType = p1_AccountType,
  #                                          AccountID = p4_AccountID,
  #                                          Token = p2_Token, Instrument = p3_Instrument,
  #                                           Count = p6_Count, ApiVersion = p5_ApiVersion)

}
  
# -- ------------------------------------------------------------------------------ -- #
# -- A particular transaction info  ----------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

InfoTransaction <- function(AccountType,AccountID,Token,TransactionNum)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/transactions/",sep="")
  Querythttp3 <- paste(Querythttp2,TransactionNum,sep="")
  QueryInst1  <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- General Info about all transactions of the account --------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

AccountTransactions  <- function(AccountType,AccountID,Token)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/alltransactions",sep="")
  QueryInst1  <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Economic Calendar ------------------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

EconomicCalendar <- function(AccountType,Token,Instrument,Period)
{

  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/calendar?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")  
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  CalenH <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  Calend <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend <- subset(Calend, select = -c(currency,region,impact))
  Calend <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <- as.POSIXct(Calend$timestamp,origin = "1970-01-01")

return(Calend)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Historical posistion ratios in OANDA ----------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

RatiosPosturas  <- function(AccountType,Token,Instrument,Period)
{

  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")                   
  
  ratios <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  ratios <- data.frame(fromJSON(ratios))
  ratios[,2] <- as.POSIXct(ratios[,2],origin = "1970-01-01")

return(ratios)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Current OANDA's Clients Spreads ---------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

Spreads <- function(AccountType,Token,Instrument,Period)
{

  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/spreads?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  spread <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  spread <- fromJSON(spread)

return(spread)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Commitment Of Traders -------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

COT <- function(AccountType,Token,Instrument)
{

  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Cot        <- getURL(Queryhttp,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)

return(Cot)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Autochartist "Our Favorites" Signals ----------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

Autochartist_Inst <- function(AccountType,Token,Instrument,Period,Type)
{

  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/signal/autochartist?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  
  Autochart  <- getURL(Queryhttp1,cainfo=system.file("CurlSSL",
  "cacert.pem",package="RCurl"),httpheader=auth)
  Autochart  <- fromJSON(Autochart)

return(Autochart)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Close existing position ------------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

ClosePosition <- function(AccountType, AccountID, Token, Inst)
{
  
  if(AccountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  Queryhttp  <- paste(httpaccount,"/v1/accounts/", sep = "")
  Queryhttp1 <- paste(Queryhttp,AccountID, sep = "")
  Queryhttp2 <- paste(Queryhttp1,"/positions/", sep = "")
  Queryhttp3 <- paste(Queryhttp2,Inst, sep = "")
  
  auth  <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  
  DELETEPosition <- httpDELETE(Queryhttp3, cainfo=system.file("CurlSSL","cacert.pem",
  package="RCurl"), httpheader=auth)
  FinalData  <- fromJSON(DELETEPosition, simplifyDataFrame = TRUE)
  
return(FinalData)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Live FX Rates Streaming (On Test Phase) -------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

LiveRates <- function(AccountType, AccountID, Token, Inst)
{
  
  if(AccountType == "practice"){
    httpaccount <- "https://stream-fxtrade.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://stream-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  http  <- paste(httpaccount,"/v1/prices?accountid=", AccountID, "&instruments=",Inst,sep="")

  StreamPrice <- getURL(http,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"),
                        httpheader=auth)
  DataStream  <- fromJSON(StreamPrice)
  
return(DataStream)
}
