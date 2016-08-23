
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubROandaAPI ---------------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- ------------------------------------------------------------------------------ -- #
# -- List of available instruments ------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

InstrumentsList <- function(AccountType,Token,AccountID)
{

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
    qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qcandleFormat,qgranularity,
                            qdailyalignment,qalignmentTimezone,qcount,sep="&")
    
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
    qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
                            qdailyalignment,qalignmentTimezone,sep="&")
    
  }
  
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","TickVolume","Complete")
  Prices$TimeStamp <- as.POSIXct(strptime(Prices$TimeStamp, "%Y-%m-%d %H:%M:%OS"),
                                 origin="1970-01-01")
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

AccountInfo   <- function(AccountType,AccountID,Token)
{
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

# -- ------------------------------------------------------------------------------ -- #
# -- Actual orders in the account ------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

AccountOrders  <- function(AccountType,AccountID,Token,Instrument)
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
  Querythttp2 <- paste(Querythttp1,"/orders?instrument=",sep="")
  Querythttp3 <- paste(Querythttp2,Instrument,sep="")
  Querythttp4 <- paste(Querythttp3,"&count=2",sep="")
  QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
}

# -- ------------------------------------------------------------------------------ -- #
# -- Place a new order ------------------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

NewOrder <- function(AccountType,AccountID,Token,OrderType,Instrument,Count,Side,
                     Expiry, Price, SL, TP, TS)
{
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
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
          .opts=list(httpheader=auth,ssl.verifypeer = TRUE))
  InstJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(InstJson)
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

# -- ------------------------------------------------------------------------------ -- #
# -- List of open trades ---------------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

OpenTrades  <- function(AccountType,AccountID,Token,Instrument)
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
  Querythttp2 <- paste(Querythttp1,"/trades?instrument=",sep="")
  Querythttp3 <- paste(Querythttp2,Instrument,sep="")
  Querythttp4 <- paste(Querythttp3,"&count=100",sep="")
  QueryInst1  <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
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

# -- ------------------------------------------------------------------------------ -- #
# -- Historical of transactions --------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------ -- #

AccountHistTransactions  <- function(AccountType,AccountID,Token,Instrument,Count)
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
  Querythttp2 <- paste(Querythttp1,"/transactions?instrument=",sep="")
  Querythttp3 <- paste(Querythttp2,Instrument,sep="")
  Querythttp4 <- paste(Querythttp3,"&count=",sep="")
  Querythttp5 <- paste(Querythttp4,Count,sep="")
  QueryInst1  <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)

return(InstJson)
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

Autochartist <- function(AccountType,Token,Instrument,Period,Type)
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
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  Queryhttp4 <- paste(Queryhttp3,"type=",sep="")
  Queryhttp5 <- paste(Queryhttp4,Type,sep="")
  
  Autochart  <- getURL(Queryhttp5,cainfo=system.file("CurlSSL",
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
