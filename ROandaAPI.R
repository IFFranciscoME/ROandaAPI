# -- Initial Developer: FranciscoME ------------------------------------------------------------------- #
# -- License: GNU General Public License -------------------------------------------------------------- #

# List of available instruments ----------------------------------------------------------------------- #

InstrumentsList <- function(AccountType,Token,AccountID){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/instruments?accountId=",sep="")
  QueryInst  <- paste(Queryhttp,AccountID,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Actual Price Request -------------------------------------------------------------------------------- #

ActualPrice <- function(AccountType,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp    <- paste(httpaccount,"/v1/prices?instruments=",sep="")
  QueryPrec    <- paste(Queryhttp,Instrument,sep="")
  InstPrec     <- getURL(QueryPrec,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  DateTime     <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),
                  format = "%H:%M:%S") + 6*60*60
  DataJSON    <- data.frame(DateTime,InstPrecjson[[1]]$bid,InstPrecjson[[1]]$ask)
  colnames(DataJSON) <- c("Tiempo","Bid","Ask")
  return(DataJSON)
}

# Historical Prices Request --------------------------------------------------------------------------- #

HisPrices  <- function(AccountType,Count,Granularity,DayAlign,TimeAlign,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount  <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  qcount  <- paste("count=",Count,sep="")
  qcandleFormat  <- "candleFormat=midpoint"
  qgranularity   <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")
  
  auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
  QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
  QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
  QueryHistPrec2 <- paste(QueryHistPrec1,qcount,qcandleFormat,qgranularity,
                    qdailyalignment,qalignmentTimezone,sep="&")
  InstHistP      <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
                    package="RCurl"),httpheader=auth)
  InstHistPjson  <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices         <- data.frame(InstHistPjson[[3]])
  Prices$Time    <- as.POSIXct(substr(Prices$time,12,19),format = "%H:%M:%S")
  colnames(Prices) <- c("Time","Open","High","Low","Close","TickVolume","Complete")
  return(Prices)
}
# Accounts per given username  ------------------------------------------------------------------------ #

UsersAccounts <- function(AccountType,Token,UserName){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts?username=",sep="")
  QueryInst  <- paste(Queryhttp,UserName,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Account Information  -------------------------------------------------------------------------------- #

AccountInfo   <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  QueryInfo   <- paste(Queryhttp,AccountID,sep="")
  CtaInfo     <- getURL(QueryInfo,cainfo=system.file("CurlSSL",
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

# Actual order in the account ------------------------------------------------------------------------- #

AccountOrders  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=2",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Place a new order ----------------------------------------------------------------------------------- #

NewOrder <- function(AccountType,Token,Instrument,AccountID,Count,Side,OrderType){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts/",sep="")
  Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
  Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
  
  postForm(Queryhttp2,style="POST",.params=c(instrument=Instrument,units=Count,upperBound=slipUp,
  lowerBound=slipDown,takeProfit=takeProfit,stopLoss=stopLoss,side=Side,type=OrderType),
  opts=list(httpheader=auth,ssl.verifypeer = FALSE))
}

# Information about a particular order ---------------------------------------------------------------- #

OrderInfo  <- function(AccountType,AccountID,Token,OrderNum){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders/",sep="")
  Querythttp3  <- paste(Querythttp2,OrderNum,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# List of open trades --------------------------------------------------------------------------------- #

OpenTrades  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=100",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# A particular trade's Information  ------------------------------------------------------------------- #

TradeInfo  <- function(AccountType,AccountID,Token,TradeNumber){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades/",sep="")
  Querythttp3  <- paste(Querythttp2,TradeNumber,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Account's Open Positions List ----------------------------------------------------------------------- #

AccountPositions  <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Position respect a particular instrument ------------------------------------------------------------ #

InstrumentPositions  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions/",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Historical of transactions -------------------------------------------------------------------------- #

AccountHistTransactions  <- function(AccountType,AccountID,Token,Instrument,Count){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=",sep="")
  Querythttp5  <- paste(Querythttp4,Count,sep="")
  QueryInst1   <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# A particular transaction info  --------------------------------------------------------------------- #

InfoTransaction  <- function(AccountType,AccountID,Token,TransactionNum){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions/",sep="")
  Querythttp3  <- paste(Querythttp2,TransactionNum,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# General Info about all transactions of the account -------------------------------------------------- #

AccountTransactions  <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/alltransactions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  # InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(QueryInst1)
}

# Economic Calendar ----------------------------------------------------------------------------------- #

EconomicCalendar <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/calendar?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")  
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  CalenH  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  Calend  <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend  <- subset(Calend, select = -c(currency,region,impact))
  Calend  <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <- as.POSIXct(Calend$timestamp,origin = "1970-01-01")
  return(Calend)
}

# Historical posistion ratios in OANDA ---------------------------------------------------------------- #

RatiosPosturas <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")                   
  
  ratios     <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  ratios     <- data.frame(fromJSON(ratios))
  ratios[,2] <- as.POSIXct(ratios[,2],origin = "1970-01-01")
  return(ratios)
}

# Current OANDA's Clients Spreads --------------------------------------------------------------------- #

Spreads <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
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

# Commitment Of Traders ------------------------------------------------------------------------------- #

COT <- function(AccountType,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Cot        <- getURL(Queryhttp,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  return(Cot)
}

# Order Book ------------------------------------------------------------------------------------------ #

OrderBook <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
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

# Autochartist "Our Favorites" Signals ---------------------------------------------------------------- #

Autochartist <- function(AccountType,Token,Instrument,Period,Type){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error")
  
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

# PENDING Modify parameters of an order ------------------------------------------------------------- #

# PENDING Close an order -..------------------------------------------------------------------------- #

# PENDING Modify parameters of a trade -------------------------------------------------------------- #

# PENDING Close a trade ----------------------------------------------------------------------------- #

# PENDING Close existing position ------------------------------------------------------------------- #
