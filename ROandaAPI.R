# -- Initial Developer: FranciscoME --------------------------------------------------- #
# -- License: GNU General Public License ---------------------------------------------- #
# -- GDL-ITESO Ingeniería Financiera -------------------------------------------------- #

ListaInstrumentos <- function(CuentaTipo,Token,Instrumento,CuentaID){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpcuenta,"/v1/instruments?accountId=",sep="")
  QueryInst  <- paste(Queryhttp,CuentaID,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                                                    package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Petici?n de precio actual ----------------------------------------------------------- #

PrecioActual <- function(CuentaTipo,Token,Instrumento){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp    <- paste(httpcuenta,"/v1/prices?instruments=",sep="")
  QueryPrec    <- paste(Queryhttp,Instrumento,sep="")
  InstPrec     <- getURL(QueryPrec,cainfo=system.file("CurlSSL","cacert.pem",
                                                      package="RCurl"),httpheader=auth)
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  FechaHora    <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),
                             format = "%H:%M:%S") + 6*60*60
  DatosJSON    <- data.frame(FechaHora,InstPrecjson[[1]]$bid,InstPrecjson[[1]]$ask)
  colnames(DatosJSON) <- c("Tiempo","Bid","Ask")
  return(DatosJSON)
}

# Peticion de precios historicos ---------------------------------------------------------------------- #

PreciosHist  <- function(AccountType,Count,Granularity,DayAlign,TimeAlign,Token,Instrument){
  if(AccountType == "practice"){
    httpcuenta  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpcuenta  <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  qcount  <- paste("count=",Count,sep="")
  qcandleFormat  <- "candleFormat=midpoint"
  qgranularity   <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")
  
  auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
  QueryHistPrec  <- paste(httpcuenta,"/v1/candles?instrument=",sep="")
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
# Petici?n de cuentas por usuario --------------------------------------------------------------------- #

CuentasUsuario <- function(CuentaTipo,Token,NombreUsuario){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpcuenta,"/v1/accounts?username=",sep="")
  QueryInst  <- paste(Queryhttp,NombreUsuario,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                                                    package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Petici?n de informaci?n de cuenta ------------------------------------------------------------------- #

InfoCuenta   <- function(CuentaTipo,CuentaID,Token){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  QueryInfo   <- paste(Queryhttp,idcuenta,sep="")
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

# Ordenes de la cuenta -------------------------------------------------------------------------------- #

OrdenesCuenta  <- function(CuentaTipo,CuentaID,Token,Instrumento){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrumento,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=2",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Colocar nueva orden --------------------------------------------------------------------------------- #

NuevaOrden <- function(CuentaTipo,Token,Instrumento,CuentaID,Cantidad,Sentido,OrdenTipo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth       <- c(Authorization = paste("Bearer",token,sep=" "))
  Queryhttp  <- paste(httpcuenta,"/v1/accounts/",sep="")
  Queryhttp1 <- paste(Queryhttp,cuenta_A,sep="")
  Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
  
  postForm(Queryhttp2,style="POST",.params=c(instrument=inst,units=unid,upperBound=slipUp,
                                             lowerBound=slipDown,takeProfit=takeProfit,stopLoss=stopLoss,side=sent,type=tipo),
           opts=list(httpheader=auth,ssl.verifypeer = FALSE))
}

# Petici?n de informaci?n de una orden ---------------------------------------------------------------- #

InfoOrden  <- function(CuentaTipo,CuentaID,Token,NumOrden){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders/",sep="")
  Querythttp3  <- paste(Querythttp2,NumOrden,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Lista de operaciones abiertas ----------------------------------------------------------------------- #

ListaOpsAbiertas  <- function(CuentaTipo,CuentaID,Token,Instrumento){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrumento,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=100",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Informaci?n de una operaci?n ------------------------------------------------------------------------ #

InfoOperacion  <- function(CuentaTipo,CuentaID,Token,NumOperacion){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades/",sep="")
  Querythttp3  <- paste(Querythttp2,NumOperacion,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Lista de posiciones abiertas ------------------------------------------------------------------------ #

PosicionesCuenta  <- function(CuentaTipo,CuentaID,Token){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Posici?n respecto a un instrumento ------------------------------------------------------------------ #

PosicionesInst  <- function(CuentaTipo,CuentaID,Token,Instrumento){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions/",sep="")
  Querythttp3  <- paste(Querythttp2,Instrumento,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Obtener hist?rico de transacciones ------------------------------------------------------------------ #

TransaccionesCuenta  <- function(CuentaTipo,CuentaID,Token,Instrumento,Cantidad){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrumento,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=",sep="")
  Querythttp5  <- paste(Querythttp4,Cantidad,sep="")
  QueryInst1   <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Obtener informaci?n de una transacci?n -------------------------------------------------------------- #

InfoTransaccion  <- function(CuentaTipo,CuentaID,Token,NumTransaccion){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions/",sep="")
  Querythttp3  <- paste(Querythttp2,NumTransaccion,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# Obtener informaci?n de todas las transacciones de la cuenta ----------------------------------------- #

TransaccionesCuenta  <- function(CuentaTipo,CuentaID,Token){
  if(CuentaTipo == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(CuentaTipo == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpcuenta,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,CuentaID,sep="")
  Querythttp2  <- paste(Querythttp1,"/alltransactions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  #  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(Querythttp2)
}

# Calendario economico -------------------------------------------------------------------------------- #

CalendarioEconomico <- function(CuentaTipo,Token,Instrumento,Periodo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/calendar?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")  
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,periodo,sep="")
  
  CalenH  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                                                  "cacert.pem",package="RCurl"),httpheader=auth)
  Calend  <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend  <- subset(Calend, select = -c(currency,region,impact))
  Calend  <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <- as.POSIXct(Calend$timestamp,origin = "1970-01-01")
  return(Calend)
}

# Historico de ratio de posturas en OANDA ------------------------------------------------------------- #

RatiosPosturas <- function(CuentaTipo,Token,Instrumento,Periodo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,periodo,sep="")                   
  
  ratios     <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                                                     "cacert.pem",package="RCurl"),httpheader=auth)
  ratios     <- data.frame(fromJSON(ratios))
  ratios[,2] <- as.POSIXct(ratios[,2],origin = "1970-01-01")
  return(ratios)
}

# Spreads --------------------------------------------------------------------------------------------- #

Spreads <- function(CuentaTipo,Token,Instrumento,Periodo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/spreads?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,periodo,sep="")
  
  spread <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                                                 "cacert.pem",package="RCurl"),httpheader=auth)
  spread <- fromJSON(spread)
  return(spread)
}

# Solicitar Commitment Of Traders --------------------------------------------------------------------- #

COT <- function(CuentaTipo,Token,Instrumento){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")
  Cot        <- getURL(Queryhttp,cainfo=system.file("CurlSSL",
                                                    "cacert.pem",package="RCurl"),httpheader=auth)
  return(Cot)
}

# Order Book ------------------------------------------------------------------------------------------ #

OrderBook <- function(CuentaTipo,Token,Instrumento,Periodo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ") 
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/orderbook_data?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,periodo,sep="")  
  
  orderbook  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                                                     "cacert.pem",package="RCurl"),httpheader=auth)
  orderbook  <- fromJSON(orderbook)
  return(orderbook)
}

# Se?ales de Autochartist "Our Favorites" ------------------------------------------------------------- #

Autochartist <- function(CuentaTipo,Token,Instrumento,Periodo,tipo){
  if(tcuenta == "practice"){
    httpcuenta <- "https://api-fxpractice.oanda.com"
  } else 
    if(tcuenta == "live"){
      httpcuenta <- "https://api-fxtrade.oanda.com"
    } else print("error tipo de cuenta no reconocido")
  
  auth  <- c(Authorization = paste("Bearer",token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpcuenta,"/labs/v1/signal/autochartist?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,inst,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,periodo,sep="")
  Queryhttp4 <- paste(Queryhttp3,"type=",sep="")
  Queryhttp5 <- paste(Queryhttp4,tipo,sep="")
  
  Autochart  <- getURL(Queryhttp5,cainfo=system.file("CurlSSL",
                                                     "cacert.pem",package="RCurl"),httpheader=auth)
  Autochart  <- fromJSON(Autochart)
  return(Autochart)
}

# PENDIENTE Modificar par?metros de una orden --------------------------------------------------------- #

# PENDIENTE Cerrar una orden -------------------------------------------------------------------------- #

# PENDIENTE Modificar par?metros de una operaci?n ----------------------------------------------------- #

# PENDIENTE Cerrar una operaci?n ---------------------------------------------------------------------- #

# PENDIENTE Cerrar posici?n existente ----------------------------------------------------------------- #
