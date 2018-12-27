
# -- Remove all objects from enviornment
rm(list=ls())

# -- Rquired Packages in order to use the R API
library("downloader")
library("RCurl")
library("jsonlite")
library("httr")

# -- Load locally from GitHub the API
RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- ---------------------------------------------------------------------------------- #
# -- General Values of Parameters in API ---------------------------------------------- #
# -- ---------------------------------------------------------------------------------- #

# -- character ---- # Your Account Type "practice" or "live"
OA_At <- "practice"  
# -- numeric ------ # Your Account ID 
OA_Ai <- 1742531     
# -- character ---- # Your Token
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6"
# -- numeric ------ # Hour of the "End of the Day"
OA_Da <- 17
# -- character ---- # Time Zone in format "Continent/Zone 
OA_Ta <- "America%2FMexico_City"
# -- character ---- # Instrument in format "CURRENCY_CURRENCY"
OA_In <- "EUR_USD"
# -- character ---- # Granularity of the prices
OA_Gn <- "D"
# -- character ---- # Initial Date
OA_F1Px <- "2016-01-01"
# -- character ---- # Final Date
OA_F2Px <- "2016-08-07"
# -- character ---- # Order Type to place "market" or "limit"
OA_Ot <- "market"
# -- numeric ------ # Order's Lot Size to open
OA_Ls <- 1 
# -- character ---- # Side of the order, "buy" or "sell"
OA_Sd <- "buy"
# -- character ---- # Expery date of the order
OA_Ex <- "2016-09-01"
# -- numeric ------ # The price to execute the order
OA_Pr <- 1
# -- numeric ------ # Take Profit
OA_Tp <- 1
# -- numeric ------ # Stop Loss
OA_Sl <- 1
# -- numeric ------ # Trailing Stop
OA_Ts <- 1

# -------------------------------------------------------------- Basic Trade Example -- #
# -- Step 0 .- Get System Time -------------------------------------------------------- #
# -- Step 1 .- Get Instrument Lists --------------------------------------------------- #
# -- Step 2 .- Get current price of a chosen instrument ------------------------------- #
# -- Step 3 .- Open a "buy" MARKET ORDER of previously chosen instrument -------------- #
# -- Step 4 .- Get TradeID of previously Opened trade --------------------------------- #
# -- Step 5 .- Modify TP and SL ------------------------------------------------------- #
# -- Step 6 .- Close TradeID ---------------------------------------------------------- #
# ------------------------------------------------------------------------------------- #

# -- 0.- Get System Time
Step0 <- Sys.time()

# -- 1.- Get Instrument Lists
Step1 <- InstrumentsList(OA_At,OA_Ak,OA_Ai)

# -- 2 .- Get current price of a chosen instrument
OA_In <- Step1[1,] # AU200_AUD
Step2 <- ActualPrice(OA_At,OA_Ak,OA_In$Instrument)

# -- 3 .- Open a "buy" market order of previously chosen instrument
Step3 <- NewOrder(AccountType = OA_At,
         AccountID  = OA_Ai,
         Token = OA_Ak,
         OrderType  = OA_Ot,
         Instrument = OA_In$Instrument,
         Count  = OA_Ls,
         Side   = OA_Sd,
         SL = trunc(Step2$Ask*0.95), # 5  % loss
         TP = trunc(Step2$Ask*1.10), # 10 % Profit
         TS = 100)                   # 10 Pips for Trailing Stop

Step4 <- OpenTrades(AccountType = OA_At,
                    AccountID = OA_Ai,
                    Token = OA_Ak,
                    Instrument = OA_In$Instrument)

Step5 <- ModifyTrade(AccountType = OA_At,
                     AccountID = OA_Ai,
                     Token = OA_Ak,
                     TradeID = 10406993698, 
                     StopLoss = 0, 
                     TakeProfit = 0,
                     TrailingStop = 150)

Step6 <- CloseTrade(AccountType = OA_At,
                    AccountID = OA_Ai,
                    Token = OA_Ak,
                    TradeID = Step4$trades$id[1])
