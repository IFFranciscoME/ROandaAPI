## ROandaAPI

R Code API for Forex Trading with OANDA Broker (CFTC and SEC regulated retail broker). This code is a series of functions for using the Oanda REST API, more about Oanda can be read at their [HomePage](http://www.oanda.com/)

- **Initial Developer:** IF.FranciscoME
- **License:** GNU General Public License
- **Location:** Guadalajara, México

## Previous Reading

At Oanda one can trade more than just *FOREX* market, but mainly it is that, so in order to have a better comprehension about what *FOREX* trading actually is:

- **Wiki source:** [Investopedia](http://www.investopedia.com/university/forexmarket/)
- **Authority:** [Office of Investor Education and Advocacy](http://www.sec.gov/investor/alerts/forextrading.pdf)

And about What and API, REST API are ?

- **Mayor Brand source:** [IBM Developer Works](http://www.ibm.com/developerworks/library/ws-restful/)
- **Wiki source:** [RestApiTutorial](http://www.restapitutorial.com/lessons/whatisrest.html)

## Getting Started

1. **Have an account:** You must actually get an account with OANDA, either *Live* or *Practice* is ok and exactly the same, if you do not have one you can create it from here [Open Account](https://fxtrade.oanda.com/your_account/fxtrade/register/gate?utm_source=oandaapi&utm_medium=link&utm_campaign=devportaldocs_demo)

2. **Get REST API Access:** Log in -> Search for *Manage API Access* link -> *Generate Personal Access Token*

3. **Save Info Safely:** Having that *Token* is like having your password, so keep it safe. If you ever lost it or need to change it, its ok and the previous one goes deprecated as soon as you generate another one, so you can *Revoke* it whenever you want.

## *R* Requirements
- Any version its ok.
- Does not have to be used RStudio or any other GUI for *R*.
- Following packages will be needed, i just added a code in order to autodetect them and install them if you havent yet

```r
if (!require(downloader)) install.packages('downloader', quiet = TRUE)
suppressMessages(library (downloader))   # for downloading files over http and https. (OPTIONAL)
if (!require(httr))       install.packages('httr', quiet = TRUE)
suppressMessages(library (httr))         # Tools for Working with URLs and HTTP
if (!require(jsonlite))   install.packages('jsonlite', quiet = TRUE)
suppressMessages(library (jsonlite))     # A Robust, High Performance JSON Parser and Generator for R
if (!require(RCurl))      install.packages('RCurl', quiet = TRUE)
suppressMessages(library (RCurl))        # General network (HTTP/FTP/...) client interface for R
```
The previous packages are actually necessary not for running the functions, those are regular expressions of R, but when you call and execute them in your code. Thats because some functions i used inside the API.

in case you prefer have access to this API with one line of code, you can use the following and the functions will available locally *(Note: You must run this code every time you begin R Session).*

```r
downloader::source_url("http://bit.ly/GitHubROandaAPI",prompt=FALSE,quiet=TRUE)
```
## More Official Resources

OANDA has a dedicated page to provide all sort of help and examples for developers, in many languages, you can always go there and consider have a resource full place for you Algorithmic Trading.

- [Developtment Guide](http://developer.oanda.com/rest-live/development-guide/) Basic introductory guide
- [Authentication](http://developer.oanda.com/rest-live/authentication/) When Authentication Enters
- [Troubleshooting](http://developer.oanda.com/rest-live/troubleshooting-errors/) Breath and then Read
- [Sample Codes](http://developer.oanda.com/rest-live/sample-code/) So this actually works, huh?
- [Interactive Console](http://developer.oanda.com/rest-practice/console/) Try before implement

## Minimal Example
Minimal example to fetch instrument list and from one of them the past prices

```r
AccountID   <- 1234567
AccountType <- "practice"
Granularity <- "H6"
DayAlign    <- 0
TimeAlign   <- "America%2FMexico_City"
Token       <- # Your Token
ResagosMax  <- 100
NC    <- .99
Start <- "2014-01-01"
End   <- "2015-10-01"

ListaInst   <- data.frame(InstrumentsList(AccTp,Token,AccID))[,c(1,3)]
TInst <- ListaInst[117,1]
PreciosHist <- HisPrices(AccTp,Gran,DayAlign,TimeAlign,Token,TInst,FIni,FFin)
PrecioCl    <- data.frame(PreciosHist$TimeStamp, round(PreciosHist$Close,4))
colnames(PrecioCl) <- c("TimeStamp","PrecioCl")

Entrenamiento <- trunc(length(PrecioCl[,1])*.85)
PrecioClEnt   <- PrecioCl[1:Entrenamiento,]
PrecioClVal   <- data.frame(PrecioClVal$TimeStamp, round(PrecioClVal$PrecioCl,4))
colnames(PrecioClVal) <-c("TimeStamp","PrecioCl")

ResagosCl <- data.frame(cbind(PrecioClEnt[,1:2],Lag(x=PrecioClEnt$PrecioCl,k=1:ResagosMax)))
ResagosCl <- ResagosCl[-c(1:ResagosMax),]
```

