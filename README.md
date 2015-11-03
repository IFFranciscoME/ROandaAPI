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

## More Official Resources

OANDA has a dedicated page to provide all sort of help and examples for developers, in many languages, you can always go there and consider have a resource full place for you Algorithmic Trading.

- [Developtment Guide](http://developer.oanda.com/rest-live/development-guide/) Basic introductory guide
- [Authentication](http://developer.oanda.com/rest-live/authentication/) When Authentication Enters
- [Troubleshooting](http://developer.oanda.com/rest-live/troubleshooting-errors/) Breath and then Read
- [Sample Codes](http://developer.oanda.com/rest-live/sample-code/) So this actually works, huh?
- [Interactive Console](http://developer.oanda.com/rest-practice/console/) Try before implement

## *R* Requirements
- Any version its ok.
- Does not have to be used RStudio or any other GUI for *R*.
- Specific packages will be needed, i just added a code in order to autodetect them and install them if you havent yet

The previous packages are actually necessary not for running the functions, those are regular expressions of R, but when you call and execute them in your code. Thats because some functions i used inside the API. in case you prefer have access to this API with one line of code, you can use the following and the functions will available locally *(Note: You must run this code every time you begin R Session).*

## Step by Step for Minimal Example

### 1. Install Required Libraries

Install required libraries if not already installed, if they are installed just load them.

```r
Pkg <- c("base","downloader","forecast","httr","jsonlite","lubridate","moments",
"PerformanceAnalytics","quantmod","reshape2","RCurl","stats","scales","tseries",
"TTR","TSA","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)
```

### 2. Load in local environment the ROandaAPI Code

Uses the function *source_url* from the package **downloader** to source a R script located in a *GitHub* repository.

```r
downloader::source_url("http://bit.ly/GitHubROandaAPI",prompt=FALSE,quiet=TRUE)
```

### 3. Data Inputs

**AccountID** is a 7 digit number, the input must be numeric, **Token** is where yours go, and the **Start** and **End** are dates in format *"YYYY-MM-DD".

```r
AccountType <- "practice"
AccountID   <- 1234567
Token       <- "ba207fab3522f33fda6a91dbfee0522f6-cdbba372874e6e69e4694f050f890277"
TimeAlign   <- "America%2FMexico_City"
Granularity <- "H6"
Start <- "2015-01-01"
End   <- "2015-10-01"
```

### 4. Two Functions as an example

**InstList** in order to get the available instruments in *Oanda* , and **HisPrices*** to get the historical prices of the selected instrument, in this case the 117th so is the *XAU_USD*, that is Gold Vs Usd.

```r
InstList     <- data.frame(InstrumentsList(AccountType,Token,AccountID))[,c(1,3)]
Instrument   <- InstList[117,1]
PastPriceAPI <- HisPrices(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Start,End)
```




