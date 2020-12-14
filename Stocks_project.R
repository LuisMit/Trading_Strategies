#######################################################################
# Programming group project
#
# Stock Picking Machine Learning Algorithm
#
# Names and Date
#######################################################################


# 1.) SET UP -------------------------------------------------------------------
# Load all necessary packgages
if (!require("GetQuandlData")) install.packages("GetQuandlData"); library("GetQuandlData")
if (!require("Quandl")) install.packages("Quandl"); library("Quandl")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("htmltab")) install.packages("htmltab"); library("htmltab")

# Declare variables
quandl_api <- "AsLB_vERSNsFGvHoonsf"
Quandl.api_key("AsLB_vERSNsFGvHoonsf")
startdate <- as.Date("2000-01-01")
enddate <- as.Date(today())

# 2.) IMPORT OF DATA -----------------------------------------------------------
# 2.1) Import tickers of all stocks contained in the S&P500
SP500_companies_raw <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", 1)
SP500_companies <- select(SP500_companies_raw, "Symbol", "Security", "GICS Sector")
tickers <- SP500_companies$Symbol
MMM <- get_Quandl_series("MMM", api_key = quandl_api, first_date = startdate, last_date = enddate,
                  collapse = "quarterly")

# 2.2) Import historic individual stock data
for (i in 1:nrow(tickers)) {
  get_Quandl_series(tickers[i], api_key = quandl_api)
  
}

# 2.2) Merge the two data frames


# 3.) CLEANING AND PREPARATION OF THE DATA -------------------------------------


# 4.) APPLICATION OF DIFFERENT MACHINE LEARNING ALGORITHMS ---------------------
# 4.1) First algortithm
# 4.2) Second algortithm
# 4.3) Third algortithm
# 4.4) Fourth algorithm


# 5.) EVALUATION AND VISUALIZATION OF ALGORITHMS -------------------------------


# 6.) APPLICATION OF THE BEST ALGORITHM ON THE CURRENT MARKET ------------------

