#######################################################################
# Programming group project
#
# Analysing the stocks of the S&P500
#
# Names and Date
#######################################################################


# 1.) SET UP -------------------------------------------------------------------

# Loading all necessary packgages
if (!require("quantmod")) install.packages("quantmod"); library("quantmod")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("htmltab")) install.packages("htmltab"); library("htmltab")

# Declaring variables
startdate <- as.Date("2000-01-01")
enddate <- as.Date(today())


# 2.) IMPORT OF DATA -------------------------------------------

# 2.1) Import tickers of all stocks contained in the S&P500

# Retrieving the symbols of all companies in the S&P500 from Wikipedia
SP500_companies_raw <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", 1)
SP500_companies <- select(SP500_companies_raw, "Symbol", "Security", "GICS Sector")
tickers <- SP500_companies$Symbol

# Removing duplicates (symbols with .B)
tickers <- tickers[tickers != c("BRK.B", "BF.B")]

# Das klappt noch nicht
# for (i in 1:length(tickers)) {
#   if (str_detect(tickers[i], ".B")) {
#     tickers <- tickers[-i]
#   }
# }

# Checking the imported data
str(stock_data)
head(stock_data)
tail(stock_data)
length(tickers)

# 2.2) Importing historic individual stock data

# Initializing an empty list
stock_data <- list()

# Importing historic individual stock prices (takes a couple of minutes)
for (i in 1:length(tickers)) {
  stock_data$new <- getSymbols(tickers[i], auto.assign = F, from = startdate, to = enddate, src = "yahoo")
  names(stock_data)[names(stock_data) == "new"] <- paste0(tickers[i])
  i = i + 1
}

# Removing stock symbols with missing values 
stock_data$LUMN <- NULL
stock_data$UA <- NULL
stock_data$VIAC <- NULL

# Checking the imported data
str(stock_data)
head(stock_data)
tail(stock_data)

# 2.3) Importing data of the S&P500 as benchmark
SP500_data <- getSymbols("^GSPC", auto.assign = F, from = startdate, to = enddate, src = "yahoo")

# Checking the imported data
str(SP500_data)
head(SP500_data)
tail(SP500_data)


# 3.) DATA PREPARATION ---------------------------------------------------------

# Cleaning the imported data
stock_data[i] <- as.data.frame(stock_data[i])
head(stock_data[1])
# Editing columns of the stocks data
for (i in length(stock_data)) {
  stock_data[i] <- as.data.frame(stock_data[i])
  stock_data[i] <- cbind(Date = rownames(stock_data[i]), stock_data[i])
  rownames(stock_data[i]) <- 1:nrow(stock_data[i])
  stock_data[i] <- stock_data[i][, c(1, 5)]
  names(stock_data[i][2]) <- "Close"
}

# Editing columns of the S&P 500 data
SP500_data <- as.data.frame(SP500_data)
SP500_data <- cbind(Date = rownames(SP500_data), SP500_data)
rownames(SP500_data) <- 1:nrow(SP500_data)
SP500_data <- SP500_data[, c(1, 5)]
names(SP500_data)[2] <- "Close"
head(SP500_data)

# Calculate historic stock returns

# Calculate historic S&P500 returns


# 4.) DATA ANALYSIS ------------------------------------------------------------



# 5.) EVALUATION AND VISUALIZATION ---------------------------------------------


