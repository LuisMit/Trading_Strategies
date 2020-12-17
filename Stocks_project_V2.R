################################################################################
# Programming group project
#
# Analysing the stocks of the S&P500
#
# Names and Date
################################################################################


# 1.) SET UP -------------------------------------------------------------------

# Loading all necessary packgages
if (!require("quantmod")) install.packages("quantmod"); library("quantmod")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("htmltab")) install.packages("htmltab"); library("htmltab")
if (!require("TTR")) install.packages("TTR"); library("TTR")

# Declaring date variables
startdate <- as.Date("2009-01-01")
enddate <- as.Date(today())


# 2.) IMPORT OF DATA -----------------------------------------------------------

# 2.1) Import tickers of all stocks currently contained in the S&P500

# Retrieving the symbols of all companies in the S&P500 from Wikipedia
SP500_companies_raw <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", 1)
SP500_companies <- select(SP500_companies_raw, "Symbol", "Security", "GICS Sector")
tickers <- SP500_companies$Symbol

# Removing duplicates (symbols with .B)
tickers <- tickers[tickers != c("BRK.B", "BF.B")]

# Removing duplicates (symbols with .B) -> anpassen
# for (i in 1:length(tickers)) {
#   if (str_detect(tickers[i], ".B")) {
#     tickers <- tickers[-i]
#   }
# }

# Inspecting the imported data
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

# Inspecting the imported data
str(stock_data)
head(stock_data)
tail(stock_data)

# 2.3) Importing data of the S&P500 as benchmark
SP500_data <- getSymbols("^GSPC", auto.assign = F, from = startdate, to = enddate, src = "yahoo")

# Inspecting the imported data
str(SP500_data)
head(SP500_data)
tail(SP500_data)


# 3.) DATA PREPARATION ---------------------------------------------------------

# Cleaning the imported data

# Editing columns of the stocks data
for (i in 1:length(stock_data)) {
  # Converting the list elements to data frames
  stock_data[[i]] <- as.data.frame(stock_data[[i]])
  # Converting index column to first column
  stock_data[[i]] <- cbind(Date = rownames(stock_data[[i]]), stock_data[[i]])
  # Creating new index column with numbers
  rownames(stock_data[[i]]) <- 1:nrow(stock_data[[i]])
  # Removing unnecessary columns
  stock_data[[i]] <- stock_data[[i]][, c(1, 5)]
  # Changing column names
  names(stock_data[[i]]) <- c("Date", "Close")
  i = i + 1
}

# Inspecting the first dataframe in the list
view(stock_data[[1]])

# Editing columns of the S&P 500 data
# Converting the list elements to data frames
SP500_data <- as.data.frame(SP500_data)
# Converting index column to first column
SP500_data <- cbind(Date = rownames(SP500_data), SP500_data)
# Creating new index column with numbers
rownames(SP500_data) <- 1:nrow(SP500_data)
# Removing unnecessary columns
SP500_data <- SP500_data[, c(1, 5)]
# Changing column names
names(SP500_data)[2] <- "Close"
# Inspecting the dataframe
view(SP500_data)


# 4.) DATA ANALYSIS ------------------------------------------------------------

# Calculating historic stock returns

# Calculating historic S&P500 returns

for (i in 1:length(stock_data)) {
  stock_data[[i]] <- stock_data[[i]][1:2]
}

# Adding columns with short-term standard moving averages
SMA_periods_st <- c(5, 10, 20, 50)
for (j in SMA_periods_st) {
  for (i in 1:length(stock_data)) {
    stock_data[[i]]$new_column <- SMA(stock_data[[i]][2], n = j)
    colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("SMA_", j)
    i = i + 1
  }
}

# Adding columns with long-term standard moving average
for (i in 1:length(stock_data)) {
  stock_data[[i]]$new_column <- SMA(stock_data[[i]][2], n = 200)
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("SMA", 200)
  i = i + 1
}

# Inspecting the first dataframe in the list
view(stock_data[[1]])

# Adding columns with short-term exponential moving averages
EMA_periods_st <- c(5, 10, 20, 50)
for (j in EMA_periods_st) {
  for (i in 1:length(stock_data)) {
    stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = j)
    colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA", j)
    i = i + 1
  }
}

# Adding columns with long-term exponential moving average
for (i in 1:length(stock_data)) {
  stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = 200)
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA", 200)
  i = i + 1
}

# Removing the data of the year 2009, since we don`t have all the moving averages in this period
for (i in 1:length(stock_data)) {
  stock_data[[i]] <- stock_data[[i]][stock_data[[i]] >= "2010-01-01"]
}

# Inspecting the first dataframe of the list
view(stock_data[[1]])

# Determining bullish crossovers
# SMA 50-200
# EMA 50-200
# EMA 100 - SMA 100
# 200 SMA Richtungswechsel
# 200 EMA Richtungswechsel

# Determining bearish crossovers
# SMA 50-200
# EMA 50-200
# EMA 100 - SMA 100
# 200 SMA Richtungswechsel
# 200 EMA Richtungswechsel

# Creating stock picking algorithms

# Choosing the best one for every stock based on past performance


# 5.) VISUALIZATION ------------------------------------------------------------

# Visualizing the performance of the portfolio and the S&P500

