####################################################################################################
# Programming group project
#
# Analysing stocks of the S&P500 and creating different trading strategies
#
# Intro XCamp 2020 - HSG
# Friday, 2020-12-19
#
# Luis Mitschke, 19-607-191
#
#
#
####################################################################################################


# 1.) SET UP ---------------------------------------------------------------------------------------

# Loading all necessary packgages
if (!require("quantmod")) install.packages("quantmod"); library("quantmod")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("htmltab")) install.packages("htmltab"); library("htmltab")
if (!require("TTR")) install.packages("TTR"); library("TTR")
if (!require("scales")) install.packages("scales"); library("scales")

# Declaring date variables
startdate <- as.Date("2015-01-01")
enddate <- as.Date(today())


# 2.) IMPORT OF DATA -------------------------------------------------------------------------------

# 2.1) Import tickers of all stocks currently contained in the S&P500

# Retrieving the symbols of all companies in the S&P500 from Wikipedia
SP500_companies_raw <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", 1)
# Selectin relevant columns
SP500_companies <- select(SP500_companies_raw, "Symbol", "Security", "GICS Sector")
# Selecting tickers of the companies we want to analyse
tickers <- c("AMZN", "AAPL", "MSFT")

# 2.2) Importing historic individual stock data

# Initializing an empty list
stock_data <- list()

# Importing historic individual stock prices (takes a couple of minutes)
for (i in 1:length(tickers)) {
  stock_data$new <- getSymbols(tickers[i], auto.assign = F, 
                               from = startdate, to = enddate, src = "yahoo")
  names(stock_data)[names(stock_data) == "new"] <- paste0(tickers[i])
  i = i + 1
}

# Inspecting the imported data
str(stock_data)
head(stock_data)
tail(stock_data)
view(stock_data)

# 2.3) Importing data of the S&P500 as benchmark
SP500_data <- getSymbols("^GSPC", auto.assign = F, from = startdate, to = enddate, src = "yahoo")

# Inspecting the imported data
str(SP500_data)
head(SP500_data)
tail(SP500_data)
view(SP500_data)


# 3.) DATA PREPARATION -----------------------------------------------------------------------------

# Cleaning the imported data

# Editing columns of the stocks data
for (i in 1:length(stock_data)) {
  # Converting the list elements to data frames
  stock_data[[i]] <- as.data.frame(stock_data[[i]])
  # Converting index column to first column
  stock_data[[i]] <- cbind(Date = rownames(stock_data[[i]]), stock_data[[i]])
  stock_data[[i]]$Date <- as.Date(stock_data[[i]]$Date)
  # Creating new index column with numbers
  rownames(stock_data[[i]]) <- 1:nrow(stock_data[[i]])
  # Removing unnecessary columns
  stock_data[[i]] <- stock_data[[i]][, c(1, 5)]
  # Changing column names
  names(stock_data[[i]]) <- c("Date", "Close")
  i = i + 1
}

# Inspecting the first dataframe in the stocks dataset
view(stock_data[[1]])

# Editing columns of the S&P 500 data
# Converting the list elements to data frames
SP500_data <- as.data.frame(SP500_data)
# Converting index column to first column
SP500_data <- cbind(Date = rownames(SP500_data), SP500_data)
SP500_data$Date <- as.Date(SP500_data$Date)
# Creating new index column with numbers
rownames(SP500_data) <- 1:nrow(SP500_data)
# Removing unnecessary columns
SP500_data <- SP500_data[, c(1, 5)]
# Changing column names
names(SP500_data)[2] <- "Close"

# Inspecting the S&P500 data
view(SP500_data)


# 4.) DATA ANALYSIS --------------------------------------------------------------------------------

# 4.1) Analysing the S&P500 data

# Adding columns with short-term standard moving averages
SMA_periods_st <- c(20, 50)
for (i in SMA_periods_st) {
  SP500_data$new_column <- SMA(SP500_data[2], n = i)
  colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("SMA_", i)
}

# Adding columns with long-term standard moving average
SP500_data$new_column <- SMA(SP500_data[2], n = 200)
colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("SMA_", 200)

# Adding columns with short-term exponential moving averages
EMA_periods_st <- c(20, 50)
for (i in EMA_periods_st) {
  SP500_data$new_column <- EMA(SP500_data[2], n = i)
  colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("EMA_", i)
}

# Adding columns with long-term exponential moving average
SP500_data$new_column <- EMA(SP500_data[2], n = 200)
colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("EMA_", 200)

# Removing the data of the first two years, since we don`t have all the moving averages in this period
first_year <- year(SP500_data$Date[1]) + 1
SP500_data <- SP500_data[year(SP500_data$Date) > first_year,]
rownames(SP500_data) <- 1:nrow(SP500_data)

# Inspecting the dataframe
view(SP500_data)

# Inspecting the averages with a graphic
ggplot(SP500_data, aes(x = Date)) +
  labs(x = "Date",
       y = "Price",
       title = "Performance of the S&P500") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +
  geom_line(aes(y = Close, color = "S&P500")) +
  geom_line(aes(y = SMA_20, color = "SMA_20")) +
  geom_line(aes(y = SMA_50, color = "SMA_50")) +
  geom_line(aes(y = SMA_200, color = "SMA_200")) +
  scale_colour_manual("", 
                      breaks = c("S&P500", "SMA_20", "SMA_50", "SMA_200"),
                      values = c("black", "red", "blue", "green")) +
  theme_minimal()

# Calculating the total return of the S&P500 between 2010-01-01 and today
return_SP500 <- (SP500_data[nrow(SP500_data), "Close"] - SP500_data[1, "Close"])/
  SP500_data[1, "Close"]

# Calculating daily returns of the S&P500 between 2010-01-01 and today
SP500_data$Return <- 0
for (i in 2:nrow(SP500_data)) {
  SP500_data$Return[i] <- (SP500_data$Close[i] - SP500_data$Close[i-1])/
    SP500_data$Close[i-1]
}

# Adding an index column for the S&P500
SP500_data$Index <- 1
for (i in 2:nrow(SP500_data)) {
  SP500_data$Index[i] <- SP500_data$Index[i-1]*(1+SP500_data$Return[i])
}

# 4.2) Analysing the stock data

# 4.2.1) Calculating moving averages

# Adding columns with short-term standard moving averages
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
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("SMA_", 200)
  i = i + 1
}

# Adding columns with short-term exponential moving averages
for (j in EMA_periods_st) {
  for (i in 1:length(stock_data)) {
    stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = j)
    colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA_", j)
    i = i + 1
  }
}

# Adding columns with long-term exponential moving average
for (i in 1:length(stock_data)) {
  stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = 200)
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA_", 200)
  i = i + 1
}

# Removing the data of the first two years, since we don`t have all the moving averages in this period
for (i in 1:length(stock_data)) {
  first_year <- year(stock_data[[i]]$Date[1]) + 1
  stock_data[[i]] <- stock_data[[i]][year(stock_data[[i]]$Date) > first_year,]
  rownames(stock_data[[i]]) <- 1:nrow(stock_data[[i]])
}

# Inspecting the first dataframe of the list
view(stock_data[[1]])

# 4.2.2) Creating different trading strategies

# Calculating periods in which the 20-day SMA is above the 50-day SMA
# Adding a column with 0s and 1s (0 = not invested, 1 = invested)
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-50-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$SMA_20[i] > stock_data[[k]]$SMA_50[i]) {
      stock_data[[k]]$`Crossover-20-50-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-50-SMA`[i] <- 0
    }
    i = i + 1
  }
  k = k + 1
}

# Calculating periods in which the 20-day EMA is above the 50-day EMA
# Adding a column with 0s and 1s (0 = not invested, 1 = invested)
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-50-EMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_20[i] > stock_data[[k]]$EMA_50[i]) {
      stock_data[[k]]$`Crossover-20-50-EMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-50-EMA`[i] <- 0
    }
    i = i + 1
  }
  k = k + 1
}

# Calculating periods in which the 50-day SMA is above the 200-day SMA
# Adding a column with 0s and 1s (0 = not invested, 1 = invested)
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-50-200-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$SMA_50[i] > stock_data[[k]]$SMA_200[i]) {
      stock_data[[k]]$`Crossover-50-200-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-50-200-SMA`[i] <- 0
    }
    i = i + 1
  }
  k = k + 1
}

# Calculating periods in which the 50-day EMA is above the 200-day EMA
# Adding a column with 0s and 1s (0 = not invested, 1 = invested)
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-50-200-EMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_50[i] > stock_data[[k]]$EMA_200[i]) {
      stock_data[[k]]$`Crossover-50-200-EMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-50-200-EMA`[i] <- 0
    }
    i = i + 1
  }
  k = k + 1
}

# Calculating periods in which the 20-day EMA is above the 20-day SMA
# Adding a column with 0s and 1s (0 = not invested, 1 = invested)
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-EMA-20-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_20[i] > stock_data[[k]]$SMA_20[i]) {
      stock_data[[k]]$`Crossover-20-EMA-20-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-EMA-20-SMA`[i] <- 0
    }
    i = i + 1
  }
  k = k + 1
}

# Inspecting the first dataframe in the list
view(stock_data[[1]])

# 4.2.3) Calculating historic returns of the different strategies

# Calculating return and adding Index column for each dataframe in the stock data
for (k in 1:length(stock_data)) {
  stock_data[[k]]$Return <- 0
  stock_data[[k]]$Index <- 1
  for (i in 2:nrow(stock_data[[k]])) {
    stock_data[[k]]$Return[i] <- (stock_data[[k]]$Close[i] - stock_data[[k]]$Close[i-1])/
      stock_data[[k]]$Close[i-1] 
    stock_data[[k]]$Index[i] <- stock_data[[k]]$Index[i-1]*(1+stock_data[[k]]$Return[i])
    i = i + 1
  }
  k = k + 1
}

# Strategy 1: 20-day SMA is above the 50-day SMA
# Calculating daily returns between 2010-01-01 and today (it takes a couple of minutes)
SMA_20_50 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = as.numeric(0), "Index" = as.numeric(1))
return_cum <- 0
amount <- 0
for (k in 1:(nrow(SMA_20_50)-1)) {
  for (i in 1:length(stock_data)) {
    for (j in 1:nrow(stock_data[[i]])) {
      if (stock_data[[i]]$Date[j] == SMA_20_50$Date[k]) {
        if (stock_data[[i]]$`Crossover-20-50-SMA`[stock_data[[i]]$Date == SMA_20_50$Date[k]] == 1) {
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == SMA_20_50$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  SMA_20_50$Return[k] <- return_cum/amount
}

# Adding an index column 
for (i in 2:nrow(SMA_20_50)) {
  SMA_20_50$Index[i] <- SMA_20_50$Index[i-1]*(1+SMA_20_50$Return[i])
}

# Strategy 2: 20-day EMA is above the 50-day EMA (it takes a couple of minutes)
# Calculating daily returns between 2010-01-01 and today
EMA_20_50 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = 0, "Index" = 1)
return_cum <- 0
amount <- 0
for (k in 1:(nrow(EMA_20_50)-1)) {
  for (i in 1:length(stock_data)) {
    for (j in 1:nrow(stock_data[[i]])) {
      if (stock_data[[i]]$Date[j] == EMA_20_50$Date[k]) {
        if (stock_data[[i]]$`Crossover-20-50-EMA`[stock_data[[i]]$Date == EMA_20_50$Date[k]] == 1) {
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == EMA_20_50$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  EMA_20_50$Return[k] <- return_cum/amount
}

# Adding an index column
for (i in 2:nrow(EMA_20_50)) {
  EMA_20_50$Index[i] <- EMA_20_50$Index[i-1]*(1+EMA_20_50$Return[i])
}

# Strategy 3: 50-day SMA is above the 200-day SMA (it takes a couple of minutes)
# Calculating daily returns between 2010-01-01 and today
SMA_50_200 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = 0, "Index" = 1)
return_cum <- 0
amount <- 0
for (k in 1:(nrow(SMA_50_200)-1)) {
  for (i in 1:length(stock_data)) {
    for (j in 1:nrow(stock_data[[i]])) {
      if (stock_data[[i]]$Date[j] == SMA_50_200$Date[k]) {
        if (stock_data[[i]]$`Crossover-50-200-SMA`[stock_data[[i]]$Date == SMA_50_200$Date[k]] == 1) {
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == SMA_50_200$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  SMA_50_200$Return[k] <- return_cum/amount
}

# Adding an index column
for (i in 2:nrow(SMA_50_200)) {
  SMA_50_200$Index[i] <- SMA_50_200$Index[i-1]*(1+SMA_50_200$Return[i])
}

# Strategy 4: 50-day EMA is above the 200-day EMA (it takes a couple of minutes)
# Calculating daily returns between 2010-01-01 and today
EMA_50_200 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = 0, "Index" = 1)
return_cum <- 0
amount <- 0
for (k in 1:(nrow(EMA_50_200)-1)) {
  for (i in 1:length(stock_data)) {
    for (j in 1:nrow(stock_data[[i]])) {
      if (stock_data[[i]]$Date[j] == EMA_50_200$Date[k]) {
        if (stock_data[[i]]$`Crossover-50-200-EMA`[stock_data[[i]]$Date == EMA_50_200$Date[k]] == 1) {
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == EMA_50_200$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  EMA_50_200$Return[k] <- return_cum/amount
}

# Adding an index column
for (i in 2:nrow(EMA_50_200)) {
  EMA_50_200$Index[i] <- EMA_50_200$Index[i-1]*(1+EMA_50_200$Return[i])
}

# Strategy 5: 20-day EMA is above the 20-day SMA (it takes a couple of minutes)
# Calculating daily returns between 2010-01-01 and today
EMA_20_SMA_20 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = 0, "Index" = 1)
return_cum <- 0
amount <- 0
for (k in 1:(nrow(EMA_20_SMA_20)-1)) {
  for (i in 1:length(stock_data)) {
    for (j in 1:nrow(stock_data[[i]])) {
      if (stock_data[[i]]$Date[j] == EMA_20_SMA_20$Date[k]) {
        if (stock_data[[i]]$`Crossover-20-EMA-20-SMA`[stock_data[[i]]$Date == EMA_20_SMA_20$Date[k]] == 1) {
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == EMA_20_SMA_20$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  EMA_20_SMA_20$Return[k] <- return_cum/amount
}

# Adding an index column
for (i in 2:nrow(EMA_20_SMA_20)) {
  EMA_20_SMA_20$Index[i] <- EMA_20_SMA_20$Index[i-1]*(1+EMA_20_SMA_20$Return[i])
}

# 4.2.4) Creating a dataframe with the results
results <- data.frame("Date" = as.Date(stock_data[[1]]$Date),
                      "SP_500" = SP500_data$Index,
                      "AMZN" = stock_data[[1]]$Index,
                      "AAPL" = stock_data[[2]]$Index,
                      "MSFT" = stock_data[[3]]$Index,
                      "SMA_20_50" = SMA_20_50$Index,
                      "EMA_20_50" = EMA_20_50$Index,
                      "SMA_50_200" = SMA_50_200$Index,
                      "EMA_50_200" = EMA_50_200$Index,
                      "EMA_20_SMA_20" = EMA_20_SMA_20$Index)

# Inspecting the results
view(results)


# 5.) VISUALIZATION --------------------------------------------------------------------------------

# Visualizing the performance of the four different strategies in comparison with the S&P500
ggplot(results, aes(x = Date)) +
  labs(x = "Date",
       y = "Index",
       title = "Performance of the strategies") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +
  geom_line(aes(y = SP_500, color = "SP_500")) +
  geom_line(aes(y = AMZN, color = "AMZN")) +
  geom_line(aes(y = AAPL, color = "AAPL")) +
  geom_line(aes(y = MSFT, color = "MSFT")) +
  geom_line(aes(y = SMA_20_50, color = "SMA_20_50")) +
  geom_line(aes(y = EMA_20_50, color = "EMA_20_50")) +
  geom_line(aes(y = SMA_50_200, color = "SMA_50_200")) +
  geom_line(aes(y = EMA_50_200, color = "EMA_50_200")) +
  geom_line(aes(y = EMA_20_SMA_20, color = "EMA_20_SMA_20")) +
  scale_colour_manual("", 
                      breaks = c("SP_500", "AMZN", "AAPL", "MSFT",
                                 "SMA_20_50", "EMA_20_50", "SMA_50_200", "EMA_50_200", "EMA_20_SMA_20"),
                      values = c("black", "antiquewhite2", "antiquewhite3", "antiquewhite4", 
                                 "red", "blue", "green", "darkorange", "darkorchid")) +
  theme_minimal()

# Results
# ...
