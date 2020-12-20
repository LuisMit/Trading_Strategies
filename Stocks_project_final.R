##############################################################################################################################
# Programming group project
#
# Moving Average Trading Strategies
# The code of this project was written in RStudio and can be executed step by step
#
# Intro XCamp 2020 - HSG
# Saturday, 2020-12-19
#
# Luis Mitschke (coding4fun)
# Pascal Simon (PasciS)
# Martin Husek (MartinCoder)
# Leanne Arcon (LAx)
##############################################################################################################################


# 1.) SET UP -----------------------------------------------------------------------------------------------------------------

# Loading all necessary packages to execute the code.
if (!require("quantmod")) install.packages("quantmod"); library("quantmod")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("htmltab")) install.packages("htmltab"); library("htmltab")
if (!require("TTR")) install.packages("TTR"); library("TTR")

# Declaring the time period during which the trading strategies are apllied.
# Note that the data for the first two years will be removed as soon as the moving averages are computed.
# Thus, the time period for the backtesting will start two years later than the date declared as start date.
startdate <- as.Date("2015-01-01")
enddate <- as.Date(today())


# 2.) IMPORT OF DATA ---------------------------------------------------------------------------------------------------------

# 2.1) Importing tickers of all stocks currently contained in the S&P500.

# Retrieving the symbols of all companies in the S&P500 from Wikipedia.
SP500_companies_raw <- htmltab("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", 1)

# Selecting relevant columns of the dataset from Wikipedia.
SP500_companies <- select(SP500_companies_raw, "Symbol", "Security", "GICS Sector")

# Selecting tickers of the companies we want to analyse (Amazon, Apple and Microsoft).
# The companies we want to analyse have to provide price data in the defined time period.
# Important: If more than five companies are chosen to analyse simultaneously, the backtesting of the
# trading strategies could take a lot of time.
tickers <- SP500_companies$Symbol[SP500_companies$Security %in% 
                                    c("Amazon.com Inc.",  "Apple Inc.",  "Microsoft Corp.")]

# 2.2) Importing historic individual stock data

# Initializing an empty list.
stock_data <- list()

# Importing historic stock prices for the companies we want to analyse.
for (i in 1:length(tickers)) {
  # Creating new list element and importing data of company i.
  stock_data$new <- getSymbols(tickers[i], auto.assign = F, 
                               from = startdate, to = enddate, src = "yahoo")
  # Renaming the new list element with the ticker of company i.
  names(stock_data)[names(stock_data) == "new"] <- paste0(tickers[i])
}

# Inspecting the imported data.
str(stock_data)
head(stock_data)
tail(stock_data)

# 2.3) Importing data of the S&P500 as benchmark.
SP500_data <- getSymbols("^GSPC", auto.assign = F, from = startdate, to = enddate, src = "yahoo")

# Inspecting the imported data.
str(SP500_data)
head(SP500_data)
tail(SP500_data)
view(SP500_data)


# 3.) DATA PREPARATION -------------------------------------------------------------------------------------------------------

# Cleaning the imported data.

# Editing columns of each list element of the stock data.
for (i in 1:length(stock_data)) {
  # Converting the list elements to data frames.
  stock_data[[i]] <- as.data.frame(stock_data[[i]])
  # Converting the row index column with the dates to the first column.
  stock_data[[i]] <- cbind(Date = rownames(stock_data[[i]]), stock_data[[i]])
  stock_data[[i]]$Date <- as.Date(stock_data[[i]]$Date)
  # Creating new row index column with numbers.
  rownames(stock_data[[i]]) <- 1:nrow(stock_data[[i]])
  # Removing unnecessary columns (High, Low, Volume, Adjusted).
  stock_data[[i]] <- stock_data[[i]][, c(1, 5)]
  # Changing column names.
  names(stock_data[[i]]) <- c("Date", "Close")
}

# Inspecting the first dataframe in the stocks dataset.
view(stock_data[[1]])

# Editing columns of the S&P 500 data.
# Converting the list elements to data frames.
SP500_data <- as.data.frame(SP500_data)
# Converting the row index column with the dates to the first column.
SP500_data <- cbind(Date = rownames(SP500_data), SP500_data)
SP500_data$Date <- as.Date(SP500_data$Date)
# Creating new row index column with numbers.
rownames(SP500_data) <- 1:nrow(SP500_data)
# Removing unnecessary columns (High, Low, Volume, Adjusted).
SP500_data <- SP500_data[, c(1, 5)]
# Changing column names.
names(SP500_data)[2] <- "Close"

# Inspecting the S&P500 data.
view(SP500_data)


# 4.) DATA ANALYSIS ----------------------------------------------------------------------------------------------------------

# 4.1) Analysing the S&P500 data.

# Adding columns with short-term standard moving averages to the S&P 500 dataframe.
# Declaring time periods for the short-term simple moving averages (20 and 50 days).
SMA_periods_st <- c(20, 50)
for (i in SMA_periods_st) {
  # Adding a new column with the moving average.
  SP500_data$new_column <- SMA(SP500_data[2], n = i)
  # Renaming the column.
  colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("SMA_", i)
}

# Adding a column with a long-term standard moving average (200 days).
SP500_data$new_column <- SMA(SP500_data[2], n = 200)
# Renaming the column.
colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("SMA_", 200)

# Adding columns with short-term exponential moving averages to the S&P 500 dataframe.
# Declaring time periods for the short-term exponential moving averages (20 and 50 days).
EMA_periods_st <- c(20, 50)
for (i in EMA_periods_st) {
  # Adding a new column with the moving average.
  SP500_data$new_column <- EMA(SP500_data[2], n = i)
  # Renaming the column.
  colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("EMA_", i)
}

# Adding a column with a long-term exponential moving average (200 days).
SP500_data$new_column <- EMA(SP500_data[2], n = 200)
# Renaming the column.
colnames(SP500_data)[which(names(SP500_data) == "new_column")] <- paste0("EMA_", 200)

# Removing the data of the first two years, since we are not able to calculate all the 
# moving averages in this period.
# Declaring a variable with the following year of the first data entry.
skip_year <- year(SP500_data$Date[1]) + 1
SP500_data <- SP500_data[year(SP500_data$Date) > skip_year,]
# Editing the row index column.
rownames(SP500_data) <- 1:nrow(SP500_data)

# Inspecting the dataframe.
view(SP500_data)

# Inspecting the moving averages with a diagram.
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

# Calculating daily returns of the S&P500 between 2017-01-01 and today.
SP500_data$Return <- 0
for (i in 2:nrow(SP500_data)) {
  SP500_data$Return[i] <- (SP500_data$Close[i] - SP500_data$Close[i-1])/
    SP500_data$Close[i-1]
}

# Adding an index column for the S&P500 based on the previously computed returns.

# Explanation for the index column which can be applied to all the index columns in this programme:
# The index column starts at the value 1 at the first day. The value at the day x is computed by multiplying the index
# value at the day x-1 with the return at the day x.
# The index columns in this programme help to compare the performance of the different financial products as well as
# the different trading strategies later on.

SP500_data$Index <- 1
for (i in 2:nrow(SP500_data)) {
  SP500_data$Index[i] <- SP500_data$Index[i-1]*(1+SP500_data$Return[i])
}

# 4.2) Analysing the stock data.

# 4.2.1) Calculating moving averages.

# Adding columns with short-term standard moving averages (20 and 50 days) for each list element.
for (j in SMA_periods_st) {
  for (i in 1:length(stock_data)) {
    stock_data[[i]]$new_column <- SMA(stock_data[[i]][2], n = j)
    colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("SMA_", j)
  }
}

# Adding columns with a long-term standard moving average (200 days) for each list element.
for (i in 1:length(stock_data)) {
  stock_data[[i]]$new_column <- SMA(stock_data[[i]][2], n = 200)
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("SMA_", 200)
}

# Adding columns with short-term exponential moving averages (20 and 50 days) for each list element.
for (j in EMA_periods_st) {
  for (i in 1:length(stock_data)) {
    stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = j)
    colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA_", j)
  }
}

# Adding columns with a long-term exponential moving average (200 days) for each list element.
for (i in 1:length(stock_data)) {
  stock_data[[i]]$new_column <- EMA(stock_data[[i]][2], n = 200)
  colnames(stock_data[[i]])[which(names(stock_data[[i]]) == "new_column")] <- paste0("EMA_", 200)
}

# Removing the data of the first two years, since we are not able to calculate all the 
# moving averages in this period.
for (i in 1:length(stock_data)) {
  skip_year <- year(stock_data[[i]]$Date[1]) + 1
  stock_data[[i]] <- stock_data[[i]][year(stock_data[[i]]$Date) > skip_year,]
  rownames(stock_data[[i]]) <- 1:nrow(stock_data[[i]])
}

# Inspecting the first dataframe of the list.
view(stock_data[[1]])

# 4.2.2) Creating different trading strategies.
# The trading strategies are based on the previously computed moving averages.
# The idea is that we invest in one of the chosen companies if a short-term moving average is above
# a long-term moving average or if a simple moving average is above an exponential moving average. 

# Strategy 1: Determining periods in which the 20-day SMA is above the 50-day SMA.
# Adding the column Crossover-20-50-SMA with 0s and 1s. 
# 0s indicate that the 20-day SMA is not above the 50-day SMA, hence we should not invest at that time.
# 1s indicate that the 20-day SMA is above the 50-day SMA, hence we should invest at that time.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-50-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$SMA_20[i] > stock_data[[k]]$SMA_50[i]) {
      stock_data[[k]]$`Crossover-20-50-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-50-SMA`[i] <- 0
    }
  }
}

# Strategy 2: Determining periods in which the 20-day EMA is above the 50-day EMA.
# Adding the column Crossover-20-50-EMA with 0s and 1s. 
# 0s indicate that the 20-day EMA is not above the 50-day EMA, hence we should not invest at that time.
# 1s indicate that the 20-day EMA is above the 50-day EMA, hence we should invest at that time.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-50-EMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_20[i] > stock_data[[k]]$EMA_50[i]) {
      stock_data[[k]]$`Crossover-20-50-EMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-50-EMA`[i] <- 0
    }
  }
}

# Strategy 3: Determining periods in which the 50-day SMA is above the 200-day SMA.
# Adding the column Crossover-50-200-SMA with 0s and 1s. 
# 0s indicate that the 50-day SMA is not above the 200-day SMA, hence we should not invest at that time.
# 1s indicate that the 50-day SMA is above the 200-day SMA, hence we should invest at that time.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-50-200-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$SMA_50[i] > stock_data[[k]]$SMA_200[i]) {
      stock_data[[k]]$`Crossover-50-200-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-50-200-SMA`[i] <- 0
    }
  }
}

# Strategy 4: Determining periods in which the 50-day EMA is above the 200-day EMA.
# Adding the column Crossover-50-200-EMA with 0s and 1s. 
# 0s indicate that the 50-day EMA is not above the 200-day EMA, hence we should not invest at that time.
# 1s indicate that the 50-day EMA is above the 200-day EMA, hence we should invest at that time.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-50-200-EMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_50[i] > stock_data[[k]]$EMA_200[i]) {
      stock_data[[k]]$`Crossover-50-200-EMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-50-200-EMA`[i] <- 0
    }
  }
}

# Strategy 5: Determining periods in which the 20-day EMA is above the 20-day SMA.
# Adding the column Crossover-20-EMA-20-SMA with 0s and 1s. 
# 0s indicate that the 20-day EMA is not above the 20-day SMA, hence we should not invest at that time.
# 1s indicate that the 20-day EMA is above the 20-day SMA, hence we should invest at that time.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$"Crossover-20-EMA-20-SMA" <- 0
  for (i in 1:nrow(stock_data[[k]])) {
    if (stock_data[[k]]$EMA_20[i] > stock_data[[k]]$SMA_20[i]) {
      stock_data[[k]]$`Crossover-20-EMA-20-SMA`[i] <- 1
    }
    else {
      stock_data[[k]]$`Crossover-20-EMA-20-SMA`[i] <- 0
    }
  }
}

# Inspecting the first dataframe in the list.
view(stock_data[[1]])

# 4.2.3) Calculating historic returns of the individual companies.

# Calculating daily returns and adding an index column for each dataframe in the stock data.
for (k in 1:length(stock_data)) {
  stock_data[[k]]$Return <- 0
  stock_data[[k]]$Index <- 1
  for (i in 2:nrow(stock_data[[k]])) {
    stock_data[[k]]$Return[i] <- (stock_data[[k]]$Close[i] - stock_data[[k]]$Close[i-1])/
      stock_data[[k]]$Close[i-1] 
    stock_data[[k]]$Index[i] <- stock_data[[k]]$Index[i-1]*(1+stock_data[[k]]$Return[i])
  }
}

# 4.2.4) Calculating historic returns of the different trading strategies.

# Strategy 1: The 20-day SMA is above the 50-day SMA.
# Calculating daily returns of this strategy between 2010-01-01 and today. This could take a couple of minutes.

# The explenation for the computations of the daily returns of the first strategy can be applied for all the computations
# of the daily returns of the other strategies.
# We calculate for each day in our time period the average return of our chosen companies which fulfill the condition of 
# the analysed trading strategy.

# Creating a new dataframe for this strategy.
SMA_20_50 <- data.frame(Date = as.Date(stock_data[[1]]$Date), "Return" = as.numeric(0), "Index" = as.numeric(1))
# Declaring necessary variables.
return_cum <- 0
amount <- 0
# The first for-loop iterates through the rows of the new dataframe which contain the dates of the analysed time period.
for (k in 1:(nrow(SMA_20_50)-1)) {
  # The second for-loop iterates through the list with the dataframes of the analysed companies.
  for (i in 1:length(stock_data)) {
    # The third for-loop iterates through the rows of the dataframe of company i.
    for (j in 1:nrow(stock_data[[i]])) {
      # The first if function checks if the current date of the first for-loop is contained in the dataframe of the
      # current company of the second for-loop.
      if (stock_data[[i]]$Date[j] == SMA_20_50$Date[k]) {
        # The next if function checks if the value of the Crossover-20-50-SMA column equals 1 for the current date of the
        # first for-loop. This means that we should invest in this company at this time. 
        if (stock_data[[i]]$`Crossover-20-50-SMA`[stock_data[[i]]$Date == SMA_20_50$Date[k]] == 1) {
          # If the conditions of both if functions hold true, the return of the current company at the current date is
          # added to the return_cum variable and the amount variable is increased by one.
          return_cum <- return_cum + stock_data[[i]]$Return[stock_data[[i]]$Date == SMA_20_50$Date[k+1]]
          amount <- amount + 1
        }
      }
    }
  }
  # Finally, the return at the current date of the first for-loop is calculated by dividing the return_cum variable with
  # the amount variable. The result is added to row k of the SMA_20_50 dataframe.
  SMA_20_50$Return[k] <- return_cum/amount
}

# Adding an index column based on the daily returns to the SMA_20_50 dataframe.
for (i in 2:nrow(SMA_20_50)) {
  SMA_20_50$Index[i] <- SMA_20_50$Index[i-1]*(1+SMA_20_50$Return[i])
}

# Strategy 2: The 20-day EMA is above the 50-day EMA.
# Calculating daily returns of this strategy between 2010-01-01 and today. This could take a couple of minutes.
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

# Adding an index column based on the daily returns to the EMA_20_50 dataframe.
for (i in 2:nrow(EMA_20_50)) {
  EMA_20_50$Index[i] <- EMA_20_50$Index[i-1]*(1+EMA_20_50$Return[i])
}

# Strategy 3: The 50-day SMA is above the 200-day SMA.
# Calculating daily returns of this strategy between 2010-01-01 and today. This could take a couple of minutes.
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

# Adding an index column based on the daily returns to the SMA_50_200 dataframe.
for (i in 2:nrow(SMA_50_200)) {
  SMA_50_200$Index[i] <- SMA_50_200$Index[i-1]*(1+SMA_50_200$Return[i])
}

# Strategy 4: The 50-day EMA is above the 200-day EMA.
# Calculating daily returns of this strategy between 2010-01-01 and today. This could take a couple of minutes.
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

# Adding an index column based on the daily returns to the EMA_50_200 dataframe.
for (i in 2:nrow(EMA_50_200)) {
  EMA_50_200$Index[i] <- EMA_50_200$Index[i-1]*(1+EMA_50_200$Return[i])
}

# Strategy 5: The 20-day EMA is above the 20-day SMA.
# Calculating daily returns of this strategy between 2010-01-01 and today. This could take a couple of minutes.
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

# Adding an index column based on the daily returns to the EMA_50_200 dataframe.
for (i in 2:nrow(EMA_20_SMA_20)) {
  EMA_20_SMA_20$Index[i] <- EMA_20_SMA_20$Index[i-1]*(1+EMA_20_SMA_20$Return[i])
}

# 4.2.5) Creating a dataframe with the index columns of the S&P500, the analysed stocks and the trading strategies.
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

# Inspecting the dataframe with the index columns.
view(results)


# 5.) VISUALIZATION ----------------------------------------------------------------------------------------------------------

# Visualizing the performance of the different strategies in comparison with the S&P500 and the analysed stocks.
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

# The diagram shows that for this time period and these three companies the SMA_50_200 as well as the EMA_50_200 strategies
# perform very well and have both similar results. In comparison to the S&P500, but most importantly also in comparison to the
# individual stocks, these two trading strategies have a better performance.
# If you have a close look at the lines representing the trading strategies, you can see that there are many small horizontal
# parts, meaning that there are many days, at which the conditions of the trading strategies are not fulfilled. This leads to
# a rather consistent increase and was especially helpful during the Covid pandemic because in this period the trading 
# strategies clearly outperformed the S&P500 and the individual stocks.