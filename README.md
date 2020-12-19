# Moving Average Trading Strategies

## Description
The code of this project was written in RStudio and can be executed step by step.

The project deals with different trading strategies based on simple and exponential moving averages.
For a selection of stocks contained in the S&P500, short- and long-term moving averages are calculated.
Various trading strategies are then created by being invested in stocks if a certain short-term moving average is above a long-term moving average.
Finally, the strategies are backtested and the performance is illustrated in a diagram.

## Approach
### 1. Set-up
- All relevant packgages are loaded and if necessary installed.
- The time period for which the financial data is imported and analysed is selected.

### 2. Data Import
- A dataset containing symbols of all companies currently included in the S&P500 is imported.
- Price data for the selected time period for each company which was chosen to analyse is imported.
- Price data for the selected time period for the S&P500 is imported.

### 3. Data preparation
- Some columns of the different dataframes are removed or renamed to simplify the data analysis.

### 4. Data analysis
##### 4.1 S&P500
- Short- and long-term simple and exponential moving averages for the S&P500 are calculated and added to the dataframe.
- The performance of the S&P500 and its moving averages are plotted to verify that the moving average calculatins are working fine.
- Daily returns of the S&P500 are calculated and added to the dataframe.
- A price index is created based on the returns of the S&P500 to better compare its performance to the trading strategies later on.
#### 4.2 Stock data
- 

### 5. Data visualization
