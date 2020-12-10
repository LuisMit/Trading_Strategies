library(readr)
quandl <- read.csv("GSPC_Quandl.csv")

#Removing a column with the same information value (id_quandl and series_name)
quandl$id_quandl <- NULL
quandl$series_name <- NULL
head(quandl)

#Renaming Columns
colnames(quandl)
names(quandl)[9] <- "Date"
names(quandl)[8] <- "Previous Day Open Interest"
names(quandl)[6] <- "Close"

write.table(quandl, file = "GSPC_Quandl_cleaned.csv", sep=",")
Quandl <- read.csv("GSPC_Quandl_cleaned.csv")

#uniform with yahoofinance
  #remove certain columns: change, settle, pdo, date -->open, high, low, close, volume
quandl[4:5] <- NULL
quandl[6:7] <- NULL

write.table(quandl, file = "GSPC_Quandl_cleaned2.csv", sep=",")




