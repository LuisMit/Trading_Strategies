install.packages("GetQuandlData")
library("GetQuandlData")
my_api_key<-"uZfo3VCpZsExxeh8pxoU"

my_symbol <- c('S&P 500 Futures, Continuous Contract' = "CHRIS/CME_SP1")
first_date <- as.Date('1982-04-01')
last_date <- as.Date('2020-12-04')


dat1 <- get_Quandl_series(id_in = my_symbol,
                          api_key = my_api_key,
                          first_date = first_date,
                          last_date = last_date)

View(dat1)
write.table(dat1, file = "GSPC_Quandl.csv", sep=",")
