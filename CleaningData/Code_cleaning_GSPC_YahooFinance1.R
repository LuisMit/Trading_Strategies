yahoo1 <- read.csv("GSPC_YAhooFinance1.csv")
yahoo2 <- read.csv("GSPC_YahooFinance2.csv")

#ajdclose = close
yahoo1[5] <- NULL
head(yahoo1)

#Renaming (uniform) and remove
colnames(yahoo1)
Yahoo1 <- rename(yahoo1, c("Open"="X.GSPC.open", "High"="X.GSPC.high", "Low"="X.GSPC.low", "Close"="X.GSPC.close", "Volume"="X.GSPC.volume"))
head(Yahoo1)

write.table(Yahoo1, file = "GSPC_YahooFinance_cleaned.csv", sep=",")