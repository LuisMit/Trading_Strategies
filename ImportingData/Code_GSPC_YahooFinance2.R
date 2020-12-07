library(quantmod)
getSymbols("^GSPC")
chartSeries(GSPC)
View(GSPC)
