install.packages("pdfetch")
library(pdfetch)
dat1<-pdfetch_YAHOO("^GSPC")
View(dat1)
