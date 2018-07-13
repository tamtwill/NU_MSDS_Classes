setwd('~/NorthwesternU_MSPA/Databases_Predict_420/GrExercises/GrEx2')
dfCust <- read.csv("customers.csv", header = T, sep = ",", stringsAsFactors = F)
dfItems <- read.csv('items.csv', header = T, sep = ",", stringsAsFactors = F)
dfMail <- read.csv('mail.csv', header = T, sep = ",", stringsAsFactors = F)

clist <- dfCust$acctno
mList <- dfMail$acctno
iList <- dfItems$acctno
missingM <- subset(dfMail, !(dfMail$acctno %in% clist))
inM <- subset(dfMail, dfMail$acctno %in% clist)
missingI <- subset(dfItems, !(dfItems$acctno %in% clist))
inI <- subset(dfItems, dfItems$acctno %in% clist)

nodupCust <- unique(dfCust)
nodupMail <- unique(dfMail)
nodupItem <- unique(dfItems)
