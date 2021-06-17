
rm(list = ls())

source("Yahoo.R")
dict <-  c("BAC",# Industrials,1
           "MS", # Industrials,2
           "JPM", # Consumer Cyclical,3
           "C", # Industrials,4
           "GS" # Industrials,5
)

DataMatrix = read.csv("/Users/asanapple/Desktop/Research/America/DataMatrixAmerican")
DataMatrix <- mapply(DataMatrix, FUN =as.numeric)
columnno = dim(DataMatrix)[2]

plot(SimpleReturnFunction(DataMatrix, c(1:columnno)))

Alldata <- SimpleReturnFunction(DataMatrix, c(1:columnno))

frequency = table(Alldata)
plot(frequency)
plot(Alldata)

