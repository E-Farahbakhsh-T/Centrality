

# For hierarchical Clustering 
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
# https://stackoverflow.com/questions/20343398/how-to-use-hclust-as-function-call-in-r

rm(list = ls())
source("/Users/asanapple/Desktop/Research/GraphicalModel/Yahoo.R") # for calling functions in Yahoo.R
dict <-  c("ABB.ST.Adj",# Industrials,1
           "ALFA.ST.Adj", # Industrials,2
           "ALIV.SDB.ST.Adj", # Consumer Cyclical,3
           "ASSA.B.ST.Adj", # Industrials,4
           "ATCO.A.ST.Adj", # Industrials,5
           "ATCO.B.ST.Adj", # Industrials,6
           "AZN.ST.Adj", # Healthcare,7
           "BOL.ST.Adj", # Basic Materials,8
           "ELUX.B.ST.Adj", # Technology,9
           "ERIC.B.ST.Adj", # Technology,10
           "EVO.ST.Adj", # Consumer Cyclical,11
           "GETI.B.ST.Adj", # Healthcare,12
           "HEXA.B.ST.Adj", # Technology,13
           "HM.B.ST.Adj", # Consumer Cyclical,14
           "INVE.B.ST.Adj", # Financial Services,15
           "NDA.SE.ST.Adj", # Financial Services,16
           "SAND.ST.Adj", # Industrials,17
           "SCA.B.ST.Adj", # Basic Materials,18
           "SEB.A.ST.Adj", # Financial Services,19
           "SECU.B.ST.Adj", # Industrials,20
           "SHB.A.ST.Adj", # Financial Services,21
           "SKA.B.ST.Adj", # Industrials,22
           "SKF.B.ST.Adj", # Industrials,23
           "SWED.A.ST.Adj", # Financial Services,24
           "SWMA.ST.Adj", # Consumer Defensive,25
           "TEL2.B.ST.Adj", # Communication Services,26
           "TELIA.ST.Adj", # Communication Services,27
           "VOLV.B.ST.Adj") # Industrials,28

DataMatrix_dated = read.csv("/Users/asanapple/Desktop/Research/21-05-01/DataMatrixdated")



# changing dates to Date format otherwise it is just string
Dates = DataMatrix_dated[,1]
aDates<-as.factor(Dates)
#str(aDates)
bDates<-as.Date(aDates,format="%Y-%m-%d") #defining what is the desired format of your date
#str(bDates)


DataMatrix = DataMatrix_dated[,2:dim(DataMatrix_dated)[2]]

DataMatrix <- mapply(DataMatrix, FUN =as.numeric)
columnno = dim(DataMatrix)[2]
IndustrialsDatamatrix <-  DataMatrix[, c(1,2,4,5,6,17,20,22,23,28)] # 17
ConsumerCyclicalMatrix <- DataMatrix[, c(3,11,14)] #3
HealthcareMatrix <- DataMatrix[, c(7,12)] #7
BasicMaterialsMatrix <- DataMatrix[, c(8,18)]
TechnologyMatrix <- DataMatrix[, c(9,10,13)] #13
FinancialServicesMatrix <- DataMatrix[, c( 15, 16, 19, 21, 24)] #19
ConsumerDefensiveMatrix <- DataMatrix[, c(25)]
dictFinancialServices<- c(15, 16, 19, 21, 24)


plot(SimpleReturnFunction(FinancialServicesMatrix, dictFinancialServices, 1, 0)[[2]])
data1<- SimpleReturnFunction(IndustrialsDatamatrix, c(1,2,4,5,6,17,20,22,23,28),1,0)
plot(SimpleReturnFunction(ConsumerCyclicalMatrix, c(3,11,14),1,0)[[1]])


sigma <- .3# >= 0
theta = 2 # <= 1
Alldata <- SimpleReturnFunction(DataMatrix, c(1:columnno), theta, sigma)
plot(Alldata[[5]], pch="*")

plot(Alldata[[1]], pch="*")
frequency = table(Alldata[[1]])
plot(frequency)


# I want to plot this 
library(ggplot2)
#What we want to plot(WWWP)


# thsi part is for considering distance between clustering and mymod(i,10)==1 is when we compare every 10 days 
# mymod(i,1)==0 when we consider all the days.

sett<- c()
for(i in 1: 1004)
{
  if (mymod(i,1)==0)
  {
    sett <- append(sett, i)
  }
}
#sett <- sett[1: (length(sett)-1)]

WWWP <- Alldata[[5]]
#database<- cbind(bDates[1:(length(WWWP) - 1)], WWWP )
database<- cbind(bDates[c(sett)], WWWP ) # just for distance bet trees in 30 days


my_DB <- as.data.frame(database)
ggplot(
  my_DB, aes(x = bDates[c(sett)], y = WWWP )) +
  geom_point() +
  geom_line() +
  labs(x = "Date",
       y = " ",
       title = " ",
       subtitle = " ")





plot(Alldata[[5]], pch="*") # type = "b" connected
# plot(Alldata[[6]], pch="*",type = "b") # type = b connected
frequency = table(Alldata[[1]])
plot(frequency)
plot(Alldata)
# If you want to print them
#write.csv(Alldata, file = "/Users/asanapple/Desktop/Research/21-05-01/Logreturncenters.csv")

# if you want you can consider the following as dictionary
# dict <-  c("ABB.ST.Adj",
#            "ALFA.ST.Adj",
#            "ALIV.SDB.ST.Adj",
#            "ASSA.B.ST.Adj",
#            "ATCO.A.ST.Adj",
#            "ATCO.B.ST.Adj",
#            "AZN.ST.Adj",
#            "BOL.ST.Adj",
#            "ELUX.B.ST.Adj",
#            "ERIC.B.ST.Adj",
#            "EVO.ST.Adj",
#            "GETI.B.ST.Adj",
#            "HEXA.B.ST.Adj",
#            "HM.B.ST.Adj",
#            "INVE.B.ST.Adj",
#            "NDA.SE.ST.Adj",
#            "SAND.ST.Adj",
#            "SCA.B.ST.Adj",
#            "SEB.A.ST.Adj",
#            "SECU.B.ST.Adj",
#            "SHB.A.ST.Adj",
#            "SKA.B.ST.Adj",
#            "SKF.B.ST.Adj",
#            "SWED.A.ST.Adj",
#            "SWMA.ST.Adj",
#            "TEL2.B.ST.Adj",
#            "TELIA.ST.Adj",
#            "VOLV.B.ST.Adj")

data1<- SimpleReturnFunction(IndustrialsDatamatrix, c(1,2,4,5,6,17,20,22,23,28))



Date = read.delim("/Users/asanapple/Desktop/Dates.txt.rtf")
