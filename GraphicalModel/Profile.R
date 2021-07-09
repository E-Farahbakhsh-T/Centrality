

# For hierarchical Clustering 
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
# https://stackoverflow.com/questions/20343398/how-to-use-hclust-as-function-call-in-r

rm(list = ls())
source("Yahoo.R") # for calling functions in Yahoo.R
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

DataMatrix = read.csv("/Users/asanapple/Desktop/Research/21-05-01/DataMatrix")
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


plot(SimpleReturnFunction(FinancialServicesMatrix, dictFinancialServices)[[2]])
data1<- SimpleReturnFunction(IndustrialsDatamatrix, c(1,2,4,5,6,17,20,22,23,28))
plot(SimpleReturnFunction(ConsumerCyclicalMatrix, c(3,11,14))[[1]])

Alldata <- SimpleReturnFunction(DataMatrix, c(1:columnno))
plot(Alldata[[4]], pch="*",type = "b") # type = b connected
frequency = table(Alldata)
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
data1<- SimpleReturnFunction(FinancialServicesMatrix, c( 15, 16, 19, 21, 24))[[1]]
data <- SimpleReturnFunction(DataMatrix, c(1:columnno))
plot(data1)
frequency = table(data1)
plot(frequency)
plot(Alldata)



plot(data1[[4]], pch="+")

data1<- SimpleReturnFunction(IndustrialsDatamatrix, c(1,2,4,5,6,17,20,22,23,28))


