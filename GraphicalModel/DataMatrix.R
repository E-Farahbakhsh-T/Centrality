


ABB.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ABB.ST.csv")
ALFA.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ALFA.ST.csv")
ALIV.SDB.ST =read.csv("/Users/asanapple/Desktop/Research/21-05-01/ALIV.SDB.ST.csv")
ASSA.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ASSA.B.ST.csv")
ATCO.A.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ATCO.A.ST.csv")
ATCO.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ATCO.B.ST.csv")
AZN.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/AZN.ST.csv")
BOL.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/BOL.ST.csv")
ELUX.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ELUX.B.ST.csv")
ERIC.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ERIC.B.ST.csv")
#### 987 rows ESSITY.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ESSITY.B.ST.csv")
EVO.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/EVO.ST.csv")
GETI.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/GETI.B.ST.csv")
HEXA.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/HEXA.B.ST.csv")
HM.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/HM.B.ST.csv")
INVE.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/INVE.B.ST.csv")
NDA.SE.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/NDA.SE.ST.csv")
SAND.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SAND.ST.csv")
SCA.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SCA.B.ST.csv")
SEB.A.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SEB.A.ST.csv")
SECU.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SECU.B.ST.csv")
SHB.A.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SHB.A.ST.csv")
SKA.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SKA.B.ST.csv")
SKF.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SKF.B.ST.csv")
SWED.A.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SWED.A.ST.csv")
SWMA.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/SWMA.ST.csv")
TEL2.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/TEL2.B.ST.csv")
TELIA.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/TELIA.ST.csv")
VOLV.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/VOLV.B.ST.csv")
## Examine headers of file (OHLCV type)
names(ABB.ST)
## Extract Adjusted Close
#geAdj = GE$GE.Adjusted["2000-01-01/2000-01-20"]
ABB.ST.Adj = ABB.ST$Adj.Close
ALFA.ST.Adj = ALFA.ST$Adj.Close
ALIV.SDB.ST.Adj = ALIV.SDB.ST$Adj.Close
ASSA.B.ST.Adj = ASSA.B.ST$Adj.Close
ATCO.A.ST.Adj = ATCO.A.ST$Adj.Close
ATCO.B.ST.Adj = ATCO.B.ST$Adj.Close
AZN.ST.Adj = AZN.ST$Adj.Close
BOL.ST.Adj = BOL.ST$Adj.Close
ELUX.B.ST.Adj = ELUX.B.ST$Adj.Close
ERIC.B.ST.Adj = ERIC.B.ST$Adj.Close
#ESSITY.B.ST.Adj = ESSITY.B.ST$Adj.Close
EVO.ST.Adj = EVO.ST$Adj.Close
GETI.B.ST.Adj = GETI.B.ST$Adj.Close
HEXA.B.ST.Adj = HEXA.B.ST$Adj.Close
HM.B.ST.Adj = HM.B.ST$Adj.Close
INVE.B.ST.Adj = INVE.B.ST$Adj.Close
NDA.SE.ST.Adj = NDA.SE.ST$Adj.Close
SAND.ST.Adj = SAND.ST$Adj.Close
SCA.B.ST.Adj = SCA.B.ST$Adj.Close
SEB.A.ST.Adj = SEB.A.ST$Adj.Close
SECU.B.ST.Adj = SECU.B.ST$Adj.Close
SHB.A.ST.Adj = SHB.A.ST$Adj.Close
SKA.B.ST.Adj = SKA.B.ST$Adj.Close
SKF.B.ST.Adj = SKF.B.ST$Adj.Close
SWED.A.ST.Adj = SWED.A.ST$Adj.Close
SWMA.ST.Adj = SWMA.ST$Adj.Close
TEL2.B.ST.Adj = TEL2.B.ST$Adj.Close
TELIA.ST.Adj = TELIA.ST$Adj.Close
VOLV.B.ST.Adj = VOLV.B.ST$Adj.Close


DataMatrix = cbind(ABB.ST.Adj ,
                   ALFA.ST.Adj,
                   ALIV.SDB.ST.Adj,
                   ASSA.B.ST.Adj,
                   ATCO.A.ST.Adj,
                   ATCO.B.ST.Adj,
                   AZN.ST.Adj ,
                   BOL.ST.Adj ,
                   ELUX.B.ST.Adj ,
                   ERIC.B.ST.Adj,
                   EVO.ST.Adj ,
                   GETI.B.ST.Adj ,
                   HEXA.B.ST.Adj ,
                   HM.B.ST.Adj ,
                   INVE.B.ST.Adj ,
                   NDA.SE.ST.Adj ,
                   SAND.ST.Adj ,
                   SCA.B.ST.Adj ,
                   SEB.A.ST.Adj ,
                   SECU.B.ST.Adj ,
                   SHB.A.ST.Adj ,
                   SKA.B.ST.Adj,
                   SKF.B.ST.Adj ,
                   SWED.A.ST.Adj,
                   SWMA.ST.Adj ,
                   TEL2.B.ST.Adj ,
                   TELIA.ST.Adj,
                   VOLV.B.ST.Adj 
)

DataMatrix


r = dim(DataMatrix)[1]
c = dim(DataMatrix)[2]

# the data in the matrix are charachters so we change to numbers
DataMatrix <- mapply(DataMatrix, FUN =as.numeric)
DataMatrix <- matrix(data=DataMatrix, ncol=c, nrow=r)
# in the following for each column we find non numerical and remove the row:
for (i in 1:c)
{
  DataMatrix = DataMatrix[!is.na(DataMatrix[,i]),]
}

write.csv(DataMatrix, "/Users/asanapple/Desktop/Research/21-05-01/DataMatrix",row.names=F)