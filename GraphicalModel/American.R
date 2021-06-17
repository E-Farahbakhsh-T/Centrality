
rm(list = ls())
BAC = read.csv("/Users/asanapple/Desktop/Research/America/BAC.csv")
MS = read.csv("/Users/asanapple/Desktop/Research/America/MS.csv")
JPM=read.csv("/Users/asanapple/Desktop/Research/America/JPM.csv")
C = read.csv("/Users/asanapple/Desktop/Research/America/C.csv")
GS = read.csv("/Users/asanapple/Desktop/Research/America/GS.csv")

BAC.Adj = BAC$Adj.Close
MS.Adj = MS$Adj.Close
JPM.Adj = JPM$Adj.Close
C.Adj = C$Adj.Close
GS.Adj = GS$Adj.Close


DataMatrix = cbind(BAC.Adj ,
                   MS.Adj,
                   JPM.Adj,
                   C.Adj,
                   GS.Adj
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

write.csv(DataMatrix, "/Users/asanapple/Desktop/Research/America/DataMatrixAmerican",row.names=F)
