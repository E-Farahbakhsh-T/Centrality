#We need to clean the data

rm(list = ls())
 
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
#### 987 column ESSITY.B.ST = read.csv("/Users/asanapple/Desktop/Research/21-05-01/ESSITY.B.ST.csv")
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
ESSITY.B.ST.Adj = ESSITY.B.ST$Adj.Close
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
                   ESSITY.B.ST.Adj ,
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
  DataMatrix=DataMatrix[!is.na(DataMatrix[,i]),]
}

# just the # of rows differs, so we find it again
r = dim(DataMatrix)[1]
#### Now, we have the data as a matrix of numbers

w = 252 # for one year without holidays
totaltime = r - w + 1

S <- array(0, dim=c(c, totaltime, w))
  
for (k in 1:c)
{
  for (i in 1:(totaltime))
  {
    S[k, i, ] = DataMatrix[i:(i+w-1), k]
  }
}



ro <- function(s1,s2) {
   cov(s1,s2)/(sqrt(var(s1)*var(s2)))
}



#Now we have totaltime = r - w + 1 times for each time we consider a graph or matrix at least
roMatrix  <- array(0, dim = c(totaltime, c, c))

for(i in 1:totaltime)
{
  for(j in 1:c)
  {   
    for(k in 1:c){
      roMatrix[i,j,k] = ro(S[j,i,],S[k,i,])
    }
  }
}



## till now we have made a matrix roMatrix such that roMatrix[1,,] indicates the first graph and so on

## Now we construct dMatrix

dMatrix  <- array(0, dim = c(totaltime, c, c))
for(i in 1:totaltime)
{
  for(j in 1:c)
  {   
    for(k in 1:c){
      dMatrix[i,j,k] = sqrt(2*(1-roMatrix[i,j,k]))
    }
  }
}



#Ploting and working with graph from Here
#https://sites.google.com/site/daishizuka/toolkits/sna/weighted-edges
library(igraph)
install.packages(igraph)


# every time we want to find the center of the mst of a graph
center = c()



for (j in 1:totaltime)
{
  net=graph.adjacency(dMatrix[j,,],mode="undirected",weighted=TRUE,diag=FALSE)

  
  # we can plot here but we behöver inte
  #plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
  
  # finding the min spanning tree
  mstnet=minimum.spanning.tree(net)
  
  # plot the mst graph
  #plot.igraph(mstnet,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
  
  #getting the weighted adjucency matrix for the mst
  # this is the upperside of the matrix so we + with its transpose
  mstadj = get.adjacency(mstnet, type = c("upper"), attr = NULL, names = TRUE, edges = TRUE)
  mstadj = mstadj + t(mstadj)
  
  maxdist = c()
  distanceMatrix<- distances(mstnet)
  for (i in 1:c)
  {
    maxdist<- append(maxdist,max(distanceMatrix[i,]))
  }
  center <- append(center,which.min(maxdist))
  #print(which.min(maxdist))
}
  
center



#Just if you see the length of the center is not totaltime because there are some unknown in the data. For ex ABB.ST.Adj[1161]
# Annars the code has been completed but the only problem is that I assumed every mst has just one center. we can check at print(which.min(maxdist)).

