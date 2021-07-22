
source("/Users/asanapple/Desktop/Research/GraphicalModel/Yahoo.R") # for calling functions in Yahoo.R


#install.packages("MTS")
library("MTS")

DSimple <- function(DataMatrix)
{
  w <- 252
  totaltime <- r - w + 1
  r <- dim(DataMatrix)[1]; c <- dim(DataMatrix)[2]
  S <- findingSMatrix(DataMatrix, w)
  rom<- findingroMatrix(S,totaltime, c)
  VARMA(S[,1,], p =0,q=1)
  # VARMA(da, p = 0, q = 0, include.mean = T, 
  #       fixed = NULL, beta=NULL, sebeta=NULL, 
  #       prelim = F, details = F, thres = 2)
  
}

