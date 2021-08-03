
suppressWarnings(if (!require(vars)) {
  install.packages("vars")})
library(vars)

DataMatrix = read.csv("~/Documents/TeachingOrebro/PhD/ElenaFarahbakshTouliPhDSU/data/DataMatrix")
#number of rows and number of columns
t=nrow(DataMatrix)
N=ncol(DataMatrix)
H=12
#compute log-returns
returns=log(DataMatrix[2:t,])-log(DataMatrix[1:(t-1),])
#fit of VAR process using AIC for the choice of order
#FitVARun=VAR(returns[1:255,],type="const",lag.max=5,ic="AIC")
#FitVARp1=VAR(returns[1:255,],type="const",p=1)
FitVARp3=VAR(returns[1:255,],type="const",p=3)
A=NULL
#for VAR with p=3
for(i in 1:N)
{
Arow=as.numeric(FitVARp3$varresult[[i]]$coefficients)
A=rbind(A,Arow[1:(3*N+1)])
}
dim(A)
#separating V, A1, A2,and A3
nu=A[,1]
A1=A[,2:(N+1)]
A2=A[,(N+2):(2*N+1)]
A3=A[,(2*N+2):(3*N+1)]
#building MA process' matrixes 
I=diag(N)
mu=solve(I-A1-A2-A3)%*%nu
#Create an array for 13 matrices of Theta0, Theta1...Theta12 as 3dim object 
Theta=array(0,c(N,N,H))
#compute first 3 matrices
Theta[,,1]=I
Theta[,,2]=A1
Theta[,,3]=A1%*%A1+A2
#compute the rest of the matrices Theta recursively
for(i in 4:H)
{
Theta[,,i]=Theta[,,(i-1)]%*%A1+Theta[,,(i-2)]%*%A2+Theta[,,(i-3)]%*%A3
}  
 
#we need sigma matrix= the covariance matrix of errors  

ResVAR=residuals(FitVARp3)
Sigma=cov(ResVAR) 
#compute DgH matrix (formula 5 Diebold)
DgH=matrix(0,N,N)
for(i in 1:N){
  for(j in 1:N){
    SumNum=0
    SumDen=0
    for(h in 1:H){
      SumNum=SumNum+as.numeric((t(I[,i])%*%Theta[,,h]%*%Sigma%*%I[,j])^2)
      SumDen=SumDen+as.numeric(t(I[,i])%*%Theta[,,h]%*%Sigma%*%t(Theta[,,h])%*%I[,i])
    }
   DgH[i,j]=SumNum/SumDen/as.numeric(Sigma[j,j]) 
  }
}
  