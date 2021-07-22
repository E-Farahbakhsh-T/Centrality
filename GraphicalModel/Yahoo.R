# Elena Farahbakhsh
# The code has been completed but the only problem is that I assumed every mst has just one center. 

RelativeClusteringCoefficient<- function(dmatrix, mainMatrix)
{
  triples <- 0 
  triangles <- 0 
  columns<- length(dmatrix[1,])
  for (i in 1:columns)
  {
    ex<- i
    for (j in seq(columns) [-ex])
    {
      if (dmatrix[i,j]>0)
      {
        ex1<- c(i,j)
        for (f in seq(columns)[-ex1])
        {
          if(dmatrix[j,f]>0)
          {
            if(mainMatrix[i,f]<1)
            {
              if (dmatrix[i,f] >0)
              {
                triangles <- triangles +1
              }else
              {
                triples <- triples +1
              }
            }
          }
        }
      }
    }
  }
  if ((triangles+triples) == 0 )
  {
    return(-1)
  }else
  {
    return(triangles/(triangles+triples))
  }
  
  
}

LocalClusteringCoefficient<- function(dmatrix, index) # dmatrix
{
  deg <- 0 
  triangles <- 0 
  set<- c()
  columns<- length(dmatrix[1,])
  i <- index
  for (j in seq(columns) [-i])
  {
    if (dmatrix[i,j]>0)
    {
      deg<- deg + 1
      set <- append(set, j)
    }
  }
  if(deg>1)
  {
    for (s in 1:(length(set)-1))
    {
      for (ts in (s+1):length(set))
      {
        
        if(dmatrix[set[s],set[ts]] > 0)
        {
          triangles <- triangles + 1
        }
      }
    }
    return(triangles/mycomination(deg,2))
  }else
  {
    return(-1)
  }
}

# for a given matrix it finds the ClusteringCoefficient
ClusteringCoefficient<- function(dmatrix) # dmatrix
{
  triples <- 0 
  triangles <- 0 
  columns<- length(dmatrix[1,])
  for (i in 1:columns)
  {
    ex<- i
    for (j in seq(columns) [-ex])
    {
      if (dmatrix[i,j]>0)
      {
        ex1<- c(i,j)
        for (f in seq(columns)[-ex1])
        {
          if(dmatrix[j,f]>0)
          {
            triples <- triples +1
            if(dmatrix[i,f]>0)
            {
              triangles <- triangles +1
            }
          }
        }
      }
    }
  }
  if (triples == 0 )
  {
    return(-1)
  }else
  {
    return(triangles/triples)
  }
}

mycomination <- function(n,c)
{
  if (n<c)
  {return (0)}
  else
  {return(factorial(n)/(factorial(c)*factorial(n-c)))}
}

#Mod(n,m)
mymod <- function(n,m)
{
  return(n- m * (n %/% m))
}

dfunction <- function(s)
{
  #return (sqrt(2*(1-(s))))
  return (1-s^2)
  #return (1-abs(s))
}

ro <- function(s1,s2) 
{
  s <- cov(s1,s2)/(sqrt(var(s1)*var(s2)))
  return(s)
}

findingSMatrix<- function(DataMatrix,w)
{
  r = dim(DataMatrix)[1]
  c = dim(DataMatrix)[2]
  SimpleReturnDataMatrix <- matrix(0, nrow=r-1, ncol=c)
  
  for (i in 1:r-1)
  {
    SimpleReturnDataMatrix[i,] = (DataMatrix[i+1,] - DataMatrix[i,])/DataMatrix[i,]
  }
  r = dim(SimpleReturnDataMatrix)[1]
  
  #### Now, we have the data as a matrix of numbers
  
  totaltime = r - w + 1
  
  S <- array(0, dim=c(c, totaltime, w))
  
  for (k in 1:c)
  {
    for (i in 1:(totaltime))
    {
      S[k, i, ] = SimpleReturnDataMatrix[i:(i+w-1), k] 
    }
  }
  # about simple return and log return look at https://www.youtube.com/watch?v=LpzXmhJe93s
  return(S)
}

findingroMatrix<- function(S, totaltime,c)
{
  #Now we have totaltime = r - w + 1 times for each time we consider a graph or matrix at least
  roMatrix  <- array(0, dim = c(totaltime, c, c))
  for(i in 1:totaltime-1)
  {
    for(j in 1:c)
    {   
      for(k in 1:c)
      {
        r1 <- ro(S[j,i,],S[k,i,])
        # if (r1<=theta1 && r1>=-(theta2))
        {
          roMatrix[i,j,k] = r1
        }
      }
    }
  }
  return (roMatrix)
}

findingdMatrix <- function(sMatrix, totaltime,c, theta)
{
  dMatrix  <- array(0, dim = c(totaltime, c, c))
  for(i in 1:totaltime)
  {
    for(j in 1:c)
    {   
      for(k in 1:c)
      {
        df <- sMatrix[i,j,k]
        if (df < theta)
        {
          dMatrix[i,j,k] = df
        }
      }
    }
  }
  return (dMatrix)
}


# returns 4 items: 
#center,distancefromstar, dMatrix, treedistance, clustering coefficient
SimpleReturnFunction <- function(DataMatrix, dict, theta, sigma)
{
  r = dim(DataMatrix)[1]
  c = dim(DataMatrix)[2]
  w = 252 # for one year without holidays
  totaltime = r - w + 1
  S <- findingSMatrix(DataMatrix, w)

  # roMatrix
  roMatrix <- findingroMatrix(S, totaltime,c)
  
  
  ## till now we have made a matrix roMatrix such that roMatrix[1,,] indicates the first graph and so on
  
  ## Now we construct dMatrix
  # another dmatrix by first choosing those which are larger than sigma 
  
  # another dmatrix
  sMatrix  <- array(1, dim = c(totaltime, c, c))
  for(i in 1:totaltime)
  {
    for(j in 1:c)
    {   
      for(k in 1:c)
      {
        df <- roMatrix[i,j,k]
        if (abs(df) > sigma)
        {
          sMatrix[i,j,k] = dfunction(df)
        }
      }
    }
  }
  
  dMatrix <- findingdMatrix(sMatrix, totaltime,c,theta)
  
  #Ploting and working with graph from Here
  #https://sites.google.com/site/daishizuka/toolkits/sna/weighted-edges
  library(igraph)
  #install.packages(igraph)
  #install.packages('dendextend')
  #install.packages('ape')
  library(dendextend)
  library(ape)
  #install.packages('TreeDist')
  library(TreeDist)
  
  
  
  # every time we want to find the center of the mst of a graph
  center = c()
  diam = c()
  distancefromstar = c()
  fit1 <- hclust(as.dist(dMatrix[1,,]), method = "single", members = NULL)
  dista <- c()
  cc <- c()
  LCC <- c()
  RCC<- c() # relative Clustering Coefficient
  
  for (j in 1:totaltime)
  {
    
    net = graph.adjacency(dMatrix[j,,],mode="undirected",weighted=TRUE,diag=FALSE)
    cc = append(cc,ClusteringCoefficient(dMatrix[j,,]))
    
    
    diam  <-  append(diam, diameter(net))
    
    # we can plot here but we behöver inte
    #plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
    
    # finding the min spanning tree
    mstnet = minimum.spanning.tree(net)
    degreevec <- degree(mstnet)
    
    # plot the mst graph
    #plot.igraph(mstnet,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
    
    #getting the weighted adjucency matrix for the mst
    # this is the upperside of the matrix so we + with its transpose
    #mstadj = get.adjacency(mstnet, type = c("upper"), attr = NULL, names = TRUE, edges = TRUE)
    #mstadj = mstadj + t(mstadj)
    
    maxdist = c()
    distanceMatrix <- distances(mstnet)
    for (i in 1:c)
    {
      maxdist<- append(maxdist, max(distanceMatrix[i,]))
    }
    s <- which(maxdist == min(maxdist))
    t=s[[1]]
    
    LCC<- append(LCC,LocalClusteringCoefficient(dMatrix[j,,],t))
    center <- append(center, dict[t])
    RCC <- append(RCC, RelativeClusteringCoefficient(dMatrix[j,,], sMatrix[j,,]))
    #center <- append(center, t)
    #print(which.min(maxdist))
    
    sumcom <- 0
    
    # if (degreevec[[t]] == length(dict)-1){
    #   distancefromstar <- append(distancefromstar, 1)
    # }else{
    #   distancefromstar <- append(distancefromstar, 0)
    # }
    
    
    for (u in 1:c)
    {
      sumcom <- sumcom + mycomination(degreevec[[u]], 2)
    }
    distancefromstar <- append(distancefromstar, mycomination(degreevec[[t]],2)/sumcom)
    
    
    # Now er work on dMatrix
    # we should change the dmatrix to be dissimilarity matrix
    
    
    if (j>1 && mymod(j,1) == 0) # mymod is for considering distance between trees at each period of time like 10, 20 and 30 days mymod(j,10) == 1
    {
      fit2 <- fit1
      fit1 <- hclust(as.dist(dMatrix[j,,]), method = "single", members = NULL)
      fil1 <- as.phylo(fit1)
      fil2 <- as.phylo(fit2)
      dista<- append(dista, TreeDistance(fil1, fil2))
    }
    
    
  }
  
  
  #plot (distancefromstar)
  write.csv(center, file = "/Users/asanapple/Desktop/Research/21-05-01/Simplereturncenters.csv")
  
  # The code has been completed but the only problem is that I assumed every mst has just one center. we can check at print(which.min(maxdist)).
  
  newlist <- list(center, distancefromstar, dMatrix, dista, cc, LCC, RCC)# dista for distance between clustering
  return(newlist)
}



LogReturnFunction <- function(DataMatrix, dict)
{
  r = dim(DataMatrix)[1]
  c = dim(DataMatrix)[2]
  SimpleReturnDataMatrix <- matrix(0, nrow=r-1, ncol=c)
  
  for (i in 1:r-1)
  {
    SimpleReturnDataMatrix[i,] = (DataMatrix[i+1,]-DataMatrix[i,])/DataMatrix[i,]
  }
  r = dim(SimpleReturnDataMatrix)[1]
  
  
  #### Now, we have the data as a matrix of numbers
  LogReturnDataMatrix = matrix(0, nrow=r, ncol=c)
  for(i in 1:r)
  {
    for(j in 1:c)
    {
      LogReturnDataMatrix <- log(1+SimpleReturnDataMatrix)
    }
    
  }
  
  w = 252 # for one year without holidays
  totaltime = r - w + 1
  
  S <- array(0, dim=c(c, totaltime, w))
  
  for (k in 1:c)
  {
    for (i in 1:(totaltime))
    {
      S[k, i, ] = LogReturnDataMatrix[i:(i+w-1), k] 
    }
  }
  # about simple return and log return look at https://www.youtube.com/watch?v=LpzXmhJe93s
  
  
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
    net = graph.adjacency(dMatrix[j,,],mode="undirected",weighted=TRUE,diag=FALSE)
    
    
    # we can plot here but we behöver inte
    #plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
    
    # finding the min spanning tree
    mstnet = minimum.spanning.tree(net)
    
    # plot the mst graph
    #plot.igraph(mstnet,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(net)$weight)
    
    #getting the weighted adjucency matrix for the mst
    # this is the upperside of the matrix so we + with its transpose
    #mstadj = get.adjacency(mstnet, type = c("upper"), attr = NULL, names = TRUE, edges = TRUE)
    #mstadj = mstadj + t(mstadj)
    
    maxdist = c()
    distanceMatrix <- distances(mstnet)
    for (i in 1:c)
    {
      maxdist<- append(maxdist, max(distanceMatrix[i,]))
    }
    t = which(maxdist == min(maxdist)) # this one finds all the mins
    center <- append(center, dict[[t]])
    #center <- append(center, t)
    #print(which.min(maxdist))
  }
  
  return(center)
  
  
}



SimpleReturnFunctionUnsymmetric <- function(DataMatrix, dict) # is not complete
{
  r = dim(DataMatrix)[1]
  c = dim(DataMatrix)[2]
  SimpleReturnDataMatrix <- matrix(0, nrow=(r-1), ncol=c)
  
  for (i in 1:(r-1))
  {
    SimpleReturnDataMatrix[i,] = (DataMatrix[i+1,] - DataMatrix[i,])/ DataMatrix[i,]
  }
  
  r = dim(SimpleReturnDataMatrix)[1]
  
  #### Now, we have the data as a matrix of numbers
  w = 252 # for one year without holidays
  totaltime = r - w + 1
  
  
  
  S <- array(0, dim=c(c, totaltime, w))
  
  for (k in 1:c)
  {
    for (i in 1:(totaltime))
    {
      S[k, i, ] = SimpleReturnDataMatrix[i:(i+w-1), k] 
    }
  }
  
  CcMatrix <- array(0, dim=c(totaltime,c,c))
  
  for (i in 3: totaltime)
  {
    print(i)
    for (j in 1:c)
    {
      for (k in 1:c)
      {
        summ = 0;
        for (s in (i-2):i)
        {
          summ = summ + S[j,s, ] %*% S[k,s, ]
        }
        CcMatrix[i,j,k] <- (summ/3)
      }
    }
    
  }
  
  
  dMatrix  <- array(0, dim = c(totaltime, c, c))
  for(i in 1:totaltime)
  {
    for(j in 1:c)
    {   
      for(k in 1:c){
        dMatrix[i,j,k] = sqrt(2*(1-CcMatrix[i,j,k]))
      }
    }
  }
  
  
  # every time we want to find the center of the mst of a graph
  center = c()
  distancefromstar = c()
  
  for (j in 3:totaltime)
  {
    net = graph.adjacency(dMatrix[j,,],mode="undirected",weighted=TRUE,diag=FALSE)
    
    
    mstnet = minimum.spanning.tree(net)
    
    
    maxdist = c()
    distanceMatrix <- distances(mstnet)
    for (i in 1:c)
    {
      maxdist<- append(maxdist, max(distanceMatrix[i,]))
    }
    print(maxdist)
    t <- which(maxdist == min(maxdist))
    
    center <- append(center, dict[t])
  }
  
  return(center)
  
}
