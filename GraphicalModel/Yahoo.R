# Elena Farahbakhsh
# The code has been completed but the only problem is that I assumed every mst has just one center. 
 

mycomination <- function(n,c)
{
  factorial(n)/(factorial(c)*factorial(n-c))
}

SimpleReturnFunction <- function(DataMatrix, dict)
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
      for(k in 1:c)
        {
        r1 <- ro(S[j,i,],S[k,i,])
        if (r1<=1 && r1>=-1)
        {
          roMatrix[i,j,k] = ro(S[j,i,],S[k,i,])
        }
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
      for(k in 1:c)
        {
        dMatrix[i,j,k] = sqrt(2*(1-roMatrix[i,j,k]))
      }
    }
  }
  
  
  
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
  
  for (j in 1:totaltime)
  {
    net = graph.adjacency(dMatrix[j,,],mode="undirected",weighted=TRUE,diag=FALSE)
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
    t <- which(maxdist == min(maxdist))
    center <- append(center, dict[t])
    
    #center <- append(center, t)
    #print(which.min(maxdist))
    
    sumcom <- 0
    
    # if (degreevec[[t]] == length(dict)-1){
    #   distancefromstar <- append(distancefromstar, 1)
    # }else{
    #   distancefromstar <- append(distancefromstar, 0)
    # }
    
   
    for (i in 1:c)
    {
      if (degreevec[[i]]>1)
      {
        sumcom <- sumcom + mycomination(degreevec[[i]],2)
        }
    }
    distancefromstar <- append(distancefromstar, mycomination(degreevec[[t]],2)/sumcom)
    
    
    
    # Now er work on dMatrix
    # we should change the dmatrix to be dissimilarity matrix
    
    
    if (j>1)
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
  
  newlist <- list(center,distancefromstar, dMatrix, dista)# dista for distance between clustering
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



