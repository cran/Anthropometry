stepArchetypoids <- function(i,nearest,data,ArchObj){
  
  N = dim(data)[1]
  
  ai <- archetypes::bestModel(ArchObj[[i]])
 
  if(is.null(archetypes::parameters(ai))){
    stop("No archetypes computed")  
  }else{
    ras <- rbind(archetypes::parameters(ai),data)
    dras <- dist(ras, method = "euclidean", diag = F, upper = T, p = 2)
    mdras <- as.matrix(dras)
    diag(mdras) = 1e+11
  }
  
  if(nearest){
    ini_arch <- sapply(1:i,indivNearest,i,mdras) 
    
    if( all(ini_arch > i) == FALSE){
      k=1
      neig <- knn(data, archetypes::parameters(ai), 1:N, k=k)
      indices1 <- attr(neig, "nn.index")
      ini_arch <- indices1[,k]
      
      while(any(duplicated(ini_arch))){
        k=k+1  
        neig <- knn(data, archetypes::parameters(ai), 1:N, k=k)
        indicesk <- attr(neig, "nn.index")
        
        dupl <- anyDuplicated(indices1[,1])
        ini_arch <- c(indices1[-dupl,1],indicesk[dupl,k])
      }
    }
    
  }else{
    ini_arch <- apply(coef(ai, "alphas"), 2, which.max) 
  }
  
  res <- archetypoids(i,data,huge=200,step=TRUE,init=ini_arch)
  cat("Done!") 
  return(list(archet = res[[1]], rss = res[[2]], archet_ini = ini_arch, alphas = res[[4]]))
}