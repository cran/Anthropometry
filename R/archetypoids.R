archetypoids <- function(i,data,huge=200,step,init,ArchObj,nearest,sequ,aux){
  
 if(!step){ 
  N = dim(data)[1]
  
  if(sequ){
    ai <- archetypes::bestModel(ArchObj[[i]])
  }else{
    ai <- archetypes::bestModel(ArchObj[[i-aux]])  
  }
  
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
 }else{
   ini_arch <- init
  } 
 
   n <- ncol(t(data))
   x_gvv <- rbind(t(data), rep(huge, n))
  
   zs <- x_gvv[,ini_arch] 
   zs <- as.matrix(zs)
  
   alphas <- matrix(0, nrow = i, ncol = n)
   for (j in 1 : n){
    alphas[, j] = coef(nnls(zs, x_gvv[,j]))
   }
  
   resid <- zs %*% alphas - x_gvv
   rss_ini <- max(svd(resid)$d) / n
  
  res_def <- swap(ini_arch, rss_ini, huge, i, x_gvv, n)
  
  return(res_def)
}