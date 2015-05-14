trimmOutl <- function(method, resMethod, oneSize, nsizes){
 if(method == "trimowa"){
    
 if(oneSize){
  trimmed <- c()
  trimmed <- resMethod$trimm
 }else{
   trimmed <- list()
   for (i in 1 : (nsizes - 1)){
    trimmed[[i]] <- resMethod[[i]]$trimm
   } 
  }  
  return(trimmed) 
 
 }else if(method == "TDDclust"){
   trimmed <- resMethod$indivTrimmed
   return(trimmed)
   
  }else if(method == "HipamAnthropom"){
    
    if(oneSize){
     outliers <- c()
     cl_1_2 <- attr(which(unlist(table(resMethod$clustering)) == 1 | unlist(table(resMethod$clustering)) == 2),"names") 
     cl_1_2 <- as.numeric(cl_1_2)
     pos <- which(unlist(resMethod$clustering) %in% cl_1_2)
     outliers <- attr(unlist(resMethod$clustering)[pos],"names")
    }else{ 
      outliers <- list()
      for (i in 1 : (nsizes - 1)){ 
       cl_1_2 <- attr(which(unlist(table(resMethod[[i]]$clustering)) == 1 | unlist(table(resMethod[[i]]$clustering)) == 2),"names") 
       cl_1_2 <- as.numeric(cl_1_2)
       pos <- which(unlist(resMethod[[i]]$clustering) %in% cl_1_2)
       outliers[[i]] <- attr(unlist(resMethod[[i]]$clustering)[pos],"names")
     }
    }
    return(outliers)
   }else if(method == "kmeansProcrustes"){
     opt_iter <- resMethod$trimmsIter[length(resMethod$trimmsIter)]
     trimmed <- resMethod$trimmWomen[[opt_iter]][[resMethod$betterNstep]]
       
     return(trimmed)         
   }else{
     stop("method must be trimowa, TDDclust,HipamAnthropom or kmeansProcrustes")
   }
}