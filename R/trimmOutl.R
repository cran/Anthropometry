trimmOutl <- function(resMethod, oneSize, nsizes){
 UseMethod("trimmOutl")
}

trimmOutl.default <- function(resMethod, oneSize, nsizes){
  discarded <- resMethod$discarded
  class(discarded) <- "trimmOutl"
  return(discarded)
}

trimmOutl.trimowa <- function(resMethod, oneSize, nsizes){
  
  if(oneSize){
    discarded <- resMethod$discarded
  }else{
    discarded <- list()
    for (i in 1 : (nsizes - 1)){
      discarded[[i]] <- resMethod[[i]]$discarded
    } 
  }  
  class(discarded) <- "trimmOutl"
  return(discarded)
}

trimmOutl.hipamAnthropom <- function(resMethod, oneSize, nsizes){
  
  if(oneSize){
    cl_1_2 <- attr(which(unlist(table(resMethod$clustering)) == 1 | unlist(table(resMethod$clustering)) == 2),"names") 
    cl_1_2 <- as.numeric(cl_1_2)
    pos <- which(unlist(resMethod$clustering) %in% cl_1_2)
    discarded <- attr(unlist(resMethod$clustering)[pos],"names")
  }else{ 
    discarded <- list()
    for (i in 1 : (nsizes - 1)){ 
      cl_1_2 <- attr(which(unlist(table(resMethod[[i]]$clustering)) == 1 | unlist(table(resMethod[[i]]$clustering)) == 2),"names") 
      cl_1_2 <- as.numeric(cl_1_2)
      pos <- which(unlist(resMethod[[i]]$clustering) %in% cl_1_2)
      discarded[[i]] <- attr(unlist(resMethod[[i]]$clustering)[pos],"names")
    }
  }
  class(discarded) <- "trimmOutl"
  return(discarded)
}  

