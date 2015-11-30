trimmOutl <- function(resMethod, nsizes){
 UseMethod("trimmOutl")
}

trimmOutl.default <- function(resMethod, nsizes){
 discarded <- resMethod$discarded
 class(discarded) <- "trimmOutl"
 return(discarded)
}

trimmOutl.trimowa <- function(resMethod, nsizes){
 discarded <- list()
 for (i in 1 : nsizes){
  discarded[[i]] <- resMethod[[i]]$discarded
 } 
    
 class(discarded) <- "trimmOutl"
 return(discarded)
}

trimmOutl.hipamAnthropom <- function(resMethod, nsizes){
 discarded <- list()
 for (i in 1 : nsizes){ 
  cl_1_2 <- attr(which(unlist(table(resMethod[[i]]$clustering)) == 1 | unlist(table(resMethod[[i]]$clustering)) == 2),"names") 
  cl_1_2 <- as.numeric(cl_1_2)
  pos <- which(unlist(resMethod[[i]]$clustering) %in% cl_1_2)
  discarded[[i]] <- attr(unlist(resMethod[[i]]$clustering)[pos],"names")
 }
  
 class(discarded) <- "trimmOutl"
 return(discarded)
}  

