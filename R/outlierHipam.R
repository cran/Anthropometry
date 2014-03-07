outlierHipam <- function(i,hip){
 cl_1_2 <- attr(which(unlist(table(hip[[i]]$clustering)) == 1 | unlist(table(hip[[i]]$clustering)) == 2),"names") 
 cl_1_2 <- as.numeric(cl_1_2)

 pos <- which(unlist(hip[[i]]$clustering) %in% cl_1_2)
 elem <- attr(unlist(hip[[i]]$clustering)[pos],"names")
 return(elem)
}

