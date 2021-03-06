anthrCases <- function(resMethod, nsizes){
 UseMethod("anthrCases")
}

anthrCases.default <- function(resMethod, nsizes){
 cases <- resMethod$cases
 #class(cases) <- "anthrCases"
 structure(cases, class = c("anthrCases", "list"))
 return(cases)
} 

anthrCases.trimowa <- function(resMethod, nsizes){
  
 if (nsizes == 1){
   cases <- c()
   cases <- resMethod$cases
 }else{
   cases <- list()
   for (i in 1 : nsizes){
     cases[[i]] <- resMethod[[i]]$cases
   }
 }  

 #class(cases) <- "anthrCases"
 structure(cases, class = c("anthrCases", "list"))
 return(cases)
} 

anthrCases.hipamAnthropom <- function(resMethod, nsizes){
 cases <- list()
 for (i in 1 : nsizes){       
  aux <- table(resMethod[[i]]$clustering)
  aux <- as.numeric(aux)
  auxBig <- which(aux > 2)
  #length(unique(rownames(resMethod[[i]]$cases))) must match with length(attr(table(resMethod[[i]]$clustering), "names"))
  cases[[i]] <- rownames(unique(resMethod[[i]]$cases))[auxBig]
 }   
  
 #class(cases) <- "anthrCases"
 structure(cases, class = c("anthrCases", "list"))
 return(cases)
}
