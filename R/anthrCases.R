anthrCases <- function(resMethod, nsizes){
 UseMethod("anthrCases")
}

anthrCases.default <- function(resMethod, nsizes){
 cases <- resMethod$cases
 class(cases) <- "anthrCases"
 return(cases)
} 

anthrCases.trimowa <- function(resMethod, nsizes){
 cases <- list()
 for (i in 1 : nsizes){
  cases[[i]] <- resMethod[[i]]$cases
 }

 class(cases) <- "anthrCases"
 return(cases)
} 

anthrCases.hipamAnthropom <- function(resMethod, nsizes){
 cases <- list()
 for (i in 1 : nsizes){       
  aux <- table(resMethod[[i]]$clustering)
  aux <- as.numeric(aux)
  auxBig <- which(aux > 2)
  cases[[i]] <- rownames(resMethod[[i]]$cases)[auxBig]
 }   
  
 class(cases) <- "anthrCases"
 return(cases)
}
