anthrCases <- function(resMethod, oneSize, nsizes){
 UseMethod("anthrCases")
}

anthrCases.default <- function(resMethod, oneSize, nsizes){
 cases <- resMethod$cases
 class(cases) <- "anthrCases"
 return(cases)
} 

anthrCases.trimowa <- function(resMethod, oneSize, nsizes){
  
  if(oneSize){
    cases <- resMethod$cases
  }else{
    cases <- list()
    for (i in 1 : (nsizes - 1)){
      cases[[i]] <- resMethod[[i]]$cases
    }
  }
  class(cases) <- "anthrCases"
  return(cases)
} 

anthrCases.hipamAnthropom <- function(resMethod, oneSize, nsizes){
  
  if(oneSize){
    aux <- table(resMethod$clustering)
    aux <- as.numeric(aux)
    auxBig <- which(aux > 2)
    cases <- rownames(resMethod$cases)[auxBig]
  }else{
    cases <- list()
    for (i in 1 : (nsizes - 1)){       
      aux <- table(resMethod[[i]]$clustering)
      aux <- as.numeric(aux)
      auxBig <- which(aux > 2)
      cases[[i]] <- rownames(resMethod[[i]]$cases)[auxBig]
    }   
  } 
  class(cases) <- "anthrCases"
  return(cases)
}
