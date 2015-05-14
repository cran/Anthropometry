anthrCases <- function(field, method, resMethod, oneSize, nsizes){
  if(field == "anthropometry"){
    
    if(method == "trimowa"){
      
      if(oneSize){
        centralCases <- c() 
        centralCases <- resMethod$meds
      }else{
        centralCases <- list()
        for (i in 1 : (nsizes - 1)){
          centralCases[[i]] <- resMethod[[i]]$meds
        }
      }  
      return(centralCases)
      
    }else if(method == "TDDclust"){
      centralCases <- as.integer(rownames(resMethod$Y))
      return(centralCases)
      
    }else if(method == "HipamAnthropom"){
      
      if(oneSize){
        centralCases <- c() 
        aux <- table(resMethod$clustering)
        aux <- as.numeric(aux)
        auxBig <- which(aux > 2)
        centralCases <- rownames(resMethod$medoids)[auxBig]
      }else{
        centralCases <- list()
        for (i in 1 : (nsizes - 1)){   	  
          aux <- table(resMethod[[i]]$clustering)
          aux <- as.numeric(aux)
          auxBig <- which(aux > 2)
          centralCases[[i]] <- rownames(resMethod[[i]]$medoids)[auxBig]
        }  
      }  
      return(centralCases)	  
    }else if(method == "kmeansProcrustes"){
      centralCases <- resMethod$copt
      return(centralCases)	       
    }else{
      stop("method must be trimowa, TDDclust, HipamAnthropom or kmeansProcrustes")
    }	
  }else if(field == "ergonomics"){
    boundaryCases <- resMethod$archet
    return(boundaryCases)
  }else{
    stop("field must be anthropometry or ergonomics")
  }	
}