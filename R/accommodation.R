accommodation <- function(dataRaw,stand,percAccomm,mahal=TRUE){
  
  if(stand == TRUE){
    data <- scale(dataRaw,center=sapply(dataRaw,mean),scale=sapply(dataRaw,sd))   
  }else{
    data <- dataRaw  
  }
  
  if(percAccomm != 1){
   if(mahal == TRUE){   
    Sx <- cov(data)
    D2 <- mahalanobis(data,colMeans(data), Sx)
    indivYes <- which(D2 <= qchisq(percAccomm, df=dim(data)[2]))
    indivNo <- which(D2 > qchisq(percAccomm, df=dim(data)[2]))
    perc <- (length(indivYes) / dim(data)[1]) * 100
    data <- data[indivYes,]
   }else{
     if(ncol(dataRaw) <= 3){
      appr <- FALSE
     }else{
       appr <- TRUE
     }   
      dt = c()
      for(i in 1 : nrow (data)){
       dt[i] <- depth(data[i,], data, approx=appr) 
      }
     num <- sum(dt == min(dt))
     indivYes <- which(dt != min(dt)) 
     indivNo <- which(dt == min(dt)) 
     perc <- (length(indivYes) / dim(data)[1]) * 100
     data <- data[indivYes,]
   }
  }else{
    data <- data
  }

 if(percAccomm != 1){  
  print(paste("The percentage of accommodation is exactly ", round(perc,2), "%",sep="")) 
 }

 if(percAccomm != 1){  
  return(list(data=data,indivYes=indivYes,indivNo=indivNo))
 }
 else{
  return(data) 
 }  
}