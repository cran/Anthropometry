getBestPamsamMO <- function(x,maxsplit,orness = 0.7,type,...){
 
 if(nrow(x) <= maxsplit){
  maxsplit <- max(nrow(x) - 1, 2)
 }
  
 if(maxsplit > 2){
  #Create the pamsams with 2 to maxsplit splits:
  max.asw <- -1
  for (k in 2 : maxsplit){
   xtemp.ps <- pamsam(x, k = k, type = type, DIST = NULL, maxsplit = maxsplit, orness = orness, ...)
   if (xtemp.ps$asw > max.asw){
    max.asw <- xtemp.ps$asw
    x.ps <- xtemp.ps
   }
  }   
 }else{
   x.ps <- pamsam(x, k = 2, type = type, DIST = NULL, maxsplit = maxsplit, orness = orness, ...)
  }
 x.ps
}
