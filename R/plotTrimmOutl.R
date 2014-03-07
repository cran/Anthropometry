plotTrimmOutl <- function(x,rows,nsizes,bustVariable,variable,color,xlim,ylim,title){

 if(variable == "chest"){
  plot(x[, bustVariable], x[, variable], pch = "*", col = "thistle1", xlab = bustVariable, ylab = variable, 
       main = title, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
  axis(1, at = seq(xlim[1], xlim[2], 10), labels = seq(xlim[1], xlim[2], 10))
  axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10))

  for(i in 1 : (nsizes-1)){
   points(x[as.character(rows[[i]]), bustVariable], x[as.character(rows[[i]]), variable], pch = i, 
          col = color[i])
  }
 }

 if(variable == "hip"){
  plot(x[, bustVariable], x[, variable], pch = "*", col = "thistle1", xlab = bustVariable, ylab = variable, 
       main = title, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
  axis(1, at = seq(xlim[1], xlim[2], 10), labels = seq(xlim[1], xlim[2], 10))
  axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10))

  for(i in 1 : (nsizes-1)){
   points(x[as.character(rows[[i]]), bustVariable], x[as.character(rows[[i]]), variable], pch = i, 
          col = color[i])
  }
 }

 if(variable == "necktoground"){
  plot(x[, bustVariable], x[, variable], pch = "*", col = "thistle1", xlab = bustVariable, ylab = variable, 
       main = title, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
  axis(1, at = seq(xlim[1], xlim[2], 10), labels = seq(xlim[1], xlim[2], 10))
  axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10))

  for(i in 1 : (nsizes-1)){
   points(x[as.character(rows[[i]]), bustVariable], x[as.character(rows[[i]]), variable], pch = i, 
          col = color[i])
  }
 }

 if(variable == "waist"){
  plot(x[, bustVariable], x[, variable], pch = "*", col = "thistle1", xlab = bustVariable, ylab = variable, 
       main = title, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
  axis(1, at = seq(xlim[1], xlim[2], 10), labels = seq(xlim[1], xlim[2], 10))
  axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10))

  for(i in 1 : (nsizes-1)){
   points(x[as.character(rows[[i]]), bustVariable], x[as.character(rows[[i]]), variable], pch = i, 
          col = color[i])
  }
 }

 if((variable != "chest") & (variable != "hip") & (variable != "necktoground") & (variable != "waist")){ 
   cat("This variable doesn't belong to the database")
 }

}
