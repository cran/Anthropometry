projShapes <- function(clust, array3D, asig, prototypes){
  out_proc <- c()
  x <- array3D[, , asig == clust]
  
  if (length(dim(x)) != 3) {
    stop("Please ensure that array3D has 3 dimensions.")
  }else{
    out_proc <- shapes::procGPA(x, distances = TRUE, pcaoutput = TRUE)
    shapes::plotshapes(out_proc$rotated)
    points(prototypes[, , clust], col = 2) 
  }
}