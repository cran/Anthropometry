overlappingRows <- function(i, resBicluster) {

 x <- rep(0, nrow(resBicluster@RowxNumber))

 x <- x + i * resBicluster@RowxNumber[, i]

 x
}

