indivNearest <- function(Indivs,NumArchet,mdras){
 as.numeric(which.min(mdras[Indivs,]) - (NumArchet))
}
