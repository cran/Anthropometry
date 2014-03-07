archetypesBoundary <- function(data,numArchet,verbose,nrep){

  ldata <- data 
  #For reproducing results, seed for randomness:
  set.seed(2010) 
  #Run archetypes algorithm repeatedly from 1 to numArchet archetypes:
  lass <- stepArchetypesMod(data = ldata, k = 1:numArchet, verbose = verbose, nrep = nrep) 

  return(lass) 
}











