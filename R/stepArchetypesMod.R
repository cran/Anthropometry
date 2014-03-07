stepArchetypesMod <- function(data,k,nrep=3,verbose=TRUE){
  
  mycall <- match.call()
  as <- list()
  for (i in 1:length(k)) {
    as[[i]] <- list()
    class(as[[i]]) <- "repArchetypes"
    for (j in seq_len(nrep)) {
      if (verbose) 
       cat("\n*** k=", k[i], ", rep=", j, ":\n", sep = "")
       as[[i]][[j]] <- archetypes(data, k = k[i], family = archetypesFamily("original",scalefn = no.scalefn,
                                                                            rescalefn = no.rescalefn))
    }
  }
  return(structure(as, class='stepArchetypes',call=mycall))
}