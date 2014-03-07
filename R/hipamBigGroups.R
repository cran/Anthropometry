hipamBigGroups <- function(i,hip){
 aux <- table(hip[[i]]$clustering)
 aux <- as.numeric(aux)
 
 auxBig <- which(aux > 2)

 rows <- rownames(hip[[i]]$medoids)[auxBig]

 return(rows)
}
