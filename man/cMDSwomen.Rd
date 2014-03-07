\name{cMDSwomen}
\alias{cMDSwomen}
\docType{data}
\title{
Description of the dissimilarities between women's trunks
}
\description{
Unlike archetypes, archetypoids can be computed when features are unavailable. Given a dissimilarity matrix, the classical multidimensional scaling (cMDS) can be applied to obtain a description of the dissimilarities. 

In Vinue et al. (2014) (submitted), the dissimilarity matrix represents the dissimilarities between women's trunks. After applying the cMDS, the database described here is obtained. Then, the archetypoid algorithm can be applied to this database, see section \emph{examples}.
}
\usage{cMDSwomen}
\format{A matrix with 470 rows and 4 columns.}
\source{Anthropometric survey of the Spanish female population.}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. Submitted for publication.

Alemany, S., Gonzalez, J. C., Nacher, B., Soriano, C., Arnaiz, C., and Heras, H., (2010). Anthropometric survey of the Spanish female population aimed at the apparel industry. \emph{Proceedings of the 2010 Intl. Conference on 3D Body scanning Technologies}, 307--315.
}
\examples{
\dontrun{
#Database:
X <- cMDSwomen
X <- as.matrix(X) 

#Computation of archetypes and archetypoids:
#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 20
lass <- stepArchetypesMod(data=X,k=1:numArch,verbose=FALSE,nrep=nrep)
screeplot(lass)

rss_lass <- matrix(0,nrow=numArch,ncol=nrep)
for(i in 1:numArch){
  for(j in 1:nrep){
    rss_lass[i,j] <- lass[[i]][[j]]$rss
  }
}
(rss_lass_def <- apply(rss_lass,1,min,na.rm=T))

for(i in 1:numArch){
  temp <-  stepArchetypoids(i,TRUE,X,lass)
  filename <- paste("res",i,sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename,".RData",sep="")) 
}  

for(i in 1:numArch){
  temp <-  stepArchetypoids(i,FALSE,X,lass)
  filename <- paste("res",i,"_which",sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename,".RData",sep=""))
} 
}
}
\keyword{datasets}
