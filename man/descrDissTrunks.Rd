\name{descrDissTrunks}
\alias{descrDissTrunks}
\docType{data}
\title{
Description of the dissimilarities between women's trunks
}
\description{
Unlike archetypes, archetypoids can be computed when features are unavailable. Given a dissimilarity matrix, the classical multidimensional scaling (cMDS) can be applied to obtain a description of the dissimilarities. 

In Vinue et al. (2015), the dissimilarity matrix represents the dissimilarities between women's trunks. After applying the cMDS, the database described here is obtained. Then, the archetypoid algorithm can be applied to this database, see section \emph{examples}.
}
\usage{descrDissTrunks}
\format{A matrix with 470 rows and 4 columns.}
\source{Anthropometric survey of the Spanish female population.}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Alemany, S., Gonzalez, J. C., Nacher, B., Soriano, C., Arnaiz, C., and Heras, H., (2010). Anthropometric survey of the Spanish female population aimed at the apparel industry. \emph{Proceedings of the 2010 Intl. Conference on 3D Body scanning Technologies}, 307--315.
}
\examples{
#Database:
#As a toy example, only the first 25 individuals are used.
X <- descrDissTrunks[1:25,]
X <- as.matrix(X) 

#Computation of archetypes and archetypoids:
#For reproducing results, seed for randomness:
suppressWarnings(RNGversion("3.5.0"))
set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
#This is a toy example. In other situation, choose numArch=10 and numRep=20.
numArch <- 5 ; nrep <- 2
lass <- stepArchetypesRawData(data = X, numArch = 1:numArch, numRep = nrep, verbose = FALSE)
#To understand the warning messages, see the vignette of the
#archetypes package.  

#screeplot(lass)

numArchoid <- 3
res_archoids_ns <- archetypoids(numArchoid, X, huge = 200, step = FALSE, ArchObj = lass, 
                                nearest = "cand_ns", sequ = TRUE)
}
\keyword{datasets}
