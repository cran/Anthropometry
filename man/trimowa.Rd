\name{trimowa}
\alias{trimowa}
\title{
Trimmed PAM with OWA operators
}
\description{
This is the methodology developed in Ibanez et al. (2012) to define an efficient apparel sizing system based on clustering techniques jointly with OWA operators. In our approach, we apply the trimmed k-medoids algorithm (\code{\link{trimmedoid}}) to the first twelve bust classes according to the sizes defined in the European standard to sizing system. Size designation of clothes. Part 3: Measurements and intervals.
}
\usage{
trimowa(x,w,K,alpha,niter,Ksteps,ahVect=c(23,28,20,25,25))
}
\arguments{
\item{x}{
Data frame. In our approach, this is each one of the subframes originated after segmenting the whole anthropometric Spanish survey in twelve bust segments, according to the European standard to sizing system. Size designation of clothes. Part 3: Measurements and intervals. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{w}{
The aggregation weights of the OWA operators. They are computed with the \code{\link{WeightsMixtureUB}}.
}
\item{K}{
Number of clusters.
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{niter}{
Number of random initializations.
}
\item{Ksteps}{
Steps per initialization.
}
\item{ahVect}{
Constants that define the \code{ah} slopes of the distance function in \code{\link{GetDistMatrix}}. Given the five variables considered, this vector is c(23,28,20,25,25). This vector would be other according to the variables considered.
}
}
\value{
A list with the following elements:

\emph{meds}: Centroids of the clusters. They are the medoids obtained for each bust class.

\emph{numTrim}: Number of trimmed individuals in each bust class.

\emph{numClass}: Number of individuals in each bust class.

\emph{noTrim}: Number of of non-trimmed individuals.

\emph{C1,C2,C3,C4}: Required constant values to define the distance \code{\link{GetDistMatrix}} (\emph{C1} is bh, \emph{C2} is bl, \emph{C3} is ah and \emph{C4} is al).

\emph{asig}: Vector of the clusters to which each individual belongs.

\emph{trimm}: Trimmed individuals.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{dataDemo}}, \code{\link{WeightsMixtureUB}}, \code{\link{GetDistMatrix}}, \code{\link{trimmedoid}}
}
\examples{
\dontrun{
#Loading the data to apply the trimowa algorithm:
dataDef <- dataDemo
num.variables <- dim(dataDef)[2]
bust <- dataDef$bust

orness <- 0.7
w <- WeightsMixtureUB(orness,num.variables)

bustCirc_4 <- seq(74,102,4)  ; bustCirc_6 <- seq(107,131,6)  ; bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
K <- 3 ; alpha <- 0.01 ; niter <- 10 ; Ksteps <- 7

ahVect <- c(23,28,20,25,25)

res_trimowa <- list()
for (i in 1 : (nsizes-1)){ 
  data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  res_trimowa[[i]] <- trimowa(data,w,K,alpha,niter,Ksteps,ahVect=ahVect)
}
}
}
\keyword{array}
