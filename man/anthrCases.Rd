\name{anthrCases}
\alias{anthrCases}
\alias{anthrCases.default}
\alias{anthrCases.trimowa}
\alias{anthrCases.hipamAnthropom}
\title{
Helper generic function for obtaining the anthropometric cases
}
\description{
Because the goal of the methodologies included in this package is always to estimate a number of anthropometric cases given a data set (both central (prototypes) and boundaries (archetypoids)), this auxiliary generic function allows the user to identify the cases computed by each method in an easy way.
}
\usage{
anthrCases(resMethod, nsizes)
\method{anthrCases}{trimowa} (resMethod, nsizes)
\method{anthrCases}{hipamAnthropom} (resMethod, nsizes)
}
\arguments{
\item{resMethod}{
This is the object which saves the results obtained by the methodologies and which contains the anthropometric cases to return.
}
\item{nsizes}{
Number of bust sizes. This argument is needed for the "trimowa" and "hipamAnthropom" methodologies because they can compute the prototypes for any given number of bust sizes.
}
}
\value{
A vector of class \code{anthrCases} with the anthropometric cases.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{TDDclust}}, \code{\link{hipamAnthropom}}, \code{\link{LloydShapes}}, \code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}, \code{\link{archetypoids}}, \code{\link{stepArchetypoids}}
}
\examples{
#kmeansProcrustes:
landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
dim(landmarksNoNa) 
#[1] 574 198 
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
#[1] 66
#As a toy example, only the first 10 individuals are used.
landmarksNoNa_First10 <- landmarksNoNa[1:10, ] 
(numIndiv <- dim(landmarksNoNa_First10)[1])
#[1] 10         
    
array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First10)
#shapes::plotshapes(array3D[,,1]) 
#calibrate::textxy(array3D[,1,1], array3D[,2,1], labs = 1:numLandmarks, cex = 0.7) 

numClust <- 2 ; algSteps <- 1 ; niter <- 1 ; stopCr <- 0.0001
resLL <- LloydShapes(array3D, numClust, algSteps, niter, stopCr, FALSE, FALSE)

prototypes <- anthrCases(resLL)                                  
}
\keyword{math}
