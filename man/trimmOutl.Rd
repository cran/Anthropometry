\name{trimmOutl}
\alias{trimmOutl}
\alias{trimmOutl.default}
\alias{trimmOutl.trimowa}
\alias{trimmOutl.hipamAnthropom}
\title{
Helper generic function for obtaining the trimmed and outlier observations
}
\description{
The methodologies included in this package which are developed to the clothing design problem take into account that a clothing sizing system is intended to cover only what we could call standard population, leaving out those individuals who are extreme respect to a set of measurements. For "trimowa", "TDDclust and "kmeansProcrustes" (which refers to as \code{\link{trimmedLloydShapes}} in this case) these individuals are called trimmed individuals. For the "hipamAnthropom" methodology these individuals are called outlier individuals.

This auxiliary generic function allows the user to identify the discarded individuals computed by each method in an easy way.
}
\usage{
trimmOutl(resMethod, oneSize, nsizes)
\method{trimmOutl}{trimowa} (resMethod, oneSize, nsizes)
\method{trimmOutl}{hipamAnthropom} (resMethod, oneSize, nsizes)
}
\arguments{
\item{resMethod}{
This is the object which saves the results obtained by the aforementioned methodologies and which contains the discarded individuals to return.
}
\item{oneSize}{
A logical specifying whether to obtain discarded observations for a single size. This argument is needed for the "trimowa" and "hipamAnthropom" methodologies because they can compute the discarded individuals for a single bust size or for the bust sizes defined by the European Normative. If FALSE, the following argument \code{nsizes} is needed.
}
\item{nsizes}{
Number of bust sizes. This argument is needed for the "trimowa" and "hipamAnthropom" methodologies because they can compute the discarded individuals for each bust size defined by the European Normative.
}
}
\value{
A vector of class \code{trimmOutl} with the discarded observations.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{TDDclust}}, \code{\link{hipamAnthropom}}, \code{\link{LloydShapes}}, \code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}
}
\examples{
\dontrun{
#kmeansProcrustes:
landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
landmarksNoNa_First50 <- landmarksNoNa[1 : 50, ]
numIndiv <- dim(landmarksNoNa_First50)[1]

array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First50)

numClust <- 3 ; alpha <- 0.01 ; algSteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
set.seed(2013)
res_kmeansProc <- trimmedLloydShapes(array3D, numIndiv, alpha, numClust, 
                                     algSteps, niter, stopCr, 
                                     verbose = FALSE)
trimmed <- trimmOutl(res_kmeansProc)
}
}
\keyword{math}
