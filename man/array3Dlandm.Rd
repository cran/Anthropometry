\name{array3Dlandm}
\alias{array3Dlandm}
\title{
Helper function for the 3D landmarks
}
\description{
This is a helper function for obtaining the array with the 3D landmarks of the sample objects
}
\usage{
array3Dlandm(numLandm,numIndiv,matLandm)
}
\arguments{
\item{numLandm}{
Number of landmarks that represent the 3D body of the individuals.
}
\item{numIndiv}{
Number of individuals to analyze.
}
\item{matLandm}{
Matrix with the \code{numLandm} landmarks for the \code{numIndiv} individuals.
}
}
\value{
Array with the 3D landmarks of the sample objects.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2016). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification} \bold{10(1)}, 103--132.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{LloydShapes}}, \code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}
}
\examples{
landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
landmarksNoNa_First50 <- landmarksNoNa[1:50, ]
numIndiv <- dim(landmarksNoNa_First50)[1]

array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First50)
}
\keyword{math}
