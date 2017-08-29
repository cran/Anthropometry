\name{projShapes}
\alias{projShapes}
\title{
Helper function for plotting the shapes
}
\description{
Helper function for plotting the projections of the shapes. It displays the projection on the xy plane of the recorded points and mean shape for a given cluster. To that end, first it is needed to carry out a generalized Procrustes analysis in the cluster to obtain the full Procrustes rotated data.
}
\usage{
projShapes(clust,array3D,asig,prototypes)
}
\arguments{
\item{clust}{
Cluster for which represent its mean shape together with the recorded points.
}
\item{array3D}{
Array with the 3D landmarks of the sample points. Each row corresponds to an observation, and each column corresponds to a dimension (x,y,z).
}
\item{asig}{
Clustering optimal results.
}
\item{prototypes}{
Vector of optimal prototypes.
}
}
\value{
Numerical vector with the percentile values of an archetypoid. 
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
dim(landmarksNoNa) 
#[1] 574 198 
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
#[1] 66
#As a toy example, only the first 15 individuals are used.
landmarksNoNa_First10 <- landmarksNoNa[1:10, ] 
(numIndiv <- dim(landmarksNoNa_First10)[1])
#[1] 10         
    
array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First10)
#shapes::plotshapes(array3D[,,1]) 
#calibrate::textxy(array3D[,1,1], array3D[,2,1], labs = 1:numLandmarks, cex = 0.7) 

numClust <- 2 ; algSteps <- 1 ; niter <- 1 ; stopCr <- 0.0001
resLL <- LloydShapes(array3D, numClust, algSteps, niter, stopCr, FALSE, FALSE)
clust_kmeansProc <- resLL$asig

prototypes <- anthrCases(resLL)

projShapes(1, array3D, clust_kmeansProc, prototypes)
#legend("topleft", c("Registrated data", "Mean shape"), pch = 1, col = 1:2, text.col = 1:2)
#title("Procrustes registrated data for cluster 1 \n with its mean shape superimposed", 
#      sub = "Plane xy")
}
\keyword{math}
