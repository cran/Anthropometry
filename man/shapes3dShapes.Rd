\name{shapes3dShapes}
\alias{shapes3dShapes}
\title{
3D shapes plot
}
\description{
This function is a slight modification of the original \code{\link{shapes3d}} function of the \pkg{shapes} R package so that the resulting plot has customized title and axes. Specifically, the changing lines regarding the original function are those related to its argument \emph{axes3} when it is fixed to TRUE.
}
\usage{
shapes3dShapes(x,loop=0,type="p",color=2,joinline=c(1:1),
               axes3=FALSE,rglopen=TRUE,main=main)
}
\arguments{
\item{x}{
See \code{\link{shapes3d}}.
}
\item{loop}{
See \code{\link{shapes3d}}.
}
\item{type}{
See \code{\link{shapes3d}}.
}
\item{color}{
See \code{\link{shapes3d}}.
}
\item{joinline}{
See \code{\link{shapes3d}}.
}
\item{axes3}{
See \code{\link{shapes3d}}.
}
\item{rglopen}{
See \code{\link{shapes3d}}.
}
\item{main}{
Allows us to give the plot a title if axes3=TRUE.
}
}
\value{
A device with the desired plot.
}
\references{
Dryden, I. L., (2012). \pkg{shapes} package. R Foundation for Statistical Computing, Vienna, Austria. Contributed package.

Dryden, I. L., and Mardia, K. V., (1998). \emph{Statistical Shape Analysis}, Wiley, Chichester.
}
\seealso{
\code{\link{shapes3d}}
}
\examples{
\dontrun{
#CLUSTERING INDIVIDUALS ACCORDING TO THEIR SHAPE:
landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
dim(landmarksNoNa) 
#[1] 574 198 
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
#[1] 66
#In the interests of simplicity of the computation involved:
landmarksNoNa_First50 <- landmarksNoNa[1 : 50, ] 
(numIndiv <- dim(landmarksNoNa_First50)[1])
#[1] 50         
    
array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First50)

numClust <- 3 ; algSteps <- 5 ; niter <- 3 ; stopCr <- 0.0001
set.seed(2013)
resHA <- HartiganShapes(array3D,numClust,algSteps,niter,stopCr,FALSE,FALSE,c(),TRUE)

prototypes <- anthrCases("anthropometry", "kmeansProcrustes", resHA)

shapes3dShapes(prototypes[,,1], loop = 0, type = "p", color = 2, joinline = c(1:1), 
            axes3 = TRUE, rglopen = TRUE, main = "Mean shape cluster 1")
}
}
\keyword{multivariate}
