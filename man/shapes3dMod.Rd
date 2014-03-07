\name{shapes3dMod}
\alias{shapes3dMod}
\title{
3D shapes plot
}
\description{
This function is a slight modification of the original \code{\link{shapes3d}} function of the \pkg{shapes} R package so that the resulting plot has customized title and axes. Specifically, the changing lines regarding the original function are those related to its argument \emph{axes3} when it is fixed to TRUE.
}
\usage{
shapes3dMod(x,loop=0,type="p",color=2,joinline=c(1:1),axes3=FALSE,rglopen=TRUE,main=main)
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
#CLUSTERING WOMEN ACCORDING TO THEIR SHAPE:
landmarks1 <- na.exclude(landmarks)
dim(landmarks1) 
#[1] 574 198 
(num.points <- (dim(landmarks1)[2]) / 3) 
#[1] 66
landmarks2 <- landmarks1[1:50,] #In the interests of simplicity of the computation involved.
(n <- dim(landmarks2)[1]) 
#[1] 100         
    
dg <- array(0,dim = c(num.points,3,n))
for(k in 1:n){            
 for(l in 1:3){            
  dg[,l,k] <-  as.matrix(as.vector(landmarks2[k,][seq(l,
                     dim(landmarks2)[2]+(l-1),by=3)]),ncol=1,byrow=T)
 }
}
 
Nclusters <- 3 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
resLL <- LloydShapes(dg,Nclusters,Nsteps,niter,stopCr,FALSE,TRUE)
copt <- resLL$copt
shapes3dMod(copt[,,1], loop = 0, type = "p", color = 2, joinline = c(1:1), 
            axes3 = TRUE, rglopen = TRUE, main = "Mean shape cluster 1")
}
}
\keyword{multivariate}
