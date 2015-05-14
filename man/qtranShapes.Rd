\name{qtranShapes}
\alias{qtranShapes}
\title{
Auxiliary qtran subroutine of the Hartigan-Wong k-means for 3D shapes
}
\description{
The Hartigan-Wong version of the k-means algorithm uses two auxiliary algorithms: the optimal transfer stage (optra) and the quick transfer stage (qtran). 

This function is the qtran subroutine adapted to the shape analysis context. It is used within \code{\link{HartiganShapes}}. See Hartigan and Wong (1979) for details of the original k-means algorithm and Amaral et al. (2010) for details about its adaptation to shape analysis.
}
\usage{
qtranShapes(array3D,n,c,ic1,ic2,nc,an1,an2,ncp,d,itran,indx)
}
\arguments{
\item{array3D}{
Array with the 3D landmarks of the sample objects.
}
\item{n}{
Number of sample objects.
}
\item{c}{
Array of centroids.
}
\item{ic1}{
The cluster to each object belongs.
}
\item{ic2}{
This vector is used to remember the cluster which each object is most likely to be transferred to at each step.
}
\item{nc}{
Number of objects in each cluster.
}
\item{an1}{
$an1(l) = nc(l) / (nc(l) - 1), l=1,\ldots,numClust$, where numClust is the number of clusters.
}
\item{an2}{
$an2(l) = nc(l) / (nc(l) + 1), l=1,\ldots,numClust$.
}
\item{ncp}{
In the optimal transfer stage, ncp(l) stores the step at which cluster l is last updated, $l=1,\ldots,numClust$.\cr
In the quick transfer stage, ncp(l) stores the step at which cluster l is last updated plus n, $l=1,\ldots,numClust$.
}
\item{d}{
Vector of distances from each object to every centroid. 
}
\item{itran}{
itran(l) = 1 if cluster l is updated in the quick-transfer stage (0 otherwise), $l=1,\ldots,k$.
}
\item{indx}{
Number of steps since a transfer took place.
}
}
\value{
A list with the following elements:: \emph{c,ic1,ic2,nc,an1,an2,ncp,d,itran,indx,icoun}, updated after the optimal transfer stage. Note that \emph{icoun} counts the steps where a re-allocation took place.
}
\note{
This function belongs to \code{\link{HartiganShapes}} and it is not solely used. That is why there is no section of \emph{examples} in this help page.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Hartigan, J. A., and Wong, M. A., (1979). A K-Means Clustering Algorithm, \emph{Applied Statistics}, 100--108.

Amaral, G. J. A., Dore, L. H., Lessa, R. P., and Stosic, B., (2010). k-Means Algorithm in Statistical Shape Analysis, \emph{Communications in Statistics - Simulation and Computation} \bold{39(5)}, 1016--1026.

Dryden, I. L., and Mardia, K. V., (1998). \emph{Statistical Shape Analysis}, Wiley, Chichester.
}
\note{
This function is based on the optra.m file available from \url{http://people.sc.fsu.edu/~jburkardt/m_src/asa136/asa136.html}.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{HartiganShapes}}
}
\keyword{array}
