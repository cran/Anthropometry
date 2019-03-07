\name{trimmedLloydShapes}
\alias{trimmedLloydShapes}
\title{
Trimmed Lloyd k-means for 3D shapes
}
\description{
The basic foundation of k-means is that the sample mean is the value that minimizes the Euclidean distance from each point, to the centroid of the cluster to which it belongs. Two fundamental concepts of the statistical shape analysis are the Procrustes mean and the Procrustes distance. Therefore, by integrating the Procrustes mean and the Procrustes distance we can use k-means in the shape analysis context. 

The k-means method has been proposed by several scientists in different forms. In computer science and pattern recognition the k-means algorithm is often termed the Lloyd algorithm (see Lloyd (1982)). 

This function is proposed to incorporate a modification to \code{\link{LloydShapes}} in order to make the k-means algorithm robust. Robustness is a property very desirable in a lot of applications. As it is well known, the results of the k-means algorithm can be influenced by outliers and extreme data, or bridging points between clusters. Garcia-Escudero et al. (1999) propose a way of making k-means more robust, which combines the k-means idea with an impartial trimming procedure: a proportion alpha (between 0 and 1) of observations are trimmed (the trimmed observations are self-determined by the data). See also \code{\link{trimmedoid}}.

Note that in the generic name of the k-means algorithm, k refers to the number of clusters to search for. To be more specific in the R code, k is referred to as \code{numClust}, see next section \emph{arguments}.
}
\usage{
trimmedLloydShapes(array3D,n,alpha,numClust,algSteps=10,niter=10,
                   stopCr=0.0001,verbose)
}
\arguments{
\item{array3D}{
Array with the 3D landmarks of the sample objects. Each row corresponds to an observation, and each column corresponds to a dimension (x,y,z).
}
\item{n}{
Number of individuals.
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{numClust}{
Number of clusters.
}
\item{algSteps}{
Number of steps per initialization. Default value is 10.
}
\item{niter}{
Number of random initializations (iterations). Default value is 10.
} 
\item{stopCr}{
Relative stopping criteria. Default value is 0.0001.
}
\item{verbose}{
A logical specifying whether to provide descriptive output about the running process.
}
}
\value{
A list with the following elements:

\emph{asig}: Optimal clustering.

\emph{cases}: Anthropometric cases (optimal centers).

\emph{vopt}: Optimal objective function.

\emph{trimmWomen}: List to save the trimmed individual of each iteration.

\emph{trimmsIter}: Vector with the number of iterations where the optimum was reached. The last number different from NA refers to the last iteration where the final optimum was reached.

\emph{bestNstep}: Nstep of the iteration where the optimum has reached.

\emph{initials}: Random initial values used in each iteration. These values can be used by \code{\link{HartiganShapes}}.

\emph{discarded}: Discarded (trimmed) observations.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2016). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification} \bold{10(1)}, 103--132.

Lloyd, S. P., (1982). Least Squares Quantization in PCM, \emph{IEEE Transactions on Information Theory} \bold{28}, 129--137.

Dryden, I. L., and Mardia, K. V., (1998). \emph{Statistical Shape Analysis}, Wiley, Chichester.

Garcia-Escudero, L. A., Gordaliza, A., and Matran, C., (2003). Trimming tools in exploratory data analysis, \emph{Journal of Computational and Graphical Statistics} \bold{12(2)}, 434--449.

Garcia-Escudero, L. A., and Gordaliza, A., (1999). Robustness properties of k-means and trimmed k-means, \emph{Journal of the American Statistical Association} \bold{94(447)}, 956--969.
}
\note{
We note that adding a trimmed procedure to the Lloyd algorithm is very direct and easy, while for the Hartigan-Wong algorithm, more modifications of the algorithm are needed, which makes the implementation of its trimmed version difficult.
}
\author{
Amelia Simo
}
\seealso{
\code{\link{LloydShapes}}, \code{\link{trimmedoid}}
}
\examples{
#CLUSTERING INDIVIDUALS ACCORDING TO THEIR SHAPE:
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

numClust <- 2 ; alpha <- 0.01 ; algSteps <- 1 ; niter <- 1 ; stopCr <- 0.0001
suppressWarnings(RNGversion("3.5.0"))
set.seed(2013)
res <- trimmedLloydShapes(array3D, numIndiv, alpha, numClust, 
                          algSteps, niter, stopCr, FALSE)

#Optimal partition and prototypes:
clust <- res$asig 
table(clust)
prototypes <- anthrCases(res)

#Trimmed individuals:
trimmed <- trimmOutl(res)
}
\keyword{array}
