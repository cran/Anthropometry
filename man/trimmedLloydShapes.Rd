\name{trimmedLloydShapes}
\alias{trimmedLloydShapes}
\title{
Trimmed Lloyd k-means for 3D shapes
}
\description{
The basic foundation of k-means is that the sample mean is the value that minimizes the Euclidean distance from each point, to the centroid of the cluster to which it belongs. Two fundamental concepts of the statistical shape analysis are the Procrustes mean and the Procrustes distance. Therefore, by integrating the Procrustes mean and the Procrustes distance we can use k-means in the shape analysis context. 

The k-means method has been proposed by several scientists in different forms. In computer science and pattern recognition the k-means algorithm is often termed the Lloyd algorithm (see Lloyd (1982)). 

This function is proposed to incorporate a modification to \code{\link{LloydShapes}} in order to make the k-means algorithm robust. Robustness is a property very desirable in a lot of applications. As it is well known, the results of the k-means algorithm can be influenced by outliers and extreme data, or bridging points between clusters. Garcia-Escudero et al. (1999) propose a way of making k-means more robust, which combines the k-means idea with an impartial trimming procedure: a proportion alpha (between 0 and 1) of observations are trimmed (the trimmed observations are self-determined by the data). See also \code{\link{trimmedoid}}.
}
\usage{
trimmedLloydShapes(dg,n,alpha,K,Nsteps=10,niter=10,stopCr=0.0001,print)
}
\arguments{
\item{dg}{
Array with the 3D landmarks of the sample objects. Each row corresponds to an observation, and each column corresponds to a dimension (x,y,z).
}
\item{n}{
Number of individuals.
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{K}{
Number of clusters.
}
\item{Nsteps}{
Number of steps per initialization. Default value is 10.
}
\item{niter}{
Number of random initializations. Default value is 10.
} 
\item{stopCr}{
Relative stopping criteria. Default value is 0.0001.
}
\item{print}{
Logical value. If TRUE, some messages associated with the running process are displayed.
}
}
\value{
A list with the following elements:

\emph{asig}: Optimal clustering.

\emph{copt}: Optimal centers.

\emph{vopt}: Optimal objective function.

\emph{trimmWomen}: List to save the trimmed individual of each iteration.

\emph{trimmsIter}: Vector with the number of iterations where the optimum was reached. The last number different from NA refers to the last iteration where the final optimum was reached.

\emph{betterNstep}: Nstep of the iteration where the optimum has reached.

\emph{initials}: Random initial values used in each iteration. These values can be used by \code{\link{HartiganShapes}}.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design. Submitted for publication.

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
\dontrun{
landmarks1 <- na.exclude(landmarks)
dim(landmarks1) 
#[1] 574 198 
(num.points <- (dim(landmarks1)[2]) / 3) 
#[1] 66
landmarks2 <- landmarks1[1:50,] #In the interests of simplicity of the computation involved.
(n <- dim(landmarks2)[1]) 
#[1] 50         
    
dg <- array(0,dim = c(num.points,3,n))
for(k in 1:n){            
 for(l in 1:3){            
  dg[,l,k] <-  as.matrix(as.vector(landmarks2[k,][seq(l,
                   dim(landmarks2)[2]+(l-1),by=3)]),ncol=1,byrow=T)
 }
}
 
K <- 3 ; alpha <- 0.01 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
res <- trimmedLloydShapes(dg,n,alpha,K,Nsteps,niter,stopCr,TRUE)

#To identify the trimmed women of the optimal iteration:
iter_opt <- res$trimmsIter[length(res$trimmsIter)]
trimm_women <- res$trimmWomen[[iter_opt]][[res$betterNstep]]

#Optimal partition:
table(res$asig)
}
}
\keyword{array}
