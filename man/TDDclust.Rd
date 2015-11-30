\name{TDDclust}
\alias{TDDclust}
\title{
Trimmed clustering based on L1 data depth
}
\description{
This is the trimmed version of the clustering algorithm based on the L1 depth proposed by Rebecka Jornsten (2004).  She segments all the observations in clusters, and assigns to each point z in the data space, the L1 depth value regarding its cluster. A trimmed procedure is incorporated to remove the more extreme individuals of each cluster (those one with the lowest depth values), in line with \code{\link{trimowa}}.
}
\usage{
TDDclust(data,numClust,lambda,Th,niter,T0,simAnn,alpha,data1,verbose=TRUE)
}
\arguments{
\item{data}{
Data frame. Each row corresponds to an observation, and each column corresponds to a variable. All variables must be numeric.
}
\item{numClust}{
Number of clusters. 
}
\item{lambda}{
Tuning parameter that controls the influence the data depth has over the clustering, see Jornsten (2004).
}
\item{Th}{
Threshold for observations to be relocated, usually set to 0.
}
\item{niter}{
Number of random initializations (iterations).
}
\item{T0}{
Simulated annealing parameter. It is the current temperature in the simulated annealing procedure.
}
\item{simAnn}{
Simulated annealing parameter. It is the decay rate, default 0.9. 
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{data1}{
The same data frame as \emph{data}, used to incorporate the trimmed observations into the rest of them for the next iteration. 
}
\item{verbose}{
A logical specifying whether to provide descriptive output about the running process. Default TRUE.
}
}
\value{
A list with the following elements:

\emph{NN}: Cluster assignment, NN[1,] is the final partition. 

\emph{cases}: Anthropometric cases (the multivariate median cluster representatives). 

\emph{DD}: Depth values of the observations (only if there are trimmed observations).

\emph{Cost}: Final value of the optimal partition.

\emph{discarded}: Discarded (trimmed) observations.

\emph{klBest}: Iteration in which the optimal partition was found.
}
\references{
Jornsten R., (2004). Clustering and classification based on the L1 data depth, \emph{Journal of Multivariate Analysis} \bold{90}, 67--89

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
This function has been defined from the original functions developed by Rebecka Jornsten, which were available freely on
\url{http://www.stat.rutgers.edu/home/rebecka/DDcl/}. However, the link to this page doesn't currently exist as a result of a website redesign. 
}
\examples{
#In the interests of simplicity of the computation involved, only 15 points are selected:
dataTDDcl <- sampleSpanishSurvey[1 : 15, c(2, 3, 5)]  
dataTDDcl_aux <- sampleSpanishSurvey[1 : 15, c(2, 3, 5)]

numClust <- 3 ; alpha <- 0.01 ; lambda <- 0.5 ; niter <- 2
Th <- 0 ; T0 <- 0 ; simAnn <- 0.9  

set.seed(2014)
res_TDDcl <- TDDclust(dataTDDcl, numClust, lambda, Th, niter, T0, simAnn, 
                      alpha, dataTDDcl_aux,FALSE) 

prototypes <- anthrCases(res_TDDcl)

table(res_TDDcl$NN[1,]) 
res_TDDcl$Cost
res_TDDcl$klBest

trimmed <- trimmOutl(res_TDDcl)
}
\keyword{array}


