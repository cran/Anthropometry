\name{TDDclust}
\alias{TDDclust}
\title{
Trimmed clustering based on L1 data depth
}
\description{
This is the trimmed version of the clustering algorithm based on the L1 depth proposed by Rebecka Jornsten (2004).  She segments all the observations in clusters, and assigns to each point z in the data space, the L1 depth value regarding its cluster. A trimmed procedure is incorporated to remove the more extreme individuals of each cluster (those one with the lowest depth values), in line with \code{\link{trimowa}}.
}
\usage{
TDDclust(x,K,lambda,Th,A,T0,alpha,lplot,Trimm,data1)
}
\arguments{
\item{x}{
Data frame. Each row corresponds to an observation, and each column corresponds to a variable. All variables must be numeric.
}
\item{K}{
Number of clusters. 
}
\item{lambda}{
Tuning parameter that controls the influence the data depth has over the clustering, see Jornsten (2004).
}
\item{Th}{
Threshold for observations to be relocated, usually set to 0.
}
\item{A}{
Number of iterations. 
}
\item{T0}{
Simulated annealing parameter. It is the current temperature in the simulated annealing procedure.
}
\item{alpha}{
Simulated annealing parameter. It is the decay rate, default 0.9. 
}
\item{lplot}{
Tracking convergence, default 0.
}
\item{Trimm}{
Proportion of no accommodated sample.
}
\item{data1}{
The same data frame as \emph{x}, used to incorporate the trimmed observations to the rest of them for the next iteration. 
}
}
\value{
A list with the following elements:

\emph{NN}: Cluster assignment, NN[1,] is the final partition. 

\emph{Y}: The multivariate median cluster representatives 

\emph{DD}: Depth values of the observations.

\emph{Cost}: Final value of the optimal partition.

\emph{indivTrimmed}: Trimmed observations.

\emph{klBest}: Iteration in which the optimal partition was found.
}
\references{
Jornsten R., (2004). Clustering and classification based on the L1 data depth, \emph{Journal of Multivariate Analysis} \bold{90}, 67--89

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. In progress.
}
\author{
This function has been defined from the original functions developed by Rebecka Jornsten, which were available freely on
\url{http://www.stat.rutgers.edu/home/rebecka/DDcl/}. However, the link to this page doesn't currently exist as a result of a website redesign. 
}
\examples{
\dontrun{
dataDef <- dataDemo[1:50,c(2,3,5)] #In the interests of simplicity of the computation involved.
data1 <- dataDemo[1:50,c(2,3,5)]

K=3     ; lambda=0.5    ; Th=0; 
A=5     ; T0=0          ; alpha=.9   
lplot=0 ; percTrimm=0.1     

Dout <- TDDclust(x=dataDef,K=K,lambda=lambda,Th=Th,A=A,T0=T0,alpha=alpha,
                 lplot=lplot,Trimm=percTrimm,data1=data1) 

table(Dout$NN[1,]) 

Dout$Cost
Dout$klBest

Dout$indivTrimmed
dataDef[Dout$indivTrimmed,]
}
}
\keyword{array}


