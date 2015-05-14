\name{trimmedoid}
\alias{trimmedoid}
\title{
Trimmed k-medoids algorithm
}
\description{
This is the trimmed k-medoids algorithm. It is used within \code{\link{trimowa}}. It is analogous to k-medoids but a proportion alpha of observations is discarded by the own procedure (the trimmed observations are self-determined by the data). Furthermore, the trimmed k-medoids is analogous to trimmed k-means. An algorithm for computing trimmed k-means can be found in Garcia-Escudero et al. (2003). See Ibanez et al. (2012) for more details. Note that in the generic name of the k-medoids algorithm, k refers to the number of clusters to search for. To be more specific in the R code, k is referred to as \code{numClust}, see next section \emph{arguments}.
}
\usage{
trimmedoid(D,numClust,alpha,niter,algSteps=7,verbose)
}
\arguments{
\item{D}{
Dissimilarity matrix.
}
\item{numClust}{
Number of clusters.
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{niter}{
Number of random initializations (iterations).
}
\item{algSteps}{
Number of steps of the algorithm per initialization. Default value is 7.
}
\item{verbose}{
A logical specifying whether to provide descriptive output about the running process.
}
}
\value{
 A list with the following elements:

\emph{vopt}: The objective value.

\emph{copt}: The trimmed medoids.

\emph{asig}: The assignation of each observation (asig=0 indicates trimmed individuals).

\emph{ch}:   The goodness index.

\emph{Dmod}: Modified data with the non-trimmed women.

\emph{qq}: Vector with the non-trimmed points.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Garcia-Escudero, L. A., Gordaliza, A., and Matran, C., (2003). Trimming tools in exploratory data analysis, \emph{Journal of Computational and Graphical Statistics} \bold{12(2)}, 434--449.

Garcia-Escudero, L. A., and Gordaliza, A., (1999). Robustness properties of k-means and trimmed k-means, \emph{Journal of the American Statistical Association} \bold{94(447)}, 956--969.
}
\author{
Irene Epifanio
}
\seealso{
\code{\link{sampleSpanishSurvey}}, \code{\link{weightsMixtureUB}}, \code{\link{getDistMatrix}}, \code{\link{trimowa}}, \code{\link{trimmedLloydShapes}}
}
\examples{
\dontrun{
#Data loading:
dataTrimowa <- sampleSpanishSurvey
bust <- dataTrimowa$bust
#First bust class:
data <- dataTrimowa[(bust >= 74) & (bust < 78), ]   
numVar <- dim(dataTrimowa)[2]

#Weights calculation:
orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness,numVar)

#Constants required to specify the distance function:
numClust <- 3
bh <- (apply(as.matrix(log(data)),2,range)[2,] 
       - apply(as.matrix(log(data)),2,range)[1,]) / ((numClust-1) * 8) 
bl <- -3 * bh
ah <- c(23,28,20,25,25)
al <- 3 * ah

#Data processing.
num.persons <- dim(data)[1]
num.variables <- dim(data)[2]
datam <- as.matrix(data)
datat <- aperm(datam, c(2,1))                     
dim(datat) <- c(1,num.persons*num.variables)   

#Dissimilarity matrix:
D <- getDistMatrix(datat,num.persons,numVar,weightsTrimowa,bl,bh,al,ah,TRUE)

trimmedoid(D,numClust,0.01,6,7,TRUE)
}
}
\keyword{math}
