\name{getDistMatrix}
\alias{getDistMatrix}
\title{
Dissimilarity matrix between individuals and prototypes
}
\description{
In the definition of a sizing system, a distance function allows us to represent mathematically the idea of garment fit and it is a key element to quantify the misfit between an individual and the prototype.

This function computes the dissimilarity defined in McCulloch et al. (1998), which is used in \code{\link{trimowa}} and \code{\link{hipamAnthropom}}. For more details, see also Ibanez et al. (2012) and Vinue et al. (2013).
}
\usage{
getDistMatrix(data,np,nv,w,bl,bh,al,ah,verbose)
}
\arguments{
\item{data}{
Data vector.
}
\item{np}{
Number of observations in the database.
}
\item{nv}{
Number of variables in the database.
}
\item{w}{
Weights for the OWA operator computed by means of \code{\link{weightsMixtureUB}}.
}
\item{bl,bh,al,ah}{
Constants required to specify the distance function.
}
\item{verbose}{
Boolean variable (TRUE or FALSE) to indicate whether to report information on progress.
}
}
\details{
At the computational level, it is asummed that all the \code{bh} values are negative, all the \code{bl} values are positive and all the \code{al} and \code{ah} slopes are positive (the sign of \code{al} is changed within the function when computing the dissimilarities).
}
\value{
A symmetric \code{np} x \code{np} matrix of dissimilarities.
}
\references{
McCulloch, C., Paal, B., and Ashdown, S., (1998). An optimization approach to apparel sizing, \emph{Journal of the Operational Research Society} \bold{49}, 492--499.

Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Leon, T., Zuccarello, P., Ayala, G., de Ves, E., and Domingo, J., (2007), Applying logistic regression to relevance feedback in image retrieval systems, \emph{Pattern Recognition} \bold{40}, 2621--2632.
}
\note{
This function requires a C code called cast.c. In order to use  \code{\link{getDistMatrix}} outside the package, the dynamic-link library is called by means of the sentence \code{dyn.load("cast.so")} (In Windows, it would be \code{dyn.load("cast.dll")}). 
}
\author{
Juan Domingo
}
\seealso{
\code{\link{trimowa}}, \code{\link{hipamAnthropom}}
}
\examples{
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
dim(datat) <- c(1,num.persons * num.variables)   

#Dissimilarity matrix:
D <- getDistMatrix(datat, num.persons, numVar, weightsTrimowa, bl, bh, al, ah, FALSE)
}
\keyword{array}
