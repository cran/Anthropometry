\name{stepArchetypoids}
\alias{stepArchetypoids}
\title{
Run the archetypoid algorithm several times
}
\description{
Execute the archetypoid algorithm repeatedly. It is inspired by the \code{\link{stepArchetypes}} function of the \pkg{archetypes} R package.
}
\usage{
stepArchetypoids(i,nearest,data,ArchObj)
}
\arguments{
\item{i}{
Number of archetypoids.
}
\item{nearest}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. This argument is a logical value: if TRUE (FALSE), the \emph{nearest} (\emph{which}) vector is calculated. Both vectors contain the nearest individuals to the archetypes returned by the \code{\link{archetypes}} function of \pkg{archetypes} (In Vinue et al. (2014), archetypes are computed after running the archetype algorithm twenty times). The \emph{nearest} vector is calculated by computing the Euclidean distance between the archetypes and the individuals and choosing the nearest. It is used in Epifanio et al. (2013). The \emph{which} vector is calculated by consecutively identifying the individual with the maximum value of alpha for each archetype, until the defined number of archetypes is reached. It is used in Eugster (2012). 
}
\item{data}{
Data matrix. Each row corresponds to an observation and each column corresponds to an anthropometric variable. All variables are numeric.
}
\item{ArchObj}{
The list returned by the \code{\link{stepArchetypesMod}} function. This function is a slight modification of the original \code{\link{stepArchetypes}} function of \pkg{archetypes} to apply the archetype algorithm to raw data. The \code{\link{stepArchetypes}} function standardizes the data by default and this option is not always desired. This list is needed to compute the nearest individuals to archetypes. 
}
}
\value{
A list with the following elements:

\emph{archet}: Final vector of k archetypoids.

\emph{rss}: Residual sum of squares corresponding to the final vector of k archetypoids.

\emph{archet_ini}: Vector of initial archetypoids (\emph{nearest} or \emph{which}).

\emph{alphas}: Alpha coefficients for the optimal vector of archetypoids.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. \emph{Submitted for publication}.

Cutler, A., and Breiman, L., (1994). Archetypal Analysis, \emph{Technometrics} \bold{36}, 338--347.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Eugster, M. J. A., (2012). Performance profiles based on archetypal athletes, \emph{International Journal of Performance Analysis in Sport} \bold{12}, 166--187.
}
\note{
It may be happen that \code{\link{archetypes}} does not find results for \emph{k} archetypes. In this case, it is not possible to calculate the vector of nearest individuals and consequently, the vector of archetypoids. Therefore, this function will return an error message. 
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}, \code{\link{archetypes}}, \code{\link{stepArchetypes}}
}
\examples{
\dontrun{
#Cockpit design problem:
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE)

#Computation of archetypes and archetypoids:
#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetypes algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 20
lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,nrep=nrep)  

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
for(i in 1:numArch){
  temp <-  stepArchetypoids(i,TRUE,preproc$data,lass)
  filename <- paste("res", i, sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep="")) 
}  

for(i in 1:numArch){
  temp <-  stepArchetypoids(i,FALSE,preproc$data,lass)
  filename <- paste("res", i, "_which",sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep=""))
}
}
}
\keyword{array}
