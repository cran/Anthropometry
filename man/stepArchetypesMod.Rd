\name{stepArchetypesMod}
\alias{stepArchetypesMod}
\title{
Archetype algorithm to raw data
}
\description{
This is a slight modification of the original \code{\link{stepArchetypes}} function of the \pkg{archetypes} R package to apply the archetype algorithm to raw data. The \code{\link{stepArchetypes}} function standardizes the data by default and this option is not always desired.
}
\usage{
stepArchetypesMod(data,k,nrep=3,verbose=TRUE)
}
\arguments{
\item{data}{
Data to obtain archetypes.
}
\item{k}{
Number of archetypes to compute, from 1 to \code{k}.
}
\item{nrep}{
For each \code{k}, run \code{\link{archetypes}} \code{nrep} times.
}
\item{verbose}{
If TRUE, the progress during exection is shown.
}
}
\value{
A list with \code{k} elements. Each element is a list of class attribute \code{\link{stepArchetypes}} with \code{nrep} elements.
}
\references{
Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. \emph{Submitted for publication}.
}
\author{
Guillermo Vinue based on the the original \code{\link{stepArchetypes}} function of \pkg{archetypes}.
}
\seealso{
\code{\link{stepArchetypes}}
}
\examples{
\dontrun{
#Cockpit design problem:
#First, the database USAF 1967 is read and preprocessed (Zehner et al (1993).).
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetypes algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 3
lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,nrep=nrep)  
}
}
\keyword{array}
