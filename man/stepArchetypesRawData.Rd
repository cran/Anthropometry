\name{stepArchetypesRawData}
\alias{stepArchetypesRawData}
\title{
Archetype algorithm to raw data
}
\description{
This is a slight modification of the original \code{\link{stepArchetypes}} function of the \pkg{archetypes} R package to apply the archetype algorithm to raw data. The \code{\link{stepArchetypes}} function standardizes the data by default and this option is not always desired.
}
\usage{
stepArchetypesRawData(data,numArch,numRep=3,verbose=TRUE)
}
\arguments{
\item{data}{
Data to obtain archetypes.
}
\item{numArch}{
Number of archetypes to compute, from 1 to \code{numArch}.
}
\item{numRep}{
For each \code{numArch}, run \code{\link{archetypes}} \code{numRep} times.
}
\item{verbose}{
If TRUE, the progress during execution is shown.
}
}
\value{
A list with \code{numArch} elements. Each element is a list of class attribute \code{\link{stepArchetypes}} with \code{numRep} elements.
}
\references{
Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.
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
USAFSurvey_First50 <- USAFSurvey[1 : 50, ]
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_First50_inch <- USAFSurvey_First50[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_First50_inch, TRUE, 0.95, TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetypes algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; numRep <- 20
lass <- stepArchetypesRawData(data = USAFSurvey_preproc$data, 
                          numArch=1:numArch, numRep = numRep, 
                          verbose = FALSE)
}
}
\keyword{array}
