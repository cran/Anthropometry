\name{nearestToArchetypes}
\alias{nearestToArchetypes}
\title{
Nearest individuals to archetypes
}
\description{
The nearest individual to each archetype can be obtained by simply computing the distance between the archetypes and the individuals and choosing the nearest. This is the procedure to obtain what is called the \emph{cand_ns} vector, see Vinue et al. (2015). It is used within \code{\link{archetypoids}} and \code{\link{stepArchetypoids}}.
}
\usage{
nearestToArchetypes(indivs,numArch,mdras)
}
\arguments{
\item{indivs}{
Vector from 1 to \code{numArch} of individuals nearest to archetypes.
}
\item{numArch}{
Number of archetypes computed. 
}
\item{mdras}{
Distance matrix between the archetypes and the individuals.
}
}
\value{
A vector with the nearest individuals to archetypes.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\author{
Irene Epifanio
}
\seealso{
\code{\link{archetypoids}}, \code{\link{stepArchetypoids}}, \code{\link{archetypesBoundary}}
}
\examples{
\dontrun{
#First,the USAF 1967 database is read and preprocessed (Zehner et al. (1993)).
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_inch <- USAFSurvey[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_inch, TRUE, 0.95, TRUE)

#Procedure and results shown in section 2.2.2 and section 3.1 of Epifanio et al. (2013):
res <- archetypesBoundary(USAFSurvey_preproc$data,15,FALSE,3)

numArch <- 3
a3 <- archetypes::bestModel(res[[numArch]])
ras <- rbind(archetypes::parameters(a3),USAFSurvey_preproc$data)
dras <- dist(ras,method="euclidean",diag=F,upper=T,p=2)
mdras <- as.matrix(dras)
diag(mdras) <- 1e+11
sapply(seq(length=numArch),nearestToArchetypes,numArch,mdras) 
}
}
\keyword{array}