\name{indivNearest}
\alias{indivNearest}
\title{
Nearest individuals to archetypes
}
\description{
The nearest individual to each archetype can be obtained by simply computing the distance between the archetypes and the individuals and choosing the nearest. This is the procedure to obtain what is called the \emph{nearest} vector, see Vinue et al. (2013). It is used within \code{\link{archetypoids}} and \code{\link{stepArchetypoids}}.
}
\usage{
indivNearest(Indivs,NumArchet,mdras)
}
\arguments{
\item{Indivs}{
Vector from 1 to \code{NumArchet}.
}
\item{NumArchet}{
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
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. \emph{Submitted for publication}.

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
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE) 

#Procedure and results shown in section 2.2.2 and section 3.1 of Epifanio et al. (2013):
res <- archetypesBoundary(preproc$data,15,FALSE,3)

i=3
a3 <- archetypes::bestModel(res[[i]])
ras <- rbind(archetypes::parameters(a3),preproc$data)
dras <- dist(ras,method="euclidean",diag=F,upper=T,p=2)
mdras <- as.matrix(dras)
diag(mdras) <- 1e+11
sapply(1:i,indivNearest,i,mdras) 
}
}
\keyword{array}