\name{Anthropometry-internalArchetypoids}
\alias{ahull}
\alias{no.scalefn}
\alias{no.rescalefn}
\alias{.perc}
\alias{swap}
\alias{swap2}
\alias{swap2_k1}
\title{
Several internal functions to compute and represent archetypes and archetypoids
}
\description{
This file contains six extra functions. The \code{ahull} function is a helper function to calculate the approximated convex hull. The \code{no.scalefn} and \code{no.rescalefn} functions allow us to use the archetype algorithm without standardizing the data (see \code{\link{stepArchetypesRawData}}). The other three functions are called by the archetypoid algorithm (\code{\link{archetypoids}}). All these functions are therefore not solely used.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Epifanio, I., Vinue,, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.
}
\seealso{
\code{\link{stepArchetypesRawData}}, \code{\link{archetypoids}}
}
\keyword{internal}

