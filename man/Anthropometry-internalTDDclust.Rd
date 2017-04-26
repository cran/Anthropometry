\name{Anthropometry-internalTDDclust}
\alias{is.empty}
\alias{rowweight}
\alias{Distvec}
\alias{l1norm}
\alias{unitvec}
\alias{initW2}
\alias{Weisziteradj}
\alias{Weiszfeldadj}
\alias{DDapply}
\alias{DDfcnadj}
\alias{DDapplyloo}
\alias{DDcalc2}
\alias{pamsil}
\alias{NNDDVQA1}
\alias{NNDDVQE}
\alias{NNDDVQEstart}
\title{
Several internal functions to clustering based on the L1 data depth
}
\description{
This file contains a large number of extra functions originally developed to execute the \emph{DDclust} algorithm proposed by R. Jornsten in Jornsten (2004). \emph{DDclust} is a method for clustering based on the L1 data depth. 

Regarding the original functions, a stopping criterion and a trimmed procedure have been incorporated to \emph{NNDDVQE} and \emph{NNDDVQEstart}. The stopping criterion includes a tolerance which, if crossed, stops the iterations. The trimmed procedure allows us to discard the more extreme individuals (those with the lowest depth values). See \code{\link{TDDclust}}. 

All these functions are therefore not solely used.
}
\references{
Jornsten R., (2004). Clustering and classification based on the L1 data depth, \emph{Journal of Multivariate Analysis} \bold{90}, 67--89

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
All these functions were originally created by R. Jornsten and they were available freely on
http://www.stat.rutgers.edu/home/rebecka/DDcl/. However, the link to this page doesn't currently exist as a result of a website redesign. 
}
\seealso{
\code{\link{TDDclust}}
}
\keyword{internal}

