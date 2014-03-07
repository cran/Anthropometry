\name{Anthropometry-internalHIPAM}
\alias{initialize.tree}
\alias{ext.dist}
\alias{pamsam}
\alias{initial.clustering}
\alias{cluster.search}
\alias{asw.calc}
\alias{further.clustering}
\alias{partition.cluster}
\alias{collapse.cluster}
\alias{collapse.step}
\alias{hipam.local}
\alias{update.tree.local}
\alias{full2dist}
\title{
Several internal functions used by both $HIPAM_{MO}$ and $HIPAM_{IMO}$ algorithms
}
\description{
This file contains several functions called for both $HIPAM_{MO}$ and $HIPAM_{IMO}$ algorithms. Therefore, they are not used directly. 

The $HIPAM_{MO}$ and $HIPAM_{IMO}$ algorithms are different in the way to generate and validate a clustering partition. However, the functions related to the further clustering steps are common to both of them. These functions are those collected in this file. 

The \emph{ext.dist} function is adapted to compute the dissimilarity developed in McCulloch et al. (1998). See \code{\link{hipamAnthropom}} for a detailed explanation of the HIPAM algorithm.
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel
sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1.
\url{http://www.math.rug.nl/~ernst/book/smida.html}.

McCulloch, C., Paal, B. and Ashdown, S., (1998). An optimization approach to apparel sizing, \emph{Journal of the Operational Research Society} \bold{49}, 492--499.
}
\author{
All these functions were originally created by E. Wit et al., and they are available freely on
\url{http://www.math.rug.nl/~ernst/book/smida.html}.
}
\seealso{
\code{\link{hipamAnthropom}}, \code{\link{getBestPamsamMO}}, \code{\link{getBestPamsamIMO}}, \code{\link{checkBranchLocalMO}}, \code{\link{checkBranchLocalIMO}}
}
\keyword{internal}

