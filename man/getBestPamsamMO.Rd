\name{getBestPamsamMO}
\alias{getBestPamsamMO}
\title{
Generation of the candidate clustering partition in HIPAM-MO
}
\description{
The HIPAM algorithm starts with one large cluster and, at each level, a given (parent) cluster is partitioned using PAM.

In this version of HIPAM, called HIPAM-MO, the number k of (child) clusters is obtained by maximizing the silhouette width (asw). See Vinue et al. (2014) for more details.

The foundation and performance of the HIPAM algorithm is explained in \code{\link{hipamAnthropom}}.
}
\usage{
getBestPamsamMO(data,maxsplit,orness=0.7,type,ah,verbose,...)
}
\arguments{
\item{data}{
Data to be clustered.
}
\item{maxsplit}{
The maximum number of clusters that any cluster can be divided when searching for the best clustering.
}
\item{orness}{
Quantity to measure the degree to which the aggregation is like a min or max operation. See \code{\link{weightsMixtureUB}} and \code{\link{getDistMatrix}}.
}
\item{type}{
Option 'MO' for using HIPAM-MO.
}
\item{ah}{
Constants that define the \code{ah} slopes of the distance function in \code{\link{getDistMatrix}}. Given the five variables considered, this vector is c(23,28,20,25,25). This vector would be different according to the variables considered.
}
\item{verbose}{
Boolean variable (TRUE or FALSE) to indicate whether to report information on progress.
}
\item{...}{
Other arguments that may be supplied.
}
}
\value{
A list with the following elements:

\emph{medoids}: The cluster medoids.

\emph{clustering}: The clustering partition obtained.

\emph{asw}: The asw of the clustering.

\emph{num.of.clusters}: Number of clusters in the final clustering.

\emph{info}: List that informs about the progress of the clustering algorithm.

\emph{profiles}: List that contains the asw and sesw (stardard error of the silhouette widths) profiles at each
stage of the search.

\emph{metric}: Dissimilarity used (called 'McCulloch' because the dissimilarity function used is that explained in McCulloch et al. (1998)).
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2014). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1.
\url{https://www.math.rug.nl/~ernst/book/smida.html}.

Pollard, K. S., and van der Laan, M. J., (2002). A method to identify significant clusters in gene expression data. \emph{Vol. II of SCI2002 Proceedings}, 318--325.

McCulloch, C., Paal, B., and Ashdown, S., (1998). An optimization approach to apparel sizing, \emph{Journal of the Operational Research Society} \bold{49}, 492--499.
}
\note{
This function belongs to the HIPAM-MO algorithm and it is not solely used. That is why there is no section of \emph{examples} in this help page. See \code{\link{hipamAnthropom}}.
}
\author{
This function was originally created by E. Wit et al., and it is available freely on
\url{https://www.math.rug.nl/~ernst/book/smida.html}.
}
\seealso{
\code{\link{hipamAnthropom}}
}
\keyword{array}