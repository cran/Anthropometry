\name{checkBranchLocalMO}
\alias{checkBranchLocalMO}
\title{
Evaluation of the candidate clustering partition in $HIPAM_{MO}$
}
\description{
In the HIPAM algorithm, each (parent) cluster P is investigated to see if it can be divided further into new (child) clusters, or stop (in this case, P would be a terminal node). 

In this version of HIPAM, called $HIPAM_{MO}$, there are two different stopping criteria: First, if $|P| leq 2$, then P is a terminal node. If not, the second stopping criteria uses the Mean Split Silhouette. See Vinue et al. (2014) for more details.

The foundation and performance of the HIPAM algorithm is explained in \code{\link{hipamAnthropom}}.
}
\usage{
checkBranchLocalMO(tree,data,i,maxsplit,asw.tol,local.const,orness,type,ah,
                   verbose,...)
}
\arguments{
\item{tree}{
The clustering tree being defined.
}
\item{data}{
Data to be clustered.
}
\item{i}{
A specific cluster of the clustering partition in a certain level of the tree.
}
\item{maxsplit}{
The maximum number of clusters that any cluster can be divided when searching for the best clustering.
}
\item{asw.tol}{
If this value is given, a tolerance or penalty can be introduced (asw.tol > 0 or asw.tol < 0, respectively) in the branch splitting procedure. Default value (0) is maintained. See page 154 of Wit et al. (2004) for more details.
}
\item{local.const}{
If this value is given (meaningful values are those between -1 and 1), a proposed partition is accepted only if the associated asw is greater than this constant. Default option for this argument is maintained, that is to say, this value is ignored. See page 154 of Wit et al. (2004) for more details.
}
\item{orness}{
Quantity to measure the degree to which the aggregation is like a min or max operation. See \code{\link{weightsMixtureUB}} and \code{\link{getDistMatrix}}.
}
\item{type}{
Option 'MO' for using $HIPAM_{MO}$.
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
The new resulting classification tree.
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2014). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1.
\url{http://www.math.rug.nl/~ernst/book/smida.html}

Pollard, K. S., and van der Laan, M. J., (2002). A method to identify significant clusters in gene expression data. \emph{Vol. II of SCI2002 Proceedings}, 318--325.
}
\note{
This function belongs to the $HIPAM_{MO}$ algorithm and it is not solely used. That is why there is no section of \emph{examples} in this help page. See \code{\link{hipamAnthropom}}.
}
\author{
This function was originally created by E. Wit et al., and it is available freely on
\url{http://www.math.rug.nl/~ernst/book/smida.html}.
}
\seealso{
\code{\link{hipamAnthropom}}
}
\keyword{array}