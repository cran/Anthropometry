\name{hipamAnthropom}
\alias{hipamAnthropom}
\title{
HIPAM algorithm for anthropometric data
}
\description{
The HIerarchical Partitioning Around Medoids clustering method (HIPAM) was originally created to gene clustering (Wit et al. (2004)). The HIPAM algorithm is a divisive hierarchical clustering method based on the PAM algorithm. 

This function is a HIPAM algorithm adapted to deal with anthropometric data. To that end, a different dissimilarity function is incorporated. This function is that explained in McCulloch et al. (1998) and it is implemented in \code{\link{getDistMatrix}}. We call it $d_{MO}$. In addition, a different method to obtain a classification tree is also incorporated.

Two HIPAM algorithms are proposed. The first one, called $HIPAM_{MO}$, is a HIPAM that uses $d_{MO}$. The second one, $HIPAM_{IMO}$, is a HIPAM algorithm that uses $d_{MO}$ and the INCA (Index Number Clusters Atypical) statistic criterion (Irigoien et al. (2008)) to decide the number of child clusters and as a stopping rule. 

See Vinue et al. (2014) for more details.
}
\usage{
hipamAnthropom(data,asw.tol=0,maxsplit=5,local.const=NULL,
               orness=0.7,type,ah=c(23,28,20,25,25),verbose,...)
}
\arguments{
\item{data}{
Data frame. In our approach, this is each of the subframes originated after segmenting the whole anthropometric Spanish survey into twelve bust segments, according to the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{asw.tol}{
If this value is given, a tolerance or penalty can be introduced (asw.tol > 0 or asw.tol < 0, respectively) in the branch splitting procedure. Default value (0) is maintained. See page 154 of Wit et al. (2004) for more details.
}
\item{maxsplit}{
The maximum number of clusters that any cluster can be divided into when searching for the best clustering.
}
\item{local.const}{
If this value is given (meaningful values are those between -1 and 1), a proposed partition is accepted only if the associated asw is greater than this constant. Default option for this argument is maintained, that is to say, this value is ignored. See page 154 of Wit et al. (2004) for more details.
}
\item{orness}{
Quantity to measure the degree to which the aggregation is like a min or max operation. See \code{\link{weightsMixtureUB}} and \code{\link{getDistMatrix}}.
}
\item{type}{
Type of HIPAM algorithm to be used. The possible options are 'MO' (for $HIPAM_{MO}$) and 'IMO' (for $HIPAM_{IMO}$).
}
\item{ah}{
Constants that define the \code{ah} slopes of the distance function in \code{\link{getDistMatrix}}. Given the five variables considered, this vector is c(23,28,20,25,25). This vector would be different according to the variables considered.
}
\item{verbose}{
Boolean variable (TRUE or FALSE) to indicate whether to report information on progress.
}
\item{...}{
Other arguments that may be supplied to the internal functions of the HIPAM algorithms.
}
}
\details{
The $HIPAM_{MO}$ algorithm uses the \code{\link{getBestPamsamMO}} and \code{\link{checkBranchLocalMO}} functions, while the $HIPAM_{IMO}$ algorithm uses the \code{\link{getBestPamsamIMO}} and \code{\link{checkBranchLocalIMO}} functions. 

For more details of HIPAM, see van der Laan et al. (2003), Wit et al. (2004) and the manual of the \pkg{smida} R package.
}
\value{
A list with the following elements:

\emph{clustering}: Final clustering that corresponds to the last level of the tree.

\emph{asw}: The asw of the final clustering.

\emph{n.levels}: Number of levels in the tree.

\emph{cases}: Anthropometric cases (medoids of all of the clusters in the tree).

\emph{active}: Activity status of each cluster (FALSE for every cluster of the final partition).

\emph{development}: Matrix that indicates the ancestors of the final clusters.

\emph{num.of.clusters}: Number of clusters in the final clustering.

\emph{metric}: Dissimilarity used (called 'McCulloch' because the dissimilarity function used is that explained in McCulloch et al. (1998)).
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2014). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1. \url{https://www.math.rug.nl/~ernst/book/smida.html}.

van der Laan, M. J., and Pollard, K. S., (2003). A new algorithm for hybrid hierarchical clustering with visualization and the bootstrap, \emph{Journal of Statistical Planning and Inference} \bold{117}, 275--303.

Pollard, K. S., and van der Laan, M. J., (2002). A method to identify significant clusters in gene expression data. \emph{Vol. II of SCI2002 Proceedings}, 318--325.

Irigoien, I., and Arenas, C., (2008). INCA: New statistic for estimating the number of clusters and identifying atypical units, \emph{Statistics in Medicine} \bold{27}, 2948--2973.

Irigoien, I., Sierra, B., and Arenas, C., (2012). ICGE: an R package for detecting relevant clusters and atypical units in gene expression, \emph{BMC Bioinformatics} \bold{13}, 1--29.

McCulloch, C., Paal, B., and Ashdown, S., (1998). An optimization approach to apparel sizing, \emph{Journal of the Operational Research Society} \bold{49}, 492--499.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).

Alemany, S., Gonzalez, J. C., Nacher, B., Soriano, C., Arnaiz, C., and Heras, H., (2010). Anthropometric survey of the Spanish female population aimed at the apparel industry. \emph{Proceedings of the 2010 Intl. Conference on 3D Body scanning Technologies}, 307--315.
}
\note{
All the functions related to the HIPAM algorithm were originally created by E. Wit et al., and they are available freely on \url{https://www.math.rug.nl/~ernst/book/smida.html}. In order to develop the $HIPAM_{MO}$ and $HIPAM_{IMO}$ algorithms, we have used and adapted them.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{getBestPamsamMO}}, \code{\link{getBestPamsamIMO}}, \code{\link{checkBranchLocalMO}}, \code{\link{checkBranchLocalIMO}}, \code{\link{plotTreeHipamAnthropom}}, 
}
\examples{
#FOR THE SIZES DEFINED BY THE EUROPEAN NORMATIVE:
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

type <- "IMO"
maxsplit <- 5 ; orness <- 0.7
ah <- c(23, 28, 20, 25, 25)

#For reproducing results, seed for randomness:
#suppressWarnings(RNGversion("3.5.0"))
#set.seed(2013)
numSizes <- 1
res_hipam <- computSizesHipamAnthropom(dataHipam, bust, bustSizes$bustCirc, numSizes,
                                       maxsplit, orness, type, ah, FALSE)

fitmodels <- anthrCases(res_hipam, numSizes)
outliers <- trimmOutl(res_hipam, numSizes)

#FOR ANY OTHER DEFINED SIZE:
#For reproducing results, seed for randomness:
#suppressWarnings(RNGversion("3.5.0"))
#set.seed(1900)
rand <- sample(1:600,20)
dataComp <- sampleSpanishSurvey[rand, c(2, 3, 5)]
numVar <- dim(dataComp)[2]

type <- "IMO"
maxsplit <- 5 ; orness <- 0.7
ah <- c(28, 25, 25) 

dataMat <- as.matrix(dataComp)
#For reproducing results, seed for randomness:
#suppressWarnings(RNGversion("3.5.0"))
#set.seed(2013)
res_hipam_One <- list() ; class(res_hipam_One) <- "hipamAnthropom" 
res_hipam_One[[1]] <- hipamAnthropom(dataMat, maxsplit = maxsplit, orness = orness, 
                                     type = type, ah = ah, verbose = FALSE)
                            
#plotTreeHipamAnthropom(res_hipam_One, main="Proposed Hierarchical PAM Clustering \n")

fitmodels_One <- anthrCases(res_hipam_One,1)
outliers_One <- trimmOutl(res_hipam_One,1)
}
\keyword{array}