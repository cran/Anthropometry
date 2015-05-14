\name{plotTreeHipamAnthropom}
\alias{plotTreeHipamAnthropom}
\title{
HIPAM dendogram
}
\description{
This function represents a dendrogram for the clustering results provided by a HIPAM algorithm. It is a small modification of the original \code{plot.tree} function of the \pkg{smida} R package, available from \url{http://www.math.rug.nl/~ernst/book/smida.html}.
}
\usage{
plotTreeHipamAnthropom(x,main,...)
}
\arguments{
\item{x}{
The HIPAM object to be plotted.
}
\item{main}{
Title of the plot.
}
\item{...}{
Other arguments that may be supplied.
}
}
\value{
A device with the desired plot.
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1. \url{http://www.math.rug.nl/~ernst/book/smida.html}.
}
\note{
This function only represents the 'tree' option of the original \code{plot.tree} function of \pkg{smida}, because we believe that this option displays better the clustering results provided by HIPAM than the option '2d'.
}
\author{
This function was originally created by E. Wit et al., and it is available freely on
\url{http://www.math.rug.nl/~ernst/book/smida.html}. We have slightly modified. 
}
\seealso{
\code{\link{hipamAnthropom}}
}
\examples{
\dontrun{
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

interv <- c("74-78","78-82","82-86","86-90","90-94","94-98","98-102",
            "102-107","107-113","113-119","119-125","125-131")

maxsplit <- 5 ; orness <- 0.7 ; alpha <- 0.01 ; type <- "IMO" #type <- "MO" for $HIPAM_{MO}$
ah <- c(23, 28, 20, 25, 25)

set.seed(2013)
res_hipam <- list()
for(i in 1 : (bustSizes$nsizes - 1)){
 data = dataHipam[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]
 dataMat <- as.matrix(data)
 res_hipam[[i]] <- hipamAnthropom(dataMat, maxsplit = maxsplit, orness = orness, type = type,
                                  ah = ah, verbose = FALSE)
}  
str(res_hipam) 

#Dendogram for the first bust class:
plotTreeHipamAnthropom(res_hipam[[1]],
                       main=paste("Proposed Hierarchical PAM Clustering \n",
                       interv[1]))
}
}
\keyword{array}
