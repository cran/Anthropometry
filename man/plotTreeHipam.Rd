\name{plotTreeHipam}
\alias{plotTreeHipam}
\title{
HIPAM dendogram
}
\description{
This function represents a dendrogram for the clustering results provided by a HIPAM algorithm. It is a small modification of the original \code{plot.tree} function of the \pkg{smida} R package, available from \url{http://www.math.rug.nl/~ernst/book/smida.html}.
}
\usage{
plotTreeHipam(x,title,...)
}
\arguments{
\item{x}{
The HIPAM object to be plotted.
}
\item{title}{
The title of the plot.
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
dataDef <- dataDemo
bust <- dataDef$bust

interv <- c("74-78","78-82","82-86","86-90","90-94","94-98","98-102",
            "102-107","107-113","113-119","119-125","125-131")
bustCirc_4 <- seq(74,102,4)  
bustCirc_6 <- seq(107,131,6)  
bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
maxsplit <- 5 ; orness <- 0.7 ; alpha <- 0.01 ; type <- "IMO" #type <- "MO" for $HIPAM_{MO}$

ahVect <- c(23, 28, 20, 25, 25)

hip <- list()
for(i in 1 : (nsizes - 1)){
  data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  d <- as.matrix(data)
  hip[[i]] <- hipamAnthropom(d,maxsplit=maxsplit,orness=orness,type=type,ahVect=ahVect) 
}   
str(hip) 

#Dendogram for the first bust class:
plotTreeHipam(hip[[1]],title=paste("Proposed Hierarchical PAM Clustering \n",interv[1]))
}
}
\keyword{array}
