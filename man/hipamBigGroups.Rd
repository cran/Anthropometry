\name{hipamBigGroups}
\alias{hipamBigGroups}
\title{
Hipam medoids of the clusters with more than 2 elements
}
\description{
This function obtains the medoids of the clusters provided by \code{\link{hipamAnthropom}} that contains more than two individuals.
}
\usage{
hipamBigGroups(i,hip)
}
\arguments{
\item{i}{
Each bust group considered according to the European standard to sizing system.
}
\item{hip}{
Hipam object, see \code{\link{hipamAnthropom}}.
}
}
\value{
The medoids of the clusters with more than two elements for each bust class.
}
\references{
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Wit, E., and McClure, J., (2004). \emph{Statistics for Microarrays: Design, Analysis and Inference}. John Wiley & Sons, Ltd.

Wit, E., and McClure, J., (2006). Statistics for Microarrays: Inference, Design and Analysis. R package version 0.1. \url{http://www.math.rug.nl/~ernst/book/smida.html}.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{hipamAnthropom}}
}
\examples{
\dontrun{
dataDef <- dataDemo
bust <- dataDef$bust

bustCirc_4 <- seq(74,102,4)  ; bustCirc_6 <- seq(107,131,6)  ; bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
maxsplit <- 5 ; orness <- 0.7

ahVect <- c(23,28,20,25,25)

hip <- list()
for(i in 1 : (nsizes-1)){
  data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  d <- as.matrix(data)
  hip[[i]] <- hipamAnthropom(d,maxsplit=maxsplit,orness=orness,type="MO",ahVect=ahVect)
}  
#Medoids of the clusters with more than two elements for the second bust class:
hipamBigGroups(2,hip)
#Medoids of the clusters with more than two elements for all the bust classes:
list.meds <- lapply(1:(nsizes-1),FUN=hipamBigGroups,hip)  
}
}
\keyword{array}