\name{outlierHipam}
\alias{outlierHipam}
\title{
Individuals of the hipam clusters with 1 or 2 elements
}
\description{
This function obtains the individuals of the clusters provided by \code{\link{hipamAnthropom}} that only contains one or two observations.

Given its hierarchical nature, the HIPAM algorithm derives clusters which only contain one or two observations. In the hierarchical clustering methods, all clusters with only one element (the so-called singleton clusters) are consider as outliers. Regarding clusters with two elements, this is because three is the minimum number of elements for clustering with PAM. Taking advantage of this fact, we can also consider the clusters with two elements as outliers. 
}
\usage{
outlierHipam(i,hip)
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
The observations of the clusters with one or two elements for each bust class.
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

bustCirc_4 <- seq(74, 102, 4)  
bustCirc_6 <- seq(107, 131, 6)  
bustCirc <- c(bustCirc_4, bustCirc_6) 
nsizes <- length(bustCirc)
maxsplit <- 5 ; orness <- 0.7

ahVect <- c(23,28,20,25,25)

hip <- list()
for(i in 1 : (nsizes - 1)){
  data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  d <- as.matrix(data)
  hip[[i]] <- hipamAnthropom(d,maxsplit=maxsplit,orness=orness,type="MO",ahVect=ahVect)
}  

#Outliers for the second bust class:
outlierHipam(2,hip)
#Outliers for all the bust classes:
list_outl1_2 <- sapply(1:(nsizes-1),outlierHipam,hip)
}
}
\keyword{array}