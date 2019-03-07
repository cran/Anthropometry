\name{computSizesHipamAnthropom}
\alias{computSizesHipamAnthropom}
\title{
Computation of the hipamAnthropom elements for a given number of sizes defined by the EN
}
\description{
This is a helper function for computing the hipamAnthropom elements provided by the \code{\link{hipamAnthropom}} algorithm for a number of bust sizes defined by the European Normative (EN). Therefore, the \code{\link{hipamAnthropom}} is used inside this function.
}
\usage{
computSizesHipamAnthropom(dataHip, bust, bustMeasur, nsizes, maxsplit, orness, 
                          type, ah, verbose = FALSE)
}
\arguments{
\item{dataHip}{
Data frame.
}
\item{bust}{
Bust column of the data frame.
}
\item{bustMeasur}{
Sequence vector of bust measurements (bust sizes) provided by the \code{\link{bustSizesStandard}} function.
}
\item{nsizes}{
Number of sizes defined by the European Normative to apply the \code{\link{hipamAnthropom}} function.
}
\item{maxsplit, orness, type, ah, verbose}{
Same arguments as those of the \code{\link{hipamAnthropom}} function.
}
}
\value{
A list with the same elements as the \code{\link{hipamAnthropom}} function.
}
\references{
European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
  
Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2014). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{hipamAnthropom}}, \code{\link{bustSizesStandard}}
}
\examples{
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

type <- "IMO"
maxsplit <- 5 ; orness <- 0.7
ah <- c(23, 28, 20, 25, 25)

suppressWarnings(RNGversion("3.5.0"))
set.seed(2013)
numSizes <- 1
res_hipam <- computSizesHipamAnthropom(dataHipam, bust, bustSizes$bustCirc, numSizes,
                                       maxsplit, orness, type, ah, FALSE)
}
\keyword{math}
