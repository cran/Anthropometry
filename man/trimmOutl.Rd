\name{trimmOutl}
\alias{trimmOutl}
\title{
Helper function for obtaining the trimmed and outlier observations
}
\description{
The methodologies included in this package that are developed to the clothing design problem take into account that a clothing sizing system is intended to cover only what we could call standard population, leaving out those individuals who are extreme respect to a set of measurements. For \code{\link{trimowa}}, \code{\link{TDDclust}} and \code{\link{LloydShapes}}, \code{\link{HartiganShapes}} and \code{\link{trimmedLloydShapes}} these individuals are called trimmed individuals and for \code{\link{hipamAnthropom}} these individuals are called outlier individuals.
}
\usage{
trimmOutl(method, resMethod, oneSize, nsizes)
}
\arguments{
\item{method}{
It is a character vector with four possible values: "trimowa" for \code{\link{trimowa}}, "TDDclust" for \code{\link{TDDclust}}, "HipamAnthropom" for \code{\link{hipamAnthropom}} and "kmeansProcrustes" for \code{\link{LloydShapes}}, \code{\link{HartiganShapes}} and \code{\link{trimmedLloydShapes}}.
}
\item{resMethod}{
This is the object which saves the results obtained by the aforementioned methodologies and which contains the discarded individuals to return.
}
\item{oneSize}{
A logical specifying whether to obtain trimmed or outliers for a single size. This argument is needed if \code{method}="trimowa" of \code{method}="HipamAnthropom" because these methodologies can compute the trimmed or outliers for a single bust size or for the bust sizes defined by the European Normative. If FALSE, the following argument \code{nsizes} is needed.
}
\item{nsizes}{
Number of bust sizes. This argument is needed if \code{method}="trimowa" of \code{method}="HipamAnthropom" because these methodologies compute the trimmed or outliers for each bust size defined by the European Normative.
}
}
\value{
Vector with the trimmed or outlier observations.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{TDDclust}}, \code{\link{hipamAnthropom}}, \code{\link{LloydShapes}}, \code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}
}
\examples{
\dontrun{
#TRIMOWA ALGORITHM:
dataTrimowa <- sampleSpanishSurvey
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness,numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

set.seed(2014)
res_trimowa <- list()
for (i in 1 : (bustSizes$nsizes - 1)){ 
 data = dataTrimowa[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]   
 res_trimowa[[i]] <- trimowa(data, weightsTrimowa, numClust, alpha, niter, 
                             algSteps,ah,verbose=FALSE)
}

trimmed <- trimmOutl("trimowa", res_trimowa, oneSize = FALSE, bustSizes$nsizes)
}
}
\keyword{math}
