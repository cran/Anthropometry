\name{computSizesTrimowa}
\alias{computSizesTrimowa}
\title{
Computation of the trimowa elements for a given number of sizes defined by the EN
}
\description{
This is a helper function for computing the trimowa elements provided by the \code{\link{trimowa}} algorithm for a number of bust sizes defined by the European Normative (EN). Therefore, the \code{\link{trimowa}} is used inside this function.
}
\usage{
computSizesTrimowa(dataTrim, bust, bustMeasur, nsizes, w, numClust, alpha, 
                   niter, algSteps, ah, verbose = FALSE)
}
\arguments{
\item{dataTrim}{
Data frame.
}
\item{bust}{
Bust column of the data frame.
}
\item{bustMeasur}{
Sequence vector of bust measurements (bust sizes) provided by the \code{\link{bustSizesStandard}} function.
}
\item{nsizes}{
Number of sizes defined by the European Normative to apply the \code{\link{trimowa}} function.
}
\item{w, numClust, alpha, niter, algSteps, ah, verbose}{
Same arguments as those of the \code{\link{trimowa}} function.
}
}
\value{
A list with the same elements as the \code{\link{trimowa}} function.
}
\references{
European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).

Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{bustSizesStandard}}
}
\examples{
dataTrimowa <- sampleSpanishSurvey
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness, numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

set.seed(2014)
numSizes <- 1
res_trimowa <- computSizesTrimowa(dataTrimowa, bust, bustSizes$bustCirc, numSizes,
                                  weightsTrimowa, numClust, alpha, niter,
                                  algSteps, ah, verbose = FALSE)
}

\keyword{math}
