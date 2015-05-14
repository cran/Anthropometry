\name{anthrCases}
\alias{anthrCases}
\title{
Helper function for obtaining the anthropometric cases
}
\description{
Because the goal of the methodologies included in this package is always to estimate a number of anthropometric cases given a data set (both central (prototypes) and boundaries (archetypoids)), this auxiliary function allows the user to identify the cases computed by each method in an easy way.
}
\usage{
anthrCases(field, method, resMethod, oneSize, nsizes)
}
\arguments{
\item{field}{
Character vector with two possible values. If "anthropometry", the prototypes computed by \code{\link{trimowa}}, \code{\link{TDDclust}}, \code{\link{hipamAnthropom}}, \code{\link{LloydShapes}}, \code{\link{HartiganShapes}} or \code{\link{trimmedLloydShapes}} are returned. If "ergonomics", the archetypoids provided by the archetypoid algorithm are provided. 
}
\item{method}{
This argument is needed if \code{field}="anthropometry". It is a character vector with four possible values: "trimowa" for \code{\link{trimowa}}, "TDDclust" for \code{\link{TDDclust}}, "HipamAnthropom" for \code{\link{hipamAnthropom}} and "kmeansProcrustes" for \code{\link{LloydShapes}}, \code{\link{HartiganShapes}} and \code{\link{trimmedLloydShapes}}.
}
\item{resMethod}{
This is the object which saves the results obtained by the aforementioned methodologies and which contains the anthropometric cases to return.
}
\item{oneSize}{
A logical specifying whether to obtain prototypes for a single size. This argument is needed if \code{method}="trimowa" of \code{method}="HipamAnthropom" because these methodologies can compute the prototypes for a single bust size or for the bust sizes defined by the European Normative. If FALSE, the following argument \code{nsizes} is needed.
}
\item{nsizes}{
Number of bust sizes. This argument is needed if \code{method}="trimowa" of \code{method}="HipamAnthropom" because these methodologies compute the prototypes for each bust size defined by the European Normative.
}
}
\value{
Vector with the central or boundary anthropometric cases.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., and Ibanez, M. V., (2014). \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{TDDclust}}, \code{\link{hipamAnthropom}}, \code{\link{LloydShapes}}, \code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}, \code{\link{archetypoids}}, \code{\link{stepArchetypoids}}
}
\examples{
\dontrun{
#TRIMOWA ALGORITHM FOR ONE SIZE:
set.seed(1900)
rand <- sample(1:600,20)
dataComp <- sampleSpanishSurvey[rand, c(2, 3, 5)]

numVar <- dim(dataComp)[2] 
orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness, numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 5 
ahVect <- c(28, 25, 25) ;  algSteps <- 7

set.seed(1900)
Trimowa <- trimowa(dataComp, weightsTrimowa, numClust, alpha, niter, 
                   algSteps, ahVect, verbose = FALSE)
prototypes_Trimowa <- anthrCases("anthropometry", "trimowa", 
                                 Trimowa, oneSize = TRUE)
}
}
\keyword{math}
