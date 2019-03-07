\name{trimowa}
\alias{trimowa}
\title{
Trimmed PAM with OWA operators
}
\description{
This is the methodology developed in Ibanez et al. (2012) to define an efficient apparel sizing system based on clustering techniques jointly with OWA operators. In our approach, we apply the trimmed k-medoids algorithm (\code{\link{trimmedoid}}) to the first twelve bust classes according to the sizes defined in the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals.
}
\usage{
trimowa(data,w,numClust,alpha,niter,algSteps,ah=c(23,28,20,25,25),verbose)
}
\arguments{
\item{data}{
Data frame. In our approach, this is each of the subframes originated after segmenting the whole anthropometric Spanish survey into twelve bust segments, according to the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{w}{
The aggregation weights of the OWA operators. They are computed with the \code{\link{weightsMixtureUB}}.
}
\item{numClust}{
Number of clusters.
}
\item{alpha}{
Proportion of trimmed sample.
}
\item{niter}{
Number of random initializations (iterations).
}
\item{algSteps}{
Number of steps of the algorithm per initialization. Default value is 7.
}
\item{ah}{
Constants that define the \code{ah} slopes of the distance function in \code{\link{getDistMatrix}}. Given the five variables considered, this vector is c(23,28,20,25,25). This vector would be different according to the variables considered.
}
\item{verbose}{
A logical specifying whether to provide descriptive output about the running process.
}
}
\value{
A list with the following elements:

\emph{cases}: Anthropometric cases (medoids of the clusters). They are the prototypes obtained for each bust class.

\emph{numTrim}: Number of trimmed individuals in each bust class.

\emph{numClass}: Number of individuals in each bust class.

\emph{noTrim}: Number of of non-trimmed individuals.

\emph{C1,C2,C3,C4}: Required constant values to define the distance \code{\link{getDistMatrix}} (\emph{C1} is bh, \emph{C2} is bl, \emph{C3} is ah and \emph{C4} is al).

\emph{asig}: Vector of the clusters to which each individual belongs.

\emph{discarded}: Discarded (trimmed) individuals.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{sampleSpanishSurvey}}, \code{\link{weightsMixtureUB}}, \code{\link{getDistMatrix}}, \code{\link{trimmedoid}}
}
\examples{
#FOR THE SIZES DEFINED BY THE EUROPEAN NORMATIVE:
dataTrimowa <- sampleSpanishSurvey
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness, numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

suppressWarnings(RNGversion("3.5.0"))
set.seed(2014)
numSizes <- 2
res_trimowa <- computSizesTrimowa(dataTrimowa, bust, bustSizes$bustCirc, numSizes,
                                  weightsTrimowa, numClust, alpha, niter, algSteps, 
                                  ah, FALSE)
prototypes <- anthrCases(res_trimowa, numSizes)

#FOR ANY OTHER DEFINED SIZE:
suppressWarnings(RNGversion("3.5.0"))
set.seed(1900)
rand <- sample(1:600,20)
dataComp <- sampleSpanishSurvey[rand, c(2, 3, 5)]
numVar <- dim(dataComp)[2]

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness, numVar)
numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(28, 25, 25) 

suppressWarnings(RNGversion("3.5.0"))
set.seed(2014)
res_trimowa <- trimowa(dataComp, weightsTrimowa, numClust, alpha, niter, 
                            algSteps, ah, verbose = FALSE)
class(res_trimowa) <- "trimowa"
prototypes <- anthrCases(res_trimowa, 1)
}
\keyword{array}
