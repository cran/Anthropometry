\name{cdfDissWomenPrototypes}
\alias{cdfDissWomenPrototypes}
\title{
CDF for the dissimilarities between women and computed medoids and standard prototypes
}
\description{
This function allows us to calculate the Cumulative Distribution Functions for the dissimilarities between all the women and the medoids obtained with the \code{\link{trimowa}} algorithm and for the dissimilarities between all the women and the standard prototypes defined by the European standard. Part 3: Measurements and intervals. In both cases, the dissimilarities have been computed by using the dissimilarity function obtained with \code{\link{getDistMatrix}}. 

These types of plots can also be used to identify the expected range of the dissimilarities, that is to say, the values between the 10 and 90th percentiles. 

This function was used to obtain the Fig. 11 of Ibanez et al. (2012).
}
\usage{
cdfDissWomenPrototypes(min_med,min_med_UNE,main,xlab,ylab,leg,cexLeg,...)
}
\arguments{
\item{min_med}{
Vector with the dissimilarities between all the women and the prototypes (medoids) obtained with \code{\link{trimowa}}.
}
\item{min_med_UNE}{
Vector with the dissimilarities between all the women and the standard prototypes.
}
\item{main}{
A title for the plot.
}
\item{xlab}{
A title for the x axis.
}
\item{ylab}{
A title for the y axis.
}
\item{leg}{
A character vector to appear in the legend.
}
\item{cexLeg}{
Character expansion for the legend.
}
\item{...}{
Further graphical parameters.
}
}
\value{
A device with the desired plot.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{sampleSpanishSurvey}}, \code{\link{weightsMixtureUB}}, \code{\link{trimowa}}, \code{\link{getDistMatrix}}
}
\examples{
#Loading the data to apply the trimowa algorithm:
dataTrimowa <- sampleSpanishSurvey
dim(dataTrimowa)
#[1] 600   5
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
chest <- dataTrimowa$chest
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness,numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

suppressWarnings(RNGversion("3.5.0"))
set.seed(2014)
#numSizes <- bustSizes$nsizes - 1
numSizes <- 2
res_trimowa <- computSizesTrimowa(dataTrimowa, bust, bustSizes$bustCirc, numSizes,
                                  weightsTrimowa, numClust, alpha, niter, algSteps, 
                                  ah, FALSE)                           

#Prototypes obtained with the trimowa algorithm:
prototypes <- anthrCases(res_trimowa, numSizes)
#length(unlist(prototypes)) is (numSizes - 1) * numClust
meds <- dataTrimowa[unlist(prototypes),]

regr <- lm(chest ~ bust)

#Prototypes defined by the European standard:
hip_UNE <- c(seq(84,112,4), seq(117,132,5))          ; hip <- rep(hip_UNE,3)
waist_UNE <- c(seq(60,88,4), seq(94,112,6))          ; waist <- rep(waist_UNE,3)
bust_UNE <- c(seq(76,104,4), seq(110,128,6))         ; bust <- rep(bust_UNE,3)
chest_UNE <- predict(regr, list(bust=bust_UNE))      ; chest <- rep(chest_UNE,3)
necktoground <- c(rep(130,12), rep(134,12),rep(138,12))

medsUNE <- data.frame(chest, necktoground, waist, hip, bust) 
dim(medsUNE)
#[1] 36  5

dataAll <- rbind(dataTrimowa, meds, medsUNE)
dim(dataAll)
#[1] 642   5

bh <- (apply(as.matrix(log(dataAll)),2,range)[2,] 
       - apply(as.matrix(log(dataAll)),2,range)[1,]) / ((numClust-1) * 8) 
bl <- -3 * bh
ah <- c(28,20,30,25,23)
al <- 3 * ah
num.persons <- dim(dataAll)[1]
dataAllm <- as.matrix(dataAll)
dataAllt <- aperm(dataAllm, c(2,1))    
dim(dataAllt) <- c(1,num.persons * numVar)  
rm(dataAllm)
D <- getDistMatrix(dataAllt, num.persons, numVar, weightsTrimowa, bl, bh, al, ah, FALSE)

sequen <- (dim(dataTrimowa)[1] + 1) : (dim(dataTrimowa)[1] + length(unlist(prototypes)))
f <- function(i, D){
 r <- min(D[i, sequen])
}
min_med <- sapply(1:dim(dataTrimowa)[1], f, D) 

sequen1 <- (dim(dataTrimowa)[1] + length(unlist(prototypes)) + 1) : dim(D)[1]
f1 <- function(i, D){
 r <- min(D[i, 619:636])
}
min_med_UNE <- sapply(1:dim(dataTrimowa)[1], f1, D)

#CDF plot:
main <- "Comparison between sizing methods"
xlab <- "Dissimilarity"
ylab <- "Cumulative distribution function"
leg <- c("Dissimilarity between women and computed medoids", 
         "Dissimilarity between women and standard prototypes")
cdfDissWomenPrototypes(min_med, min_med_UNE, main, xlab, ylab, leg,cexLeg = 0.7)
}
\keyword{dplot}
