\name{cdfDiss}
\alias{cdfDiss}
\title{
CDF for the dissimilarities between women and computed medoids and standard prototypes
}
\description{
This function allows us to calculate the cumulative distribution functions for the dissimilarities between all the women and the medoids obtained with the \code{\link{trimowa}} algorithm and for the dissimilarities between all the women and the standard prototypes defined by the European standard. Part 3: Measurements and intervals. In both cases, the dissimilarities have been computed by using the dissimilarity function obtained with \code{\link{GetDistMatrix}}. 

These types of plots can also be used to identify the expected range of the dissimilarities, that is to say, the values between the 10 and 90th percentiles. 

This function was used to obtain the Fig. 11 of Ibanez et al. (2012).
}
\usage{
cdfDiss(min_med,min_med_UNE,main,xlab,ylab,leg,cexLeg,...)
}
\arguments{
\item{min_med}{
Vector with the dissimilarities between all the women and the medoids obtained with \code{\link{trimowa}}.
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
\code{\link{dataDemo}}, \code{\link{WeightsMixtureUB}}, \code{\link{trimowa}}, \code{\link{GetDistMatrix}}
}
\examples{
\dontrun{
#Loading the data to apply the trimowa algorithm:
dataDef <- dataDemo
dim(dataDef)
#[1] 600   5
num.variables <- dim(dataDef)[2]
bust <- dataDef$bust
chest <- dataDef$chest

orness <- 0.7
w <- WeightsMixtureUB(orness,num.variables)

bustCirc_4 <- seq(74,102,4)  ; bustCirc_6 <- seq(107,131,6)  ; bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
K <- 3 ; alpha <- 0.01 ; niter <- 6 ; Ksteps <- 7

ahVect <- c(23,28,20,25,25)

res_trimowa <- list()
for (i in 1 : (nsizes-1)){ 
 data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
 res_trimowa[[i]] <- trimowa(data,w,K,alpha,niter,Ksteps,ahVect=ahVect)
}

#Medoids obtained with the trimowa algorithm:
medoids <- list()
for (i in 1 : (nsizes-1)){ 
 medoids[[i]] <- res_trimowa[[i]]$meds
} 
medoids <- unlist(medoids)
length(medoids)
#[1] 36
meds <- dataDef[medoids,]

regr <- lm(chest ~ bust)

#Prototypes defined by the European standard:
hip_UNE <- c(seq(84,112,4),seq(117,132,5))          ; hip <- rep(hip_UNE,3)
waist_UNE <- c(seq(60,88,4),seq(94,112,6))          ; waist <- rep(waist_UNE,3)
bust_UNE <- c(seq(76,104,4),seq(110,128,6))         ; bust <- rep(bust_UNE,3)
chest_UNE <- predict(regr,list(bust=bust_UNE))      ; chest <- rep(chest_UNE,3)
necktoground <- c(rep(130,12),rep(134,12),rep(138,12))

medsUNE <- data.frame(chest,necktoground,waist,hip,bust) 
dim(medsUNE)
#[1] 36  5

dataAll <- rbind(dataDef,meds,medsUNE)
dim(dataAll)
#[1] 672   5

bh <- (apply(as.matrix(log(dataAll)),2,range)[2,] 
       - apply(as.matrix(log(dataAll)),2,range)[1,]) / ((K-1) * 8) 
bl <- -3 * bh
ah <- c(28,20,30,25,23)
al <- 3 * ah
num.persons <- dim(dataAll)[1]
dataAllm <- as.matrix(dataAll)
dataAllt <- aperm(dataAllm, c(2,1))    
dim(dataAllt) <- c(1,num.persons*num.variables)  
rm(dataAllm)
D <- GetDistMatrix(dataAllt,num.persons,num.variables,w,bl,bh,al,ah,T)

f <- function(i, D){
 r <- min(D[i,601:636])
}
min_med <- sapply(1:600, f, D) 

f1 <- function(i, D){
 r <- min(D[i,637:672])
}
min_med_UNE <- sapply(1:600, f1, D)

#CDF plot:
main <- "Comparison between sizing methods"
xlab <- "Dissimilarity"
ylab <- "Cumulative distribution function"
leg <- c("Dissimilarity between women and computed medoids", 
         "Dissimilarity between women and standard prototypes")
cdfDiss(min_med,min_med_UNE,main,xlab,ylab,leg,cexLeg=0.7)
}
}

\keyword{dplot}
