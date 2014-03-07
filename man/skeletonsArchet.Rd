\name{skeletonsArchet}
\alias{skeletonsArchet}
\title{
Skeleton plots of archetypal individuals
}
\description{
This function represents the skeleton plots of the archetypal observations (archetypes and archetypoids) of \code{\link{dataUSAF}}.
}
\usage{
skeletonsArchet(vect,main)
}
\arguments{
\item{vect}{
Vector with the measurements of each archetype.
}
\item{main}{
The title of the plot.
}
}
\value{
A device with the desired plot.
}
\references{
Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\note{
This function allows us to reproduce the archetypes of Figure 5 of Epifanio et al. (2013), see \code{\link{archetypesBoundary}}.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{archetypesBoundary}}, \code{\link{dataUSAF}}
}
\examples{
\dontrun{
#List with the measurements of each archetype (Table 7 of Epifanio et al (2013)):
lista_arch <- list()
lista_arch[[1]] <- c(34.18, 25.85, 18.65, 39.66, 35.05, 26.73)
lista_arch[[2]] <- c(28.51, 21.23, 15.39, 33.57, 29.24, 21.26)
lista_arch[[3]] <- c(35.34, 24.94, 18.79, 36.7, 32.28, 23.41)
lista_arch[[4]] <- c(31.34, 22.27, 16.89, 38, 33.08, 25.8)
lista_arch[[5]] <- c(32.33, 25.09, 17.84, 34.46, 29.58, 22.82)
lista_arch[[6]] <- c(29.69, 24.18, 18.22, 38.07, 33.04, 24.56)
lista_arch[[7]] <- c(29.24, 22.97, 14.99, 36.88, 32.28, 24.22)

for(i in 1:length(lista_arch)){
 main <- paste("Archetype", i, sep = " ")
 skeletonsArchet(lista_arch[[i]],main)
}

#AN EXAMPLE FOR THE ARCHETYPOIDS:
#COCKPIT DESIGN PROBLEM:
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetypes algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10
nrep <- 20
lass <- stepArchetypesMod(data=preproc$data,k=1:10,verbose=FALSE,nrep=20)  

i <- 3 #number of archetypoids.
res <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,nearest=TRUE,sequ=TRUE)

#Looking for the individuals in the non standardized database: 
aux <- mpulg[setdiff(1:dim(mpulg)[1],preproc$indivNo),]
rownames(aux) <- 1:dim(preproc$data)[1]

skeletonsArchet(aux[res[[1]][1],],"Archetypoid 1")
}
}
\keyword{array}
