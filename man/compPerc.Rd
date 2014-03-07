\name{compPerc}
\alias{compPerc}
\title{
Computing percentiles of a certain archetypoid
}
\description{
This function computes the percentiles of an archetypoid for a given variable. Once these percentile values have been calculated, they can be represented by means of a barplot.
}
\usage{
compPerc(column,indiv,data,digits)
}
\arguments{
\item{column}{
Numeric variable (column of a data frame).
}
\item{indiv}{
A certain archetypoid.
}
\item{data}{
Data frame that contains the columns to be analyzed. 
}
\item{digits}{
Argument of the \code{\link{round}} function (it is a integer indicating the number of decimal places to be used).
}
}
\value{
Numerical vector with the percentile values of an archetypoid. 
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. Submitted for publication.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}
}
\examples{
\dontrun{
#Cockpit design problem:
m <- dataUSAF
sel <- c(48,40,39,33,34,36)
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 20
lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,nrep=nrep)  

#Three archetypoids:
i <- 3
res <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,nearest=TRUE,sequ=TRUE)
aux <- res$archet

percs <- list()
for(j in 1:length(aux)){
 percs[[j]] <- sapply(1:dim(preproc$data)[2],compPerc,aux[j],preproc$data,0)
}
m <- matrix(unlist(percs),nrow=6,ncol=length(percs),byrow=F)

barplot(m,beside=TRUE,main=paste(i," archetypoids (from nearest)",sep = ""),
        ylim=c(0,100),ylab="Percentile")
}
}
\keyword{math}
