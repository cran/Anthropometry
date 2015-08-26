\name{matPercs}
\alias{matPercs}
\title{
Helper function for the percentils of the archetypoids
}
\description{
This is a helper function for computing the percentils of the archetypoids. Once the percentils of an archetypoid for a given variable are calculated using the \code{\link{percentilsArchetypoid}} function, this function saves these percentile values in a matrix to be represented by means of a barplot.
}
\usage{
matPercs(archoids,data)
}
\arguments{
\item{archoids}{
Vector of archetypoids for which computing their percentils.
}
\item{data}{
Data frame that contains the anthropometric numeric variables and archetypoids to be analyzed. 
}
}
\value{
Numerical matrix with the percentile values of an archetypoid. 
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data. \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}, \code{\link{percentilsArchetypoid}}
}
\examples{
\dontrun{
#COCKPIT DESIGN PROBLEM:
USAFSurvey_First50 <- USAFSurvey[1 : 50, ]
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches:
USAFSurvey_First50_inch <- USAFSurvey_First50[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_First50_inch, TRUE, 0.95, TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; numRep <- 20
lass <- stepArchetypesRawData(data=USAFSurvey_preproc$data, 
                          numArch=1:numArch, numRep=numRep, 
                          verbose=FALSE)  

#Three archetypoids:
numArchoid <- 3 #number of archetypoids.
res_ns <- archetypoids(numArchoid,USAFSurvey_preproc$data,huge=200,step=FALSE,
                       ArchObj=lass,nearest="cand_ns",sequ=TRUE)

boundaries_ns <- anthrCases(res_ns)

matPer <- matPercs(boundaries_ns, USAFSurvey_preproc$data)

barplot(matPer, beside = TRUE, main = paste(numArchoid," archetypoids", sep = ""),
        ylim = c(0, 100), ylab = "Percentile")
}
}
\keyword{math}
