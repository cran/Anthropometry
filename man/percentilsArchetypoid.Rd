\name{percentilsArchetypoid}
\alias{percentilsArchetypoid}
\title{
Computing percentiles of a certain archetypoid
}
\description{
This function computes the percentiles of an archetypoid for a given variable. Once these percentile values have been calculated, they can be represented by means of a barplot. This function is used inside \code{\link{matPercs}}.
}
\usage{
percentilsArchetypoid(column,indiv,data,digits)
}
\arguments{
\item{column}{
Numeric variable (column of a data frame).
}
\item{indiv}{
A certain archetypoid.
}
\item{data}{
Data frame that contains the columns and archetypoids to be analyzed. 
}
\item{digits}{
Argument of the \code{\link{round}} function (it is a integer indicating the number of decimal places to be used).
}
}
\value{
Numerical vector with the percentile values of an archetypoid. 
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}, \code{\link{matPercs}}
}
\examples{
\dontrun{
#COCKPIT DESIGN PROBLEM:
USAFSurvey_First50 <- USAFSurvey[1 : 50, ]
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_First50_inch <- USAFSurvey_First50[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_First50_inch, TRUE, 0.95, TRUE)

#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; numRep <- 20
lass <- stepArchetypesMod(data = USAFSurvey_preproc$data, numArch=1:numArch, numRep = numRep, 
                          verbose = FALSE)
screeplot(lass)

#Three archetypoids:
numArchoid <- 3
res_ns <- archetypoids(numArchoid, USAFSurvey_preproc$data, huge = 200, step = FALSE, 
                                ArchObj = lass, nearest = "cand_ns" , sequ = TRUE)
 
percentilsArchetypoid(1,res_ns$archet[1],USAFSurvey_preproc$data,0)
}
}
\keyword{math}
