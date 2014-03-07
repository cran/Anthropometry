\name{accommodation}
\alias{accommodation}
\title{
Data preprocessing before computing archetypes
}
\description{
This function allows us to preprocess the data before computing archetypes and archetypoids. First, depending on the
problem, it is possible to standardize the data or not. Second, it is possible to use the Mahalanobis distance or a depth procedure to select the accommodated subsample of data.
}
\usage{
accommodation(dataRaw,stand,percAccomm,mahal=TRUE)
}
\arguments{
\item{dataRaw}{
Raw data. Each row corresponds to an observation and each column corresponds to an anthropometric variable. All variables are numeric.
}
\item{stand}{
A logical value. If TRUE (FALSE) the data are (not) standardized. This option will depend on the problem.
}
\item{percAccomm}{
Percentage of the population to accommodate (value between 0 and 1). When this percentage is equal to 1 all the individuals will be accommodated.
}
\item{mahal}{
If percAccom is different from 1, then \code{mahal=TRUE} (\code{mahal=FALSE}) indicates that the Mahalanobis distance (a depth procedure) will be used to select the accommodated subsample of data. 
}
}
\details{
In somes cases, the depth procedure has the disadvantage that the desired percentage of accommodation is not under control of the analyst and it could not coincide exactly with \code{percAccomm}.
}
\value{
A list with the following elements:

\emph{data}: Database after preprocessing.

\emph{indivYes}: Individuals who belong to \code{data}.

\emph{indivNo}: Individuals discarded in the accommodation procedure.
}
\references{
Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Genest, M., Masse, J.-C., and Plante, J.-F., (2012). \pkg{depth}: Depth functions tools for multivariate analysis. R package version 2.0-0.
}
\author{
Irene Epifanio and Guillermo Vinue
}
\examples{
\dontrun{
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data pre-processing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE) 
preproc <- accommodation(mpulg,TRUE,0.95,FALSE) 
}
}
\keyword{array}