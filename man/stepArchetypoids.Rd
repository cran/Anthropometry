\name{stepArchetypoids}
\alias{stepArchetypoids}
\title{
Run the archetypoid algorithm several times
}
\description{
Execute the archetypoid algorithm repeatedly. It is inspired by the \code{\link{stepArchetypes}} function of the \pkg{archetypes} R package.
}
\usage{
stepArchetypoids(numArchoid,nearest="cand_ns",data,ArchObj)
}
\arguments{
\item{numArchoid}{
Number of archetypoids.
}
\item{nearest}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. This initial vector contain the nearest individuals to the archetypes returned by the \code{\link{archetypes}} function of \pkg{archetypes} (In Vinue et al. (2015), archetypes are computed after running the archetype algorithm twenty times). This argument is a string vector with three different possibilities. The first and default option is "cand_ns" and allows us to calculate the nearest individuals by computing the Euclidean distance between the archetypes and the individuals and choosing the nearest. It is used in Epifanio et al. (2013). The second option is "cand_alpha" and allows us to calculate the nearest individuals by consecutively identifying the individual with the maximum value of alpha for each archetype, until the defined number of archetypes is reached. It is used in Eugster (2012). The third and final option is "cand_beta" and allows us to calculate the nearest individuals by identifying the individuals with the maximum beta value for each archetype, i.e. the major contributors in the generation of the archetypes.
}
\item{data}{
Data matrix. Each row corresponds to an observation and each column corresponds to an anthropometric variable. All variables are numeric.
}
\item{ArchObj}{
The list object returned by the \code{\link{stepArchetypesRawData}} function. This function is a slight modification of the original \code{\link{stepArchetypes}} function of \pkg{archetypes} to apply the archetype algorithm to raw data. The \code{\link{stepArchetypes}} function standardizes the data by default and this option is not always desired. This list is needed to compute the nearest individuals to archetypes. 
}
}
\value{
A list with the following elements:

\emph{cases}: Anthropometric cases (final vector of \code{numArchoid} archetypoids).

\emph{rss}: Residual sum of squares corresponding to the final vector of \code{numArchoid} archetypoids.

\emph{archet_ini}: Vector of initial archetypoids (\emph{cand_ns}, \emph{cand_alpha} or \emph{cand_beta}).

\emph{alphas}: Alpha coefficients for the optimal vector of archetypoids.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

Cutler, A., and Breiman, L., (1994). Archetypal Analysis, \emph{Technometrics} \bold{36}, 338--347.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \doi{10.18637/jss.v030.i08}.

Eugster, M. J. A., (2012). Performance profiles based on archetypal athletes, \emph{International Journal of Performance Analysis in Sport} \bold{12}, 166--187.
}
\note{
It may be happen that \code{\link{archetypes}} does not find results for \emph{k} archetypes. In this case, it is not possible to calculate the vector of nearest individuals and consequently, the vector of archetypoids. Therefore, this function will return an error message. 
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}, \code{\link{archetypes}}, \code{\link{stepArchetypes}}
}
\examples{
#COCKPIT DESIGN PROBLEM:
#As a toy example, only the first 25 individuals are used.
USAFSurvey_First25 <- USAFSurvey[1:25, ]
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_First25_inch <- USAFSurvey_First25[,variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_First25_inch, TRUE, 0.95, TRUE)

#For reproducing results, seed for randomness:
#suppressWarnings(RNGversion("3.5.0"))
#set.seed(2010) 
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
#This is a toy example. In other situation, choose numArch=10 and numRep=20.
numArch <- 2 ; numRep <- 2
lass <- stepArchetypesRawData(data = USAFSurvey_preproc$data, numArch = 1:numArch,
                              numRep = numRep, verbose = FALSE) 
#To understand the warning messages, see the vignette of the
#archetypes package.                                

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
#for(numArchoid in 1:numArch){
# temp <-  stepArchetypoids(numArchoid,nearest="cand_ns",USAFSurvey_preproc$data,lass)
# filename <- paste("res", numArchoid, sep="")
# assign(filename,temp)
# save(list=c(filename),file=paste(filename, ".RData", sep="")) 
#}
temp <-  stepArchetypoids(2,nearest="cand_ns",USAFSurvey_preproc$data,lass)
}
\keyword{array}
