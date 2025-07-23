\name{archetypoids}
\alias{archetypoids}
\title{
Finding archetypoids
}
\description{
Archetypoid algorithm. It is based on the PAM clustering algorithm. It is made up of two phases (a BUILD phase and a SWAP phase). In the BUILD phase, an initial set of archetypoids is determined. Unlike PAM, this collection is not derived in a stepwise format. Instead, it is suggested you choose the set made up of the nearest individuals returned by the \link[archetypes]{archetypes} function (Eugster et al. (2009)). This set can be defined in three different ways, see next section \emph{arguments}. The goal of the SWAP step is the same as that of the SWAP step of PAM, but changing the objective function. The initial vector of archetypoids is attempted to be improved. This is done by exchanging selected individuals for unselected individuals and by checking whether these replacements reduce the objective function of the archetypoid analysis problem. 

All details are given in Vinue et al. (2015).
}
\usage{
archetypoids(numArchoid,data,huge=200,step,init,ArchObj,nearest="cand_ns",sequ,aux)
}
\arguments{
\item{numArchoid}{
Number of archetypoids (archetypal observations).
}
\item{data}{
Data matrix. Each row corresponds to an observation and each column corresponds to an anthropometric variable. All variables are numeric.
}
\item{huge}{
This is a penalization added to solve the convex least squares problems regarding the minimization problem to estimate archetypoids, see Eugster et al. (2009). Default value is 200.
}
\item{step}{
Logical value. If TRUE, the archetypoid algorithm is executed repeatedly within \code{\link{stepArchetypoids}}. Therefore, this function requires the next argument \code{init} (but neither the \code{ArchObj} nor the \code{nearest} arguments) that specifies the initial vector of archetypoids, which has already been computed within \code{\link{stepArchetypoids}}. If FALSE, the archetypoid algorithm is executed once. In this case, the \code{ArchObj} and \code{nearest} arguments are required to compute the initial vector of archetypoids.
}
\item{init}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. It is computed within \code{\link{stepArchetypoids}}. See \code{nearest} argument below for an explanation of how this vector is calculated.
}
\item{ArchObj}{
The list object returned by the \code{\link{stepArchetypesRawData}} function. This function is a slight modification of the original \link[archetypes]{stepArchetypes} to apply the archetype algorithm to raw data. The \link[archetypes]{stepArchetypes} function standardizes the data by default and this option is not always desired. This list is needed to compute the nearest individuals to archetypes. Required when \code{step=FALSE}.
}
\item{nearest}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. Required when \code{step=FALSE}. This initial vector contain the nearest individuals to the archetypes returned by the \link[archetypes]{archetypes} (In Vinue et al. (2015), archetypes are computed after running the archetype algorithm twenty times). This argument is a string vector with three different possibilities. The first and default option is "cand_ns" and allows us to calculate the nearest individuals by computing the Euclidean distance between the archetypes and the individuals and choosing the nearest. It is used in Epifanio et al. (2013). The second option is "cand_alpha" and allows us to calculate the nearest individuals by consecutively identifying the individual with the maximum value of alpha for each archetype, until the defined number of archetypes is reached. It is used in Eugster (2012). The third and final option is "cand_beta" and allows us to calculate the nearest individuals by identifying the individuals with the maximum beta value for each archetype, i.e. the major contributors in the generation of the archetypes.
}
\item{sequ}{
Logical value. It indicates whether a sequence of archetypoids (TRUE) or only a single number of them (FALSE) is computed. It is determined by the number of archetypes computed by means of \code{\link{stepArchetypesRawData}}.
}
\item{aux}{
If \code{sequ}=FALSE, this value is equal to \code{numArchoid}-1 since for a single number of archetypoids, the list associated with the archetype object only has one element.
}
}
\details{
As mentioned, this algorithm is based on PAM. These types of algorithms aim to find good solutions in a short period of time, although not necessarily the best solution. Otherwise, the global minimum solution may always be obtained using as much time as it would be necessary, but this would be very inefficient computationally.
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
It may be happen that \link[archetypes]{archetypes} does not find results for \code{numArchoid} archetypes. In this case, it is not possible to calculate the vector of nearest individuals and consequently, the vector of archetypoids. Therefore, this function will return an error message. 
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\code{\link{stepArchetypesRawData}}, \link[archetypes]{archetypes}, \code{\link{stepArchetypoids}}
}
\examples{
#Note: For a sportive example, see www.uv.es/vivigui/softw/more_examples.R 

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
numArch <- 5 ; numRep <- 2
lass <- stepArchetypesRawData(data = USAFSurvey_preproc$data, numArch = 1:numArch,
                              numRep = numRep, verbose = FALSE)  
#To understand the warning messages, see the vignette of the
#archetypes package.                              

#screeplot(lass)

numArchoid <- 3 #number of archetypoids.
res_ns <- archetypoids(numArchoid, USAFSurvey_preproc$data, huge = 200, step = FALSE,
                       ArchObj = lass, nearest = "cand_ns",sequ = TRUE)
}
\keyword{array}
