\name{archetypoids}
\alias{archetypoids}
\title{
Finding archetypoids
}
\description{
Archetypoid algorithm. It is based on the PAM clustering algorithm. It is made up of two phases (a BUILD phase and a SWAP phase). In the BUILD phase, an initial set of archetypoids is determined. Unlike PAM, this collection is not derived in a stepwise format. Instead, it is suggested you choose the set made up of the nearest individuals returned by the \code{\link{archetypes}} function of the \pkg{archetypes} R package (Eugster et al. (2009)). This set can be defined in two different ways, see next section \emph{arguments}. The goal of the SWAP step is the same as that of the SWAP step of PAM, but changing the objective function. The initial vector of archetypoids is attempted to be improved. This is done by exchanging selected individuals for unselected individuals and by checking whether these replacements reduce the objective function of the archetypoid analysis problem. 

More details are given in Vinue et al. (2014) (submitted).
}
\usage{
archetypoids(i,data,huge=200,step,init,ArchObj,nearest,sequ,aux)
}
\arguments{
\item{i}{
Number of archetypoids.
}
\item{data}{
Data matrix. Each row corresponds to an observation and each column corresponds to an anthropometric variable. All variables are numeric.
}
\item{huge}{
This is a penalization added to solve the convex least squares problems regarding the minimization problem to estimate archetypoids, see Eugster et al. (2009). Default value is 200.
}
\item{step}{
Logical value. If TRUE, the archetypoid algorithm is executed repeatedly within \code{\link{stepArchetypoids}}. Therefore, this function requires the next argument \code{init} (but neither the \code{ArchObj} nor the \code{nearest} arguments) that specifies the initial vector of archetypoids, which has been already computed within \code{\link{stepArchetypoids}}. If FALSE, the archetypoid algorithm is executed once. In this case, the \code{ArchObj} and \code{nearest} arguments are required to compute the initial vector of archetypoids.
}
\item{init}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. It is computed within \code{\link{stepArchetypoids}}. See next \code{nearest} argument to know how this vector is calculated.
}
\item{ArchObj}{
The list returned by the \code{\link{stepArchetypesMod}} function. This function is a slight modification of the original \code{\link{stepArchetypes}} function of \pkg{archetypes} to apply the archetype algorithm to raw data. The \code{\link{stepArchetypes}} function standardizes the data by default and this option is not always desired. This list is needed to compute the nearest individuals to archetypes. Required when \code{step=FALSE}.
}
\item{nearest}{
Initial vector of archetypoids for the BUILD phase of the archetypoid algorithm. Required when \code{step=FALSE}. This argument is a logical value: if TRUE (FALSE), the \emph{nearest} (\emph{which}) vector is calculated. Both vectors contain the nearest individuals to the archetypes returned by the \code{\link{archetypes}} function of \pkg{archetypes} (In Vinue et al. (2014), archetypes are computed after running the archetype algorithm twenty times). The \emph{nearest} vector is calculated by computing the Euclidean distance between the archetypes and the individuals and choosing the nearest. It is used in Epifanio et al. (2013). The \emph{which} vector is calculated by identifying consecutively the individual with the maximum value of alpha for each archetype, until getting the number of archetypes defined. It is used in Eugster (2012). 
}
\item{sequ}{
Logical value. It indicates whether a sequence of archetypoids (TRUE) or only a single number of them (FALSE) is computed. It is determined by the number of archetypes computed by means of \code{\link{stepArchetypesMod}}.
}
\item{aux}{
If \code{sequ}=FALSE, this value is equal to \code{i}-1 since for a single number of archetypoids, the list associated with the archetype object only has one element.
}
}
\details{
As mentioned, this algorithm is based on PAM. These types of algorithms aims to find good solutions in a short period of time, although not necessarily the best solution. Otherwise, the global minimum solution may always be obtained using as much time as it would be necessary, but this would be very inefficient computationally.
}
\value{
A list with the following elements:

\emph{archet}: Final vector of k archetypoids.

\emph{rss}: Residual sum of squares corresponding to the final vector of k archetypoids.

\emph{archet_ini}: Vector of initial archetypoids (\emph{nearest} or \emph{which}).
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. \emph{Submitted for publication}.

Cutler, A., and Breiman, L., (1994). Archetypal Analysis, \emph{Technometrics} \bold{36}, 338--347.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Eugster, M. J. A., (2012). Performance profiles based on archetypal athletes, \emph{International Journal of Performance Analysis in Sport} \bold{12}, 166--187.
}
\note{
It may be happen that \code{\link{archetypes}} does not find results for \emph{k} archetypes. In this case, it is not possible to calculate the vector of nearest individuals and consequently, the vector of archetypoids. Therefore, this function will return an error message. 
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\code{\link{stepArchetypesMod}}, \code{\link{archetypes}}, \code{\link{stepArchetypoids}}
}
\examples{
\dontrun{
#SPORTIVE EXAMPLE:
#Database:
if(nzchar(system.file(package = "SportsAnalytics"))){
 data("NBAPlayerStatistics0910", package = "SportsAnalytics")
}      
mat <- NBAPlayerStatistics0910[,c("TotalMinutesPlayed","FieldGoalsMade")]
rownames(mat) <- NULL

#Calculating archetypes by using the archetype algorithm:
#Data preprocessing:
preproc <- accommodation(mat,stand=TRUE,percAccomm=1)

#For reproducing results, seed for randomness:
set.seed(4321)
#Run archetype algorithm repeatedly from 1 to 15 archetypes:
lass15 <- stepArchetypesMod(data=preproc,k=1:15,verbose=FALSE,nrep=20)
screeplot(lass15) 

#Calculating real archetypes:
i <- 3 #number of archetypoids.
res <- archetypoids(i,preproc,huge=200,step=FALSE,ArchObj=lass15,nearest=TRUE,sequ=TRUE)
arquets <- NBAPlayerStatistics0910[res[[1]],c("Name","TotalMinutesPlayed","FieldGoalsMade")]
res_which <- archetypoids(i,preproc,huge=200,step=FALSE,ArchObj=lass15,nearest=FALSE,sequ=TRUE)
arquets_eug <- NBAPlayerStatistics0910[res_which[[1]],
                  c("Name","TotalMinutesPlayed","FieldGoalsMade")]

col_pal <- RColorBrewer::brewer.pal(7, "Set1")
col_black <- rgb(0, 0, 0, 0.2)

plot(mat, pch = 1, col = col_black, xlim = c(0,3500), main = "NBA archetypal basketball         
     players \n obtained in Eugster (2012) \n and with our proposal", 
     xlab = "Total minutes played", ylab = "Field goals made")
points(mat[as.numeric(rownames(arquets)),], pch = 4, col = col_pal[1]) 
points(mat[as.numeric(rownames(arquets_eug)),], pch = 4, col = col_pal[1]) 
text(mat[as.numeric(rownames(arquets_eug)),][2,1], 
     mat[as.numeric(rownames(arquets_eug)),][2,2], 
     labels = arquets_eug[2,"Name"], pos = 4, col = "blue")
plotrix::textbox(c(50,800), 50, "Travis Diener") 
plotrix::textbox(c(2800,3500), 780, "Kevin Durant", col = "blue")
plotrix::textbox(c(2800,3500), 270, "Jason Kidd", col = "blue")
legend("topleft",c("archetypes of Eugster","archetypes of our proposal"), 
       lty= c(1,NA), pch = c(NA,22), col = c("blue","black"))


#If a specific number of archetypes is computed only:
i=3
set.seed(4321)
lass3 <- stepArchetypesMod(data=preproc,k=i,verbose=FALSE,nrep=3)
res3 <- archetypoids(i,preproc,huge=200,step=FALSE,ArchObj=lass3,nearest=TRUE,sequ=FALSE,aux=2)
arquets3 <- NBAPlayerStatistics0910[res3[[1]],c("Name","TotalMinutesPlayed","FieldGoalsMade")]


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
#Run archetype algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 20
lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,nrep=nrep)  
screeplot(lass)

i <- 3 #number of archetypoids.
res <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,nearest=TRUE,sequ=TRUE)
res_which <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,nearest=FALSE,sequ=TRUE)
}
}
\keyword{array}