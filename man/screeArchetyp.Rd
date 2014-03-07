\name{screeArchetyp}
\alias{screeArchetyp}
\title{
Screeplot of archetypes and archetypoids
}
\description{
This function allows us to represent in the same plot the screeplot of the archetypes and the both \emph{nearest} and \emph{which} archetypoids.
}
\usage{
screeArchetyp(k,rss_lass_def,rss_step,rss_step_which,ylim,main,
              xlab,ylab,col=c("red","blue"),axis2,seq,leg)
}
\arguments{
\item{k}{
Number of archetypes and archetypoids.
}
\item{rss_lass_def}{
Vector of the residual sum of squares (rss) associated with each archetype from 1 to \code{k}.
}
\item{rss_step}{
Vector of the residual sum of squares (rss) associated with each \emph{nearest} archetypoid from 1 to \code{k}.
}
\item{rss_step_which}{
Vector of the residual sum of squares (rss) associated with each \emph{which} archetypoid from 1 to \code{k}.
}
\item{ylim}{
The y limits of the plot.
}
\item{main}{
Title of the plot.
}
\item{xlab}{
A title for the x axis.  
}  
\item{ylab}{
A title for the y axis.  
}  
\item{col}{
Color vector for the screeplots of the archetypoids. Default is c("red","blue").
}  
\item{axis2}{
A logical value. If TRUE, the y axis can be customized to have spaced tick-marks by means of the following argument \code{seq}.
}
\item{seq}{
Vector sequence with the values of the tick-marks to be drawn in the y axis.
}
\item{leg}{
If TRUE, a legend is shown.
}
}
\value{
A device with the desired plot.
}
\references{
Vinue, G., Epifanio, I., and Alemany, S., (2014). Archetypoids: a new approach to define representative archetypal data. \emph{Submitted for publication}.

Cutler, A., and Breiman, L., (1994). Archetypal Analysis, \emph{Technometrics} \bold{36}, 338--347.

Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Eugster, M. J. A., (2012). Performance profiles based on archetypal athletes, \emph{International Journal of Performance Analysis in Sport} \bold{12}, 166--187.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{archetypoids}}, \code{\link{stepArchetypoids}}
}
\examples{
\dontrun{
#COCKPIT DESIGN PROBLEM:
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE)

#Computation of archetypes and archetypoids:
#For reproducing results, seed for randomness:
set.seed(2010) 
#Run archetypes algorithm repeatedly from 1 to numArch archetypes:
numArch <- 10 ; nrep <- 20
lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,nrep=nrep)  

rss_lass <- matrix(0,nrow=numArch,ncol=nrep)
for(i in 1:numArch){
  for(j in 1:nrep){
    rss_lass[i,j] <- lass[[i]][[j]]$rss
  }
}
(rss_lass_def <- apply(rss_lass,1,min,na.rm=T))

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
for(i in 1:numArch){
  temp <-  stepArchetypoids(i,TRUE,preproc$data,lass)
  filename <- paste("res", i, sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep="")) 
}  

for(i in 1:numArch){
  temp <-  stepArchetypoids(i,FALSE,preproc$data,lass)
  filename <- paste("res", i, "_which",sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep=""))
}

#Numerical and graphical results:
for(i in 1:numArch){
  load(paste("res", i, ".RData", sep = ""))
}
rss_step <- c()
for (i in 1:numArch){
  rss_step[i] <- get(paste("res", i, sep = ""))[[2]]
}
(rss_step <- as.numeric(rss_step))


for(i in 1:numArch){
  load(paste("res", i, "_which.RData", sep = ""))
}
rss_step_which <- c()
for (i in 1:numArch){
  rss_step_which[i] <- get(paste("res", i, "_which", sep = ""))[[2]]
}
(rss_step_which <- as.numeric(rss_step_which))

main <- "Aircraft pilots archetypes and archetypoids" 
xlab <- "Archetypes/Archetypoids"
ylab <- "RSS"
screeArchetyp(numArch,rss_lass_def,rss_step,rss_step_which,c(0.005,0.040),
              main,xlab,ylab,col=c("red","blue"),TRUE,c(0.005,0.040),TRUE)
}
}
\keyword{math}
