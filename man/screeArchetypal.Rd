\name{screeArchetypal}
\alias{screeArchetypal}
\title{
Screeplot of archetypal individuals
}
\description{
This function allows us to represent in the same plot the screeplot of the archetypes and the both \emph{cand_ns}, \emph{cand_alpha} and \emph{cand_beta} archetypoids.
}
\usage{
screeArchetypal(numArch,rss_lass_def,rss_step_ns,rss_step_alpha,rss_step_beta,
                ylim,main,xlab,ylab,col=c("red","blue","green3"),axis2,seq,leg)
}
\arguments{
\item{numArch}{
Number of archetypal observations (archetypes and archetypoids).
}
\item{rss_lass_def}{
Vector of the residual sum of squares (rss) associated with each archetype from 1 to \code{numArch}.
}
\item{rss_step_ns}{
Vector of the residual sum of squares (rss) associated with each \emph{cand_ns} archetypoid from 1 to \code{numArch}.
}
\item{rss_step_alpha}{
Vector of the residual sum of squares (rss) associated with each \emph{cand_alpha} archetypoid from 1 to \code{numArch}.
}
\item{rss_step_beta}{
Vector of the residual sum of squares (rss) associated with each \emph{cand_beta} archetypoid from 1 to \code{numArch}.
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
Color vector for the screeplots of the archetypoids. Default is c("red","blue","green3").
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
Vinue, G., Epifanio, I., and Alemany, S., (2015). Archetypoids: a new approach to define representative archetypal data, \emph{Computational Statistics and Data Analysis} \bold{87}, 102--115.

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
#The following R code allows us to obtain a similar plot regarding Figure 5 
#of the paper Vinue et al. (2015).
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
lass <- stepArchetypesRawData(data = USAFSurvey_preproc$data, 
                          numArch=1:numArch, numRep = numRep, 
                          verbose = FALSE)
  
rss_lass <- matrix(0,nrow=numArch,ncol=numRep)
for(i in 1:numArch){
  for(j in 1:numRep){
    rss_lass[i,j] <- lass[[i]][[j]]$rss
  }
}
(rss_lass_def <- apply(rss_lass,1,min,na.rm=T))

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
for(numArchoid in 1:numArch){
  temp <-  stepArchetypoids(numArchoid,nearest="cand_ns",USAFSurvey_preproc$data,lass)
  filename <- paste("res_ns", numArchoid, sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep="")) 
}   

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
for(numArchoid in 1:numArch){
  temp <-  stepArchetypoids(numArchoid,nearest="cand_alpha",USAFSurvey_preproc$data,lass)
  filename <- paste("res_alpha", numArchoid, sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep="")) 
}  

#Run archetypoids algorithm repeatedly from 1 to numArch archetypes:
for(numArchoid in 1:numArch){
  temp <-  stepArchetypoids(numArchoid,nearest="cand_beta",USAFSurvey_preproc$data,lass)
  filename <- paste("res_beta", numArchoid, sep="")
  assign(filename,temp)
  save(list=c(filename),file=paste(filename, ".RData", sep="")) 
}  

#Numerical and graphical results:
#Cand_ns:
for(i in 1:numArch){
  load(paste("res_ns", i, ".RData", sep = ""))
}
rss_step <- c()
for (i in 1:numArch){
  rss_step[i] <- get(paste("res_ns", i, sep = ""))[[2]]
}
(rss_step_ns <- as.numeric(rss_step))

#Cand_alpha:
for(i in 1:numArch){
  load(paste("res_alpha", i, ".RData", sep = ""))
}
rss_step_which <- c()
for (i in 1:numArch){
  rss_step_which[i] <- get(paste("res_alpha", i, sep = ""))[[2]]
}
(rss_step_alpha <- as.numeric(rss_step_which))

#Cand_beta:
for(i in 1:numArch){
  load(paste("res_beta", i, ".RData", sep = ""))
}
rss_step_which <- c()
for (i in 1:numArch){
  rss_step_which[i] <- get(paste("res_beta", i, sep = ""))[[2]]
}
(rss_step_beta <- as.numeric(rss_step_which))

forYlim <- c(rss_lass_def,rss_step_ns,rss_step_alpha,rss_step_beta)
range(forYlim)
#[1] 0.03370146 0.27395811

#main <- "Aircraft pilots archetypes and archetypoids" 
xlab <- "Archetypes/Archetypoids"
ylab <- "RSS"
screeArchetypal(numArch,rss_lass_def,rss_step_ns,rss_step_alpha,rss_step_beta,c(0,0.3),
                main="",xlab,ylab,col=c("red","blue","green3"),TRUE,seq(0,0.3,0.1),TRUE)
}
}
\keyword{math}
