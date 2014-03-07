\name{archetypesBoundary}
\alias{archetypesBoundary}
\title{
Archetypal analysis in multivariate accommodation problem
}
\description{
This function allows us to reproduce the results shown in section 2.2.2 and section 3.1 of Epifanio et al. (2013). In addition, from the results provided by this function, the other results shown in section 3.2 and section 3.3 of the same paper can be also reproduced (see section \emph{examples} below).
}
\usage{
archetypesBoundary(data,numArchet,verbose,nrep)
}
\arguments{
\item{data}{
USAF 1967 database (see \code{\link{dataUSAF}}). Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{numArchet}{
Number of archetypes.
}
\item{verbose}{
Logical value. If TRUE, some details of the execution progress are shown (this is the same argument as that of the \code{\link{stepArchetypes}} function of the \pkg{archetypes} R package (Eugster (2009))).
}
\item{nrep}{
For each archetype run \code{\link{archetypes}} nrep times (this is the same argument as that of the \code{\link{stepArchetypes}} function of \pkg{archetypes}).
}
}
\details{
Before using this function, the more extreme (100 - percAcomm*100)\% observations must be removed by means of the \code{\link{accommodation}} function. To this end, it is recommended that you use the Mahalanobis distance. In this case, the depth procedure has the disadvantage that the desired percentage of accommodation is not under control of the analyst and it could not coincide exactly with that indicated.
}
\value{
A list with \code{numArchet} elements. Each element is a list of class attribute \code{\link{stepArchetypes}} with \code{nrep} elements.
}
\references{
Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \url{http://www.jstatsoft.org/}.

Zehner, G. F., Meindl, R. S., and Hudson, J. A., (1993). A multivariate anthropometric method for crew station design: abridged. Tech. rep. Ohio: Human Engineering Division, Armstrong Laboratory, Wright-Patterson Air Force Base.
}
\note{
We would like to note that, some time after publishing the paper Epifanio et al. (2013), we found out that the \code{\link{stepArchetypes}} function standardizes the data by default (even when the data are already standardized) and this option is not always desired. In order to avoid this way to proceed, we have created the \code{\link{stepArchetypesMod}} function, which is used within \code{\link{archetypesBoundary}} instead of using \code{\link{stepArchetypes}}. Therefore, the results provided by \code{\link{archetypesBoundary}} allows us to reproduce the results of Epifanio et al. (2013) but they are now slightly different.
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\code{\link{archetypes}}, \code{\link{stepArchetypes}}, \code{\link{stepArchetypesMod}}, \code{\link{dataUSAF}}, \code{\link{indivNearest}}, \code{\link{accommodation}}
}
\examples{
\dontrun{
#The following R code allows us to reproduce the results of the paper Epifanio et al. (2013).
#First,the USAF 1967 database is read and preprocessed (Zehner et al. (1993)).
m <- dataUSAF
#Variable selection:
sel <- c(48,40,39,33,34,36)
#Changing to inches: 
mpulg <- m[,sel] / (10 * 2.54)

#Data preprocessing:
preproc <- accommodation(mpulg,TRUE,0.95,TRUE) 

#Procedure and results shown in section 2.2.2 and section 3.1:
res <- archetypesBoundary(preproc$data,15,FALSE,3)

#Results shown in section 3.2 (figure 3):
screeplot(res) 

#3 archetypes:
a3 <- archetypes::bestModel(res[[3]])
archetypes::parameters(a3)
#7 archetypes:
a7 <- archetypes::bestModel(res[[7]])
archetypes::parameters(a7) 
#Plotting the percentiles of each archetype:
#Figure 2 (b):
barplot(a3,preproc$data,percentiles=T,which="beside") 
#Figure 2 (f):
barplot(a7,preproc$data,percentiles=T,which="beside")

#Results shown in section 3.3 related with PCA.
pznueva <- prcomp(preproc$data,scale=T,retx=T) 
#Table 3:
summary(pznueva)
pznueva
#PCA scores for 3 archetypes:
p3 <- predict(pznueva,archetypes::parameters(a3)) 
#PCA scores for 7 archetypes:
p7 <- predict(pznueva,archetypes::parameters(a7))
#Representing the scores:
#Figure 4 (a):
xyplotPCA(p3[,1:2],pznueva$x[,1:2],data.col=gray(0.7),atypes.col=1,atypes.pch=15)
#Figure 4 (b):
xyplotPCA(p7[,1:2],pznueva$x[,1:2],data.col=gray(0.7),atypes.col=1,atypes.pch=15)

#Percentiles for 7 archetypes (table 5):
Fn <- ecdf(preproc$data)
round(Fn(archetypes::parameters(a7)) * 100)

#Which are the nearest individuals to archetypes?:
#Example for three archetypes:
ras <- rbind(archetypes::parameters(a3),preproc$data)
dras <- dist(ras,method="euclidean",diag=F,upper=T,p=2)
mdras <- as.matrix(dras)
diag(mdras) = 1e+11
i <- 3
nearest <- sapply(1:i,indivNearest,i,mdras)

#In addition, we can turn the standardized values to the original variables.
p <- archetypes::parameters(a7)
m <- sapply(mpulg,mean)
s <- sapply(mpulg,sd)
d <- p
for(i in 1 : 6){
 d[,i] = p[,i] * s[i] + m[i]
}
#Table 7:
t(d)
}
}
\keyword{array}