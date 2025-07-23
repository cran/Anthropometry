\name{archetypesBoundary}
\alias{archetypesBoundary}
\title{
Archetypal analysis in multivariate accommodation problem
}
\description{
This function allows us to reproduce the results shown in section 2.2.2 and section 3.1 of Epifanio et al. (2013). In addition, from the results provided by this function, the other results shown in section 3.2 and section 3.3 of the same paper can be also reproduced (see section \emph{examples} below).
}
\usage{
archetypesBoundary(data,numArch,verbose,numRep)
}
\arguments{
\item{data}{
USAF 1967 database (see \code{\link{USAFSurvey}}). Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{numArch}{
Number of archetypes (archetypal observations).
}
\item{verbose}{
Logical value. If TRUE, some details of the execution progress are shown (this is the same argument as that of the \link[archetypes]{stepArchetypes} function (Eugster (2009))).
}
\item{numRep}{
For each archetype run \link[archetypes]{archetypes} \code{numRep} times (this is the same argument as the \code{nrep} argument of \link[archetypes]{stepArchetypes}.
}
}
\details{
Before using this function, the more extreme (100 - \code{percAcomm}*100)\% observations must be removed by means of the \code{\link{preprocessing}} function. To that end, it is recommended that you use the Mahalanobis distance. In this case, the depth procedure has the disadvantage that the desired percentage of accommodation is not under control of the analyst and it may not exactly coincide with that one indicated.
}
\value{
A list with \code{numArch} elements. Each element is a list of class attribute \link[archetypes]{stepArchetypes} with \code{numRep} elements.
}
\references{
Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.

Eugster, M. J., and Leisch, F., (2009). From Spider-Man to Hero - Archetypal Analysis in R, \emph{Journal of Statistical Software} \bold{30}, 1--23, \doi{10.18637/jss.v030.i08}.

Zehner, G. F., Meindl, R. S., and Hudson, J. A., (1993). A multivariate anthropometric method for crew station design: abridged. Tech. rep. Ohio: Human Engineering Division, Armstrong Laboratory, Wright-Patterson Air Force Base.
}
\note{
We would like to note that, some time after publishing the paper Epifanio et al. (2013), we found out that the \link[archetypes]{stepArchetypes} function standardizes the data by default (even when the data are already standardized) and this option is not always desired. In order to avoid this way of proceeding, we have created the \code{\link{stepArchetypesRawData}} function, which is used within \code{\link{archetypesBoundary}} instead of using \link[archetypes]{stepArchetypes}. Therefore, the results provided by \code{\link{archetypesBoundary}} allows us to reproduce the results of Epifanio et al. (2013) but they are now slightly different.
}
\author{
Irene Epifanio and Guillermo Vinue
}
\seealso{
\link[archetypes]{archetypes}, \link[archetypes]{stepArchetypes}, \code{\link{stepArchetypesRawData}}, \code{\link{USAFSurvey}}, \code{\link{nearestToArchetypes}}, \code{\link{preprocessing}}
}
\examples{
#The following R code allows us to reproduce the results of the paper Epifanio et al. (2013).
#As a toy example, only the first 25 individuals are used.
#First,the USAF 1967 database is read and preprocessed (Zehner et al. (1993)).
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_inch <- USAFSurvey[1:25, variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_inch, TRUE, 0.95, TRUE)

#Procedure and results shown in section 2.2.2 and section 3.1:
#For reproducing results, seed for randomness:
#suppressWarnings(RNGversion("3.5.0"))
#set.seed(2010)
res <- archetypesBoundary(USAFSurvey_preproc$data, 15, FALSE, 3)
#To understand the warning messages, see the vignette of the
#archetypes package.

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
barplot(a3,USAFSurvey_preproc$data, percentiles = TRUE, which = "beside") 
#Figure 2 (f):
barplot(a7,USAFSurvey_preproc$data, percentiles = TRUE, which = "beside")

#Results shown in section 3.3 related with PCA.
pznueva <- prcomp(USAFSurvey_preproc$data, scale = TRUE, retx = TRUE) 
#Table 3:
summary(pznueva)
pznueva
#PCA scores for 3 archetypes:
p3 <- predict(pznueva,archetypes::parameters(a3)) 
#PCA scores for 7 archetypes:
p7 <- predict(pznueva,archetypes::parameters(a7))
#Representing the scores:
#Figure 4 (a):
xyplotPCArchetypes(p3[,1:2], pznueva$x[,1:2], data.col = gray(0.7), atypes.col = 1, 
                   atypes.pch = 15)
#Figure 4 (b):
xyplotPCArchetypes(p7[,1:2], pznueva$x[,1:2], data.col = gray(0.7), atypes.col = 1, 
                   atypes.pch = 15)

#Percentiles for 7 archetypes (table 5):
Fn <- ecdf(USAFSurvey_preproc$data)
round(Fn(archetypes::parameters(a7)) * 100)

#Which are the nearest individuals to archetypes?:
#Example for three archetypes:
ras <- rbind(archetypes::parameters(a3),USAFSurvey_preproc$data)
dras <- dist(ras,method = "euclidean", diag = FALSE, upper = TRUE, p = 2)
mdras <- as.matrix(dras)
diag(mdras) = 1e+11
numArch <- 3
sapply(seq(length=numArch),nearestToArchetypes,numArch,mdras) 

#In addition, we can turn the standardized values to the original variables.
p <- archetypes::parameters(a7)
m <- sapply(USAFSurvey_inch,mean)
s <- sapply(USAFSurvey_inch,sd)
d <- p
for(i in 1 : 6){
 d[,i] = p[,i] * s[i] + m[i]
}
#Table 7:
t(d)
}
\keyword{array}