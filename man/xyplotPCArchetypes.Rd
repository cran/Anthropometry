\name{xyplotPCArchetypes}
\alias{xyplotPCArchetypes}
\title{
PC scores for archetypes
}
\description{
This function is a small modification of the generic \code{xyplot} function  of the \pkg{archetypes} R package. It shows the scores for the principal components of all individuals jointly with the scores for the computed archetypes. This function is used to obtain the Figure 4 of the subsection 3.3 of Epifanio et al. (2013).
}
\value{
A device with the desired plot.
}
\note{
There are no usage and arguments sections in this help file because they are the same than those of the page 25 of the reference manual of \pkg{archetypes}.
}
\references{
Epifanio, I., Vinue, G., and Alemany, S., (2013). Archetypal analysis: contributions for estimating boundary cases in multivariate accommodation problem, \emph{Computers & Industrial Engineering} \bold{64}, 757--765.
}
\author{
Irene Epifanio
}
\seealso{
\code{\link{archetypesBoundary}}, \code{\link{USAFSurvey}}
}
\examples{
#First,the USAF 1967 database is read and preprocessed (Zehner et al. (1993)).
#Variable selection:
variabl_sel <- c(48, 40, 39, 33, 34, 36)
#Changing to inches: 
USAFSurvey_inch <- USAFSurvey[1:25, variabl_sel] / (10 * 2.54)

#Data preprocessing:
USAFSurvey_preproc <- preprocessing(USAFSurvey_inch, TRUE, 0.95, TRUE)

#Procedure and results shown in section 2.2.2 and section 3.1:
set.seed(2010)
res <- archetypesBoundary(USAFSurvey_preproc$data, 15, FALSE, 3)
#To understand the warning messages, see the vignette of the
#archetypes package.  

a3 <- archetypes::bestModel(res[[3]])
a7 <- archetypes::bestModel(res[[7]])

pznueva <- prcomp(USAFSurvey_preproc$data, scale = TRUE, retx = TRUE) 
#PCA scores for 3 archetypes:
p3 <- predict(pznueva,archetypes::parameters(a3)) 
#PCA scores for 7 archetypes:
p7 <- predict(pznueva,archetypes::parameters(a7))
#Representing the scores:
#Figure 4 (a):
xyplotPCArchetypes(p3[,1:2], pznueva$x[,1:2], data.col = gray(0.7), 
                   atypes.col = 1, atypes.pch = 15)
#Figure 4 (b):
xyplotPCArchetypes(p7[,1:2], pznueva$x[,1:2], data.col = gray(0.7), 
                   atypes.col = 1, atypes.pch = 15)
}
\keyword{array}
