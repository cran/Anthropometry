\name{HartiganShapes}
\alias{HartiganShapes}
\title{
Hartigan-Wong k-means for 3D shapes
}
\description{
The basic foundation of k-means is that the sample mean is the value that minimizes the Euclidean distance from each point, to the centroid of the cluster to which it belongs. Two fundamental concepts of the statistical shape analysis are the Procrustes mean and the Procrustes distance. Therefore, by integrating the Procrustes mean and the Procrustes distance we can use k-means in the shape analysis context. 

The k-means method has been proposed by several scientists in different forms. In computer science and pattern recognition the k-means algorithm is often termed the Lloyd algorithm (see Lloyd (1982)). However, in many texts, the term k-means algorithm is used for certain similar sequential clustering algorithms. Hartigan and Wong (1979) use the term k-means for an algorithm that searches for the locally optimal k-partition by moving points from one cluster to another.

This function allows us to use the Hartigan-Wong version of k-means adapted to deal with 3D shapes. Note that in the generic name of the k-means algorithm, k refers to the number of clusters to search for. To be more specific in the R code, k is referred to as \code{numClust}, see next section \emph{arguments}.
}
\usage{
HartiganShapes(array3D,numClust,algSteps=10,niter=10,
               stopCr=0.0001,simul,initLl,initials,verbose)
}
\arguments{
\item{array3D}{
Array with the 3D landmarks of the sample objects. Each row corresponds to an observation, and each column corresponds to a dimension (x,y,z).
}
\item{numClust}{
Number of clusters.
}
\item{algSteps}{
Number of steps per initialization. Default value is 10.
}
\item{niter}{
Number of random initializations (iterations). Default value is 10.
}
\item{stopCr}{
Relative stopping criteria. Default value is 0.0001.
}
\item{simul}{
Logical value. If TRUE, this function is used for a simulation study.
}
\item{initLl}{
Logical value. If TRUE, see next argument \code{initials}. If FALSE, they are new random initial values.
}
\item{initials}{
If \code{initLl=TRUE}, they are the same random initial values used in each iteration of \code{\link{LloydShapes}}. If \code{initLl=FALSE} this argument must be passed simply as an empty vector.
}
\item{verbose}{
A logical specifying whether to provide descriptive output about the running process.
}
}
\details{
There have been several attempts to adapt the k-means algorithm in the context of the statistical shape analysis, each one adapting a different version of the k-means algorithm (Amaral et al. (2010), Georgescu (2009)). In Vinue, G. et al. (2014), it is demonstrated that the Lloyd k-means represents a noticeable reduction in the computation involved when the sample size increases, compared with the Hartigan-Wong k-means. We state that Hartigan-Wong should be used in the shape analysis context only for very small samples.
}
\value{
A list with the following elements:

\emph{ic1}: Optimal clustering.

\emph{copt}: Optimal centers.

\emph{vopt}: Optimal objective function.

If a simulation study is carried out, the following elements are returned:

\emph{ic1}: Optimal clustering.

\emph{copt}: Optimal centers.

\emph{vopt}: Optimal objective function.

\emph{compTime}: Computational time.

\emph{AllRate}: Allocation rate.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification}, 1--30.

Hartigan, J. A., and Wong, M. A., (1979). A K-Means Clustering Algorithm, \emph{Applied Statistics}, 100--108.

Lloyd, S. P., (1982). Least Squares Quantization in PCM, \emph{IEEE Transactions on Information Theory} \bold{28}, 129--137.

Amaral, G. J. A., Dore, L. H., Lessa, R. P., and Stosic, B., (2010). k-Means Algorithm in Statistical Shape Analysis, \emph{Communications in Statistics - Simulation and Computation} \bold{39(5)}, 1016--1026.

Georgescu, V., (2009). Clustering of Fuzzy Shapes by Integrating Procrustean Metrics and Full Mean Shape Estimation into K-Means Algorithm. \emph{In IFSA-EUSFLAT Conference.}

Dryden, I. L., and Mardia, K. V., (1998). \emph{Statistical Shape Analysis}, Wiley, Chichester.
}
\note{
This function is based on the kmns.m file available from \url{http://people.sc.fsu.edu/~jburkardt/m_src/asa136/asa136.html}
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{LloydShapes}}, \code{\link{trimmedLloydShapes}}, \code{\link{landmarksSampleSpaSurv}}, \code{\link{cube8landm}}, \code{\link{parallelep8landm}}, \code{\link{cube34landm}}, \code{\link{parallelep34landm}}, \code{\link{procGPA}}, \code{\link{optraShapes}}, \code{\link{qtranShapes}}
}
\examples{
\dontrun{
#CLUSTERING INDIVIDUALS ACCORDING TO THEIR SHAPE:
landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
dim(landmarksNoNa) 
#[1] 574 198 
numLandmarks <- (dim(landmarksNoNa)[2]) / 3
#[1] 66
#In the interests of simplicity of the computation involved:
landmarksNoNa_First50 <- landmarksNoNa[1 : 50, ] 
(numIndiv <- dim(landmarksNoNa_First50)[1])
#[1] 50         
    
array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First50)
shapes::plotshapes(array3D[,,1]) 
calibrate::textxy(array3D[,1,1],array3D[,2,1],labs=1:numLandmarks,cex=0.7) 
 
numClust <- 3 ; algSteps <- 5 ; niter <- 3 ; stopCr <- 0.0001
set.seed(2013)
resHA <- HartiganShapes(array3D,numClust,algSteps,niter,stopCr,FALSE,FALSE,c(),TRUE)

#Numerical and graphical results:
asig <- resHA$ic1  #table(asig) shows the clustering results.
prototypes <- anthrCases("anthropometry", "kmeansProcrustes", resHA)

data_First50 <- sampleSpanishSurvey[1 : 50, ]
boxplot(data_First50$necktoground ~ as.factor(asig), main = "Neck to ground")

projShapes(1, array3D, asig, prototypes)
legend("topleft", c("Registrated data", "Mean shape"),
       pch = 1, col = 1:2, text.col = 1:2)
title("Procrustes registrated data for cluster 1 \n
       with its mean shape superimposed", sub = "Plane xy")


#SIMULATION STUDY:
#Definition of the cluster of cubes:
Ms_cube <- cube8landm
#Ms_cube <- cube34landm #for the case of 34 landmarks.
colMeans(Ms_cube)
dim(Ms_cube) 
shapes::plotshapes(Ms_cube[,,1])

#Number of landmarks and variables:
k_cube <- dim(Ms_cube)[1]
vars_cube <- k_cube * dim(Ms_cube)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_cube <- 0.01
Sigma_cube <- diag(sigma_cube,vars_cube)

#Sample size of each cluster (25, 250, 450):
n_cube <- 25

#Cluster of cubes:
simu1_cube <- mvtnorm::rmvt(n_cube,Sigma_cube,df=99)[,c(1 : k_cube * dim(Ms_cube)[2] 
        - 2, 1 : k_cube * dim(Ms_cube)[2] - 1, 1 : k_cube * dim(Ms_cube)[2])]
Simu1_cube <- as.vector(Ms_cube) + t(simu1_cube) 
dim(Simu1_cube) 

#Labels vector to identify the elements in the cluster of cubes:
etiqs_cl1 <- paste("cube_", 1:n_cube, sep = "")

#First cluster:
cl1 <- array(Simu1_cube, dim = c(k_cube, dim(Ms_cube)[2], n_cube), 
             dimnames = list(NULL, NULL, etiqs_cl1))
colMeans(cl1) 
dim(cl1) 

#Definition of the cluster of parallelepipeds:
Ms_paral <- parallelep8landm
#Ms_paral <- parallelep34landm #for the case of 34 landmarks.
colMeans(Ms_paral)
dim(Ms_paral) 

#Number of landmarks and variables:
k_paral <- dim(Ms_paral)[1] 
vars_paral <- k_paral * dim(Ms_paral)[2] 

#Covariance matrix (0.01, 9, 36):
sigma_paral <- 0.01
Sigma_paral <- diag(sigma_paral,vars_paral)

#Sample size of each cluster (25, 250, 450):
n_paral <- 25

#Cluster of parallelepipeds:
simu1_paral <- mvtnorm::rmvt(n_paral, Sigma_paral, df = 99)[,c(1 : k_paral * 
                    dim(Ms_paral)[2] - 2, 1 : k_paral * dim(Ms_paral)[2] - 
                    1, 1 : k_paral * dim(Ms_paral)[2])]
Simu1_paral <- as.vector(Ms_paral) + t(simu1_paral) 
dim(Simu1_paral) 

#Labels vector to identify the elements in the cluster of parallelepipeds:
etiqs_cl2 <- paste("Parallelepiped_", 1:n_paral, sep = "")

#Second cluster:
cl2 <- array(Simu1_paral, dim = c(k_paral, dim(Ms_paral)[2], n_paral), 
             dimnames = list(NULL, NULL, etiqs_cl2))
colMeans(cl2) 
dim(cl2) 

#Combine both clusters: 
array3D <- abind::abind(cl1,cl2)
str(array3D)
shapes3dShapes(array3D[,,1], loop = 0, type = "p", color = 2, joinline = c(1:1), 
            axes3 = TRUE, rglopen = TRUE, main = "First figure")

#First, the Lloyd algorithm is executed and then the Hartigan algorithm with 
#the same initial values used by the Lloy algorithm is executed:
numClust <- 2 ; algSteps <- 3 ; niter <- 3 ; stopCr <- 0.0001
resLLSim <- LloydShapes(array3D,numClust,algSteps,niter,stopCr,TRUE,TRUE)
resHASim <- HartiganShapes(array3D,numClust,algSteps,niter,stopCr,TRUE,TRUE,resLLSim$initials,TRUE)
}
}
\keyword{array}
