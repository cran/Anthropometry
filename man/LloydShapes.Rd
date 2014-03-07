\name{LloydShapes}
\alias{LloydShapes}
\title{
Lloyd k-means for 3D shapes
}
\description{
The basic foundation of k-means is that the sample mean is the value that minimizes the Euclidean distance from each point, to the centroid of the cluster to which it belongs. Two fundamental concepts of the statistical shape analysis are the Procrustes mean and the Procrustes distance. Therefore, by integrating the Procrustes mean and the Procrustes distance we can use k-means in the shape analysis context. 

The k-means method has been proposed by several scientists in different forms. In computer science and pattern recognition the k-means algorithm is often termed the Lloyd algorithm (see Lloyd (1982)). 

This function allows us to use the Lloyd version of k-means adapted to deal with 3D shapes. 
}
\usage{
LloydShapes(dg,K,Nsteps=10,niter=10,stopCr=0.0001,simul,print)
}
\arguments{
\item{dg}{
Array with the 3D landmarks of the sample objects. Each row corresponds to an observation, and each column corresponds to a dimension (x,y,z).
}
\item{K}{
Number of clusters.
}
\item{Nsteps}{
Number of steps per initialization. Default value is 10.
}
\item{niter}{
Number of random initializations. Default value is 10.
}
\item{stopCr}{
Relative stopping criteria. Default value is 0.0001.
}
\item{simul}{
A logical value. If TRUE, this function is used for a simulation study.
}
\item{print}{
Logical value. If TRUE, some messages associated with the running process are displayed.
}
}
\details{
There have been several attempts to adapt the k-means algorithm in the context of the statistical shape analysis, each one adapting a different version of the k-means algorithm (Amaral et al. (2010), Georgescu (2009)). In Vinue et al. (2014) (submitted), it is demonstrated that the Lloyd k-means represents a noticeable reduction in the computation involved when the sample size increases, compared with the Hartigan-Wong k-means. We state that Hartigan-Wong should be used in the shape analysis context only for very small samples.
}
\value{
A list with the following elements:

\emph{asig}: Optimal clustering.

\emph{copt}: Optimal centers.

\emph{vopt}: Optimal objective function.

\emph{initials}: Random initial values used in each iteration. These values are then used by \code{\link{HartiganShapes}}.

If a simulation study is carried out, the following elements are returned:

\emph{compTime}: Computational time.

\emph{AllRate}: Allocation rate.

\emph{initials}: Random initial values used in each iteration. These values are then used by \code{\link{HartiganShapes}}.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design. Submitted for publication.

Lloyd, S. P., (1982). Least Squares Quantization in PCM, \emph{IEEE Transactions on Information Theory} \bold{28}, 129--137.

Dryden, I. L., and Mardia, K. V., (1998). \emph{Statistical Shape Analysis}, Wiley, Chichester.
}
\author{
Amelia Simo
}
\seealso{
\code{\link{HartiganShapes}}, \code{\link{trimmedLloydShapes}}, \code{\link{landmarks}}, \code{\link{cube8}}, \code{\link{parallelepiped8}}, \code{\link{cube34}}, \code{\link{parallelepiped34}}, \code{\link{procGPA}}
}
\examples{
\dontrun{
#CLUSTERING INDIVIDUALS ACCORDING TO THEIR SHAPE:
landmarks1 <- na.exclude(landmarks)
dim(landmarks1) 
#[1] 574 198 
(num.points <- (dim(landmarks1)[2]) / 3) 
#[1] 66
landmarks2 <- landmarks1[1:50,] #In the interests of simplicity of the computation involved.
(n <- dim(landmarks2)[1]) 
#[1] 50         
    
dg <- array(0,dim = c(num.points,3,n))
for(k in 1:n){            
 for(l in 1:3){            
  dg[,l,k] <-  as.matrix(as.vector(landmarks2[k,][seq(l,dim(landmarks2)[2]+
                         (l-1),by=3)]),ncol=1,byrow=T)
 }
}
shapes::plotshapes(dg[,,1]) 
calibrate::textxy(dg[,1,1],dg[,2,1],labs=1:num.points,cex=0.7) 

K <- 3 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
resLL <- LloydShapes(dg,K,Nsteps,niter,stopCr,FALSE,TRUE)

#Numerical and graphical results:
asig <- resLL$asig #table(asig) shows the clustering results.
copt <- resLL$copt #optimal centers.

#Generalised Procrustes analysis into each cluster:
out_proc <- list()
for(h in 1 : K){
 out_proc[[h]] = shapes::procGPA(dg[, , asig == h], distances = T, pcaoutput = T)
}

data <- dataDemo[1:50,]
boxplot(data$necktoground ~ as.factor(asig), main = "Neck to ground")

shapes::plotshapes(out_proc[[1]]$rotated)
points(copt[,,1], col = 2)
legend("topleft", c("Rotated data", "Mean shape"), pch = 1, col = 1:2, text.col = 1:2)
title("Procrustes rotated data for cluster 1 \n with its mean shape superimposed", sub = "Plane xy")



#SIMULATION STUDY:
#Definition of the cluster of cubes:
Ms_cube <- cube8
#Ms_cube <- cube34 #for the case of 34 landmarks.
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
Ms_paral <- parallelepiped8
#Ms_paral <- parallelepiped34 #for the case of 34 landmarks.
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
           dim(Ms_paral)[2] - 2, 1 : k_paral * dim(Ms_paral)[2] - 1, 
           1 : k_paral * dim(Ms_paral)[2])]
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
dg <- abind::abind(cl1,cl2)
str(dg)
shapes3dMod(dg[,,1], loop=0, type="p", color=2, joinline=c(1:1),
            axes3=TRUE, rglopen=TRUE, main="First figure")

K <- 2 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
resLLSim <- LloydShapes(dg,K,Nsteps,niter,stopCr,TRUE,TRUE)
}
}
\keyword{array}
