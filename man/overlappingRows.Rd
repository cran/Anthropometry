\name{overlappingRows}
\alias{overlappingRows}
\title{
Overlapped biclusters by rows
}
\description{
This function allows us to check which rows belong to more than one bicluster. It is used within the \code{\link{CCbiclustAnthropo}} function.
}
\usage{
overlappingRows(i,resBicluster)
}
\arguments{
\item{i}{
Bicluster number.
}
\item{resBicluster}{
An object of class Biclust.
}
}
\value{
A matrix with as many rows as rows of the original matrix, and as many columns as obtained biclusters. 
}
\details{
In order to know how this function works, it is necessary to understand the following commands:

\emph{res.bicl@RowxNumber[,1]} indicates the rows that belong to the bicluster 1, by assigning a TRUE value to the position of those rows inside the original matrix. By using \emph{table(res.bicl@RowxNumber[,1])}, we obtain the number of rows belonging to bicluster 1.

\emph{1 * res.bicl@RowxNumber[,1]} makes TRUES into 1s.

\emph{i * res.bicl@RowxNumber[,i]} makes TRUES into the corresponding value of i.

In short, this function puts a 1 in those rows belonging to bicluster 1, a 2 in those ones of bicluster 2, and so on.

The fact that certain columns of the matrix returned by this function have a value different from 0 at the same row, will indicate that that row belong to both biclusters.
}
\references{
Vinue, G., and Ibanez, M. V., (2014), \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. In progress.

Kaiser, S., and Leisch, F., (2008). A Toolbox for Bicluster Analysis in R. Tech.rep., Department of Statistics (University of Munich).
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{CCbiclustAnthropo}}
}
\examples{
\dontrun{
#Note: package biclust needed.
#This is an example of using this function with a certain database 
#made up of body dimensions related to the lower body part.
data <- dataUser[(waist >= 58) & (waist < 115),] #dataUser is the user database.
rownames(data) <- 1:dim(data)[1]
  
waist <- data[,"WaistCircumference"] 
    
waist_4 <- seq(58, 86, 4) 
waist_6 <- seq(91, 115, 6) 
waistCirc <- c(waist_4,waist_6)
nsizes <- length(waistCirc) 

#Position of the body variables in the database:
lowerVars <- c(14, 17:25, 27, 28, 65:73, 75, 77:81, seq(100, 116, 2))

nBic <- c(2, 2, 4, rep(5, 7), 3, 3)  
diffRanges <- list(c(14,20), c(24,30), c(24,30), c(33,39), c(29,35), c(29,35), 
                   c(28,35), c(31,38), c(31,38), c(30,37), c(26,33), c(25,32))
percDisac <- 0.01 
dir <- "/home/guillermo/"
  
res_bicl_antropom <- CCbiclustAnthropo(data,waist,waistCirc,lowerVars,
                                       nsizes,nBic,diffRanges,percDisac,dir)

#For a single size:
size <- 5
res <- res_bicl_antropom[[1]][[size]]

sapply(1 : res@Number, overlappingRows, res)
}
}
\keyword{manip}

