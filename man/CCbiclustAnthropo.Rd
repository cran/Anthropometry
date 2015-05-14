\name{CCbiclustAnthropo}
\alias{CCbiclustAnthropo}
\title{
Cheng and Church biclustering algorithm applied to anthropometric data
}
\description{
This function is the implementation in R of the algorithm that uses the Cheng and Church biclustering method (from now on, CC) to find size groups (biclusters) and disaccommodated individuals.

Designing lower body garments depends not only on the waist circumference (the principal dimension in this case), but also on other secondary control dimensions (for upper body garments the bust circumference is usually required only). Biclustering identifies groups of observations with a similar pattern in a subset of attributes instead of in the whole of them. Therefore, it seems to be more interesting to use a biclustering algorithm with a set of lower body variables. 

In Vinue et al. (2014), the way of proceeding was as follows: first, all the body variables related to the lower body part included in the Spanish anthropometric survey were chosen (there were 36). Second, the data set was divided into twelve segments (classes) using waist circumference values according to the European standard. Part 3: Measurements and intervals. Finally, the CC algorithm was applied to each waist class.
}
\usage{
CCbiclustAnthropo(data,waistVariable,waistCirc,lowerVar,
                  nsizes,nBic,diffRanges,percDisac,dir)
}
\arguments{
\item{data}{
Data matrix. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{waistVariable}{
Vector containing the waist values of the individuals.
}
\item{waistCirc}{
\code{data} is segmented into twelve waist classes. This vector contains the waist values to define each one of the twelve classes.  
}
\item{lowerVar}{
Lower body dimensions.
}
\item{nsizes}{
Number of waist sizes.
}
\item{nBic}{
Maximum number of biclusters to be found in each waist size. 
}
\item{diffRanges}{
List with \code{nsizes} elements. Each element is a vector whose extremes indicate the acceptable boundaries for selecting variables with a similar scale. This is needed because CC may be very influenced in case of variables involved in the study are on very different scales. 
}  
\item{percDisac}{
Proportion of no accommodated sample. 
}
\item{dir}{
Working directory where to save the results.
}
}
\details{
Interesting results in terms of apparel design were found: an efficient partition into different biclusters was obtained. All individuals in the same bicluster can wear a garment designed for the particular body dimensions (waist and other variables) which were the most relevant for defining the group. Each group is represented by the median woman. Because the CC algorithm is nonexhaustive, i.e, some rows (and columns) do not belong to any bicluster, this property can be used to fix a proportion of no accommodated sample.

This approach was descriptive and exploratory. It is emphasized that this function cannot be used with \code{\link{sampleSpanishSurvey}}, because this data file does not contain variables related to the lower body part in addition to waist and hip. However, this function is included in the package in the hope that it could be helpful or useful for other researchers. 
}
\value{
 A list with the following elements:

\emph{res}: List with \code{nsizes} elements. Each element contains the biclustering results for each waist segment.

\emph{dims}: List with \code{nsizes} elements. Each element contains the number of variables with a similar scale in each waist segment.

\emph{delta}: List with \code{nsizes} elements. Each element contains the delta parameter of the CC algorithm for each waist segment. 

\emph{disac}: List with \code{nsizes} elements. Each element contains the number of women who not belong to any bicluster for each waist segment.

\emph{mat}: List with \code{nsizes} elements. Each element contains the matrix showing which rows belong to each bicluster for each waist segment. This matrix allow us to know whether there are rows that belong to more than one bicluster, that is to say, whether there are overlapping biclusters. This is very important in our application because each individual must be assigned to a single size. See the \emph{Note} section.

\emph{tab_acc}: List with \code{nsizes} elements. Each element is a list with four elements. The first component indicates how many individuals belong to a single bicluster and how many do not belong to any bicluster. The second component refers to the number of biclusters found in each segment. The third one indicates the number of women that belong to each waist segment. The fourth one coincides with the \emph{disac} element.

\emph{ColBics}: List with \code{nsizes} elements. Each element contains the variables that belong to each bicluster for each waist segment.
}
\references{
Vinue, G., and Ibanez, M. V., (2014), \emph{Data depth and Biclustering applied to anthropometric data. Exploring their utility in apparel design}. Technical report.

Cheng, Y., and Church, G., (2000). Biclustering of expression data. \emph{Proceedings of the Eighth International Conference on Intelligent Systems for Molecular Biology} \bold{8}, 93--103.

Kaiser, S., and Leisch, F., (2008). A Toolbox for Bicluster Analysis in R. Tech.rep., Department of Statistics (University of Munich).

Alemany, S., Gonzalez, J. C., Nacher, B., Soriano, C., Arnaiz, C., and Heras, H., (2010). Anthropometric survey of the Spanish female population aimed at the apparel industry. \emph{Proceedings of the 2010 Intl. Conference on 3D Body scanning Technologies}, 307--315.

European Committee for Standardization. Size designation of clothes. Part 2: Primary and secondary dimensions. (2002).

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\note{
In order to know whether a row belongs to more than one bicluster, we count the number of 0s in each row of the \emph{mat} matrix returned by this function (see the \emph{Value} section). 

In case of there are res@Number - 1 0s in each row of \emph{mat}, then each row belongs to only one bicluster. The \emph{mat} matrix indicates with an 1 the rows that make up of the bicluster 1, with a 2 those rows that make up of the bicluster 2 and so on. In addition, it indicates with a 0 the rows that do not belong to any bicluster. Therefore, in order to check overlapping, every row must have a number of 0s equal to the total number of biclusters minus one. This one will indicate that that row belongs to a single bicluster. Otherwise, every row must have a number of 0s equal to the total number of biclusters. In this case, that row does not belong to any bicluster.  

For instance, if we find two biclusters, there should be one or two 0s in each row in case of no overlapping.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{overlapBiclustersByRows}}
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
}
}
\keyword{manip}

