\name{landmarks}
\alias{landmarks}
\docType{data}
\title{
Landmarks representing the woman's body
}
\description{
The body shape of the women who belong to \code{\link{dataDemo}} is represented by a set of anatomical correspondence points, called landmarks. 

This database collects the set of landmarks of each woman. 

The landmarks considered were placed in three different ways:
\itemize{
\item Automatic landmarks: automatically calculated with scanner program algorithms, based on geometrical features of the body.
 
\item Manual landmarks: points which are not reflected on the external body geometry; they were located through palpation by expert personnel and identified by a physical marker.
 
\item Digital landmarks: detected on the computer screen in the 3D scanned image. They are not robust on the automatic calculation but are easy to detect on the screen.  
} 
}
\usage{landmarks}
\format{A data frame with 600 observations and 198 variables (66 landmarks times 3 dimensions).}
\source{Anthropometric survey of the Spanish female population.}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design. Accepted for publication in Advances in Data Analysis and Classification.

Alemany, S., Gonzalez, J. C., Nacher, B., Soriano, C., Arnaiz, C., and Heras, H., (2010). Anthropometric survey of the Spanish female population aimed at the apparel industry. \emph{Proceedings of the 2010 Intl. Conference on 3D Body scanning Technologies}, 307--315.
}
\keyword{datasets}
