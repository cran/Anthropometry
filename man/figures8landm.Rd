\name{figures8landm}
\alias{figures8landm}
\title{
Figures of 8 landmarks with labelled landmarks
}
\description{
This function allows us to represent the two geometric figures (a cube and a parallelepiped) of 8 landmarks, with the landmark labels. Both appear in the paper Vinue et al. (2016), referred below.
}
\usage{
figures8landm(figure,data)
}
\arguments{
\item{figure}{
A character vector, two values are admitted: if figure="cube", the cube is represented. If figure="paral", the parallelepiped is represented.
}
\item{data}{
The data with the landmarks of the corresponding figure.
}
}
\value{
A plot of the cube or the parallelepiped with the landmark labels.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2016). The k-means algorithm for 3D shapes with an application to apparel design, \emph{Advances in Data Analysis and Classification} \bold{10(1)}, 103--132.
}
\author{
Guillermo Vinue
}
\examples{
figures8landm("cube", cube8landm)
figures8landm("paral", parallelep8landm)
}
\keyword{array}