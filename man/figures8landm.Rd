\name{figures8landm}
\alias{figures8landm}
\title{
Figures with labelled landmarks
}
\description{
This function allows us to represent the two geometric figures (a cube and a parallelepiped) of 8 landmarks, with the landmark labels. Both appear in the submitted paper Vinue et al. (2013), referred below.
}
\usage{
figures8landm(figure,data)
}
\arguments{
\item{figure}{
A character, two values are admitted: if figure="cube", the cube is represented. If figure="paral", the parallelepiped is represented.
}
\item{data}{
The data with the landmarks of the corresponding figure.
}
}
\value{
A plot of the cube or the parallelepiped with the landmark labels.
}
\references{
Vinue, G., Simo, A., and Alemany, S., (2014). The k-means algorithm for 3D shapes with an application to apparel design. Submitted for publication.
}
\author{
Guillermo Vinue
}
\examples{
\dontrun{
figures8landm("cube",cube8)
figures8landm("paral",parallelepiped8)
}
}
\keyword{array}