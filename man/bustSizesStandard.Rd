\name{bustSizesStandard}
\alias{bustSizesStandard}
\title{
Helper function for defining the bust sizes 
}
\description{
This is a helper function for defining the twelve bust sizes (from 74 cm to 131 cm) according to the sizes proposed in the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals.
}
\usage{
bustSizesStandard(bustCirc_4, bustCirc_6)
}
\arguments{
\item{bustCirc_4}{
Sequence of measurements from 74 to 102 in groups of four.
}
\item{bustCirc_6}{
Sequence of measurements from 107 to 131 in groups of six.
}
}
\value{
A list with the following elements:

\emph{bustCirc}: Vector of the twelve bust sizes.

\emph{nsizes}: Number of bust sizes (twelve).
}
\references{
European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).

Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{trimowa}}, \code{\link{hipamAnthropom}}
}
\examples{
\dontrun{
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))
}
}
\keyword{math}
