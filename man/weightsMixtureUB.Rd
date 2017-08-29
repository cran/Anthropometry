\name{weightsMixtureUB}
\alias{weightsMixtureUB}
\title{
Calculation of the weights for the OWA operators
}
\description{
This function calculates the weights of the OWA operators. They can be used to adjust the compromise between the style of garments and the general comfort sensation of wearers. This function is used both in \code{\link{trimowa}} and \code{\link{hipamAnthropom}}.
}
\usage{
weightsMixtureUB(orness,numVar)
}
\arguments{
\item{orness}{
Quantity to measure the degree to which the aggregation is like a min or max operation.
}
\item{numVar}{
Number of variables of the database.
}
}
\value{
Vector with the weights.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2014). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

Leon, T., Zuccarello, P., Ayala, G., de Ves, E., and Domingo, J., (2007), Applying logistic regression to relevance feedback in image retrieval systems, \emph{Pattern Recognition} \bold{40}, 2621--2632.
}
\author{
Guillermo Ayala
}
\seealso{
\code{\link{dbinom}}, \code{\link{getDistMatrix}}, \code{\link{trimowa}}, \code{\link{hipamAnthropom}}
}
\examples{
numVar <- dim(sampleSpanishSurvey)[2]
orness <- 0.7
w <- weightsMixtureUB(orness,numVar)
}

\keyword{math}
