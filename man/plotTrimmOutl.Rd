\name{plotTrimmOutl}
\alias{plotTrimmOutl}
\title{
Trimmed or outlier observations representation
}
\description{
This function represents the scatter plots of bust circumference against other selected variable (chest,hip,neck to ground or waist) jointly with the trimmed individuals discarded in each bust class provided by \code{\link{trimowa}} or with the outlier individuals provided by \code{\link{hipamAnthropom}}.
}
\usage{
plotTrimmOutl(data,trimmOutl,nsizes,bustVariable,variable,col,xlim,ylim,main)
}
\arguments{
\item{data}{
Data frame. It should contain the chest, neck to ground, waist, hip and bust measurements of the individuals. In order to be able to represent them, the name of the columns of the database must be 'chest', 'necktoground', 'waist', 'hip' and 'bust' respectively, see \code{\link{sampleSpanishSurvey}}. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{trimmOutl}{
Trimmed women (if \code{\link{trimowa}}) or outlier women (if \code{\link{hipamAnthropom}}).
}
\item{nsizes}{
Number of subsets (classes), into the database is segmented. In our approach, the whole anthropometric Spanish survey is segmented into twelve bust segments, according to the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals.
}
\item{bustVariable}{
Bust variable.
}
\item{variable}{
Anthropometric variable to be plotted. It can be 'chest', 'necktoground', 'waist' and 'hip'.
}
\item{col}{
A specification for the trimmed or outlier women color in each bust class.
}
\item{xlim}{
Axis lenght of the x axis according to the range of the bust variable.
} 
\item{ylim}{
Axis lenght of the y axis according to the range of the selected variable among chest, hip, neck to ground and waist.
}
\item{main}{
Title of the plot.
}
}
\value{
A device with the desired plot.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{sampleSpanishSurvey}}, \code{\link{hipamAnthropom}}, \code{\link{trimowa}}
}
\examples{
#TRIMOWA ALGORITHM:
dataTrimowa <- sampleSpanishSurvey
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness, numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

set.seed(2014)
numSizes <- 1
res_trimowa <- computSizesTrimowa(dataTrimowa, bust, bustSizes$bustCirc, numSizes,
                                  weightsTrimowa, numClust, alpha, niter, algSteps, 
                                  ah, FALSE)

prototypes <- anthrCases(res_trimowa, numSizes)
trimmed <- trimmOutl(res_trimowa, numSizes)

bustVariable <- "bust"
xlim <- c(72, 132)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "chest"
range(dataTrimowa[,variable])
#[1] 76.7755 135.8580
ylim <- c(70,140)
main <- "Trimmed women \n bust vs chest"

plotTrimmOutl(dataTrimowa, trimmed, numSizes, bustVariable, variable, color, 
              xlim, ylim, main)

#For other plots and an example for the hipam algorithm, 
#see www.uv.es/vivigui/softw/more_examples.R 
}
\keyword{dplot}