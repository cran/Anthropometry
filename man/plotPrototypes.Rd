\name{plotPrototypes}
\alias{plotPrototypes}
\title{
Prototypes representation
}
\description{
This function represents the scatter plots of bust circumference against other selected variable (chest, hip, neck to ground or waist) jointly with the prototypes obtained for each bust class provided by either \code{\link{trimowa}} or \code{\link{hipamAnthropom}}. In addition, the prototypes defined by the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals can be also displayed.
}
\usage{
plotPrototypes(data,prototypes,nsizes,bustVariable,variable,col,xlim,ylim,main,EN)
}
\arguments{
\item{data}{
Data frame. It should contain the chest, neck to ground, waist, hip and bust measurements of the individuals. In order to be able to represent them, the name of the columns of the database must be 'chest', 'necktoground', 'waist', 'hip' and 'bust' respectively, see \code{\link{sampleSpanishSurvey}}. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{prototypes}{
Prototypes (medoids) i.e., typical persons within the sample, obtained with \code{\link{trimowa}} or \code{\link{hipamAnthropom}}.
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
A specification for the medoids color in each bust class.
}
\item{xlim}{
Axis lenght of the x axis according to the range of the bust variable.
} 
\item{ylim}{
Axis lenght of the y axis according to the range of the selected variable among chest, hip, neck to ground and waist.
}
\item{main}{
Main title of the plot.
}
\item{EN}{
A logical value. If TRUE, the prototypes defined by the European standard for each variable are represented. See section \emph{Details} for more details.
}
}
\details{
Ir order to check the goodness of \code{\link{trimowa}}, the sizes defined by the prototypes can be compared with those defined by the European standard to sizing system. This standard establishes 12 sizes according to the combinations of the bust, waist and hip measurements and does not fix neither chest nor height standard measurements. We can approximate the chest measurements through a linear regression analysis, taking the bust measurements detailed in the standard as independent variable. Besides, we take as neck to ground measurements for the standard sizing system, the values 132, 136 and 140 cm because those are the most repeated values and they are those which best cover our data set. See Ibanez et al. (2012) for a complete explanation.
}
\value{
A device with the desired plot.
}
\references{
Ibanez, M. V., Vinue, G., Alemany, S., Simo, A., Epifanio, I., Domingo, J., and Ayala, G., (2012). Apparel sizing using trimmed PAM and OWA operators, \emph{Expert Systems with Applications} \bold{39}, 10512--10520.

Vinue, G., Leon, T., Alemany, S., and Ayala, G., (2013). Looking for representative fit models for apparel sizing, \emph{Decision Support Systems} \bold{57}, 22--33.

European Committee for Standardization. Size designation of clothes. Part 3: Measurements and intervals. (2005).
}
\note{
As mentioned, this function is especially defined for the sizes established by the European standard on sizing systems. Part 3: Measurements and intervals. In order to use this function with other standard, this function must be adapted. 
}
\author{
Guillermo Vinue
}
\seealso{
\code{\link{sampleSpanishSurvey}}, \code{\link{weightsMixtureUB}}, \code{\link{trimowa}}, \code{\link{getDistMatrix}}, \code{\link{trimmedoid}}, \code{\link{hipamAnthropom}}
}
\examples{
\dontrun{
#TRIMOWA ALGORITHM:
dataTrimowa <- sampleSpanishSurvey
numVar <- dim(dataTrimowa)[2]
bust <- dataTrimowa$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

orness <- 0.7
weightsTrimowa <- weightsMixtureUB(orness,numVar)

numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
ah <- c(23, 28, 20, 25, 25)

set.seed(2014)
res_trimowa <- list()
for (i in 1 : (bustSizes$nsizes - 1)){ 
 data = dataTrimowa[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]   
 res_trimowa[[i]] <- trimowa(data, weightsTrimowa, numClust, alpha, niter, 
                             algSteps,ah,verbose=FALSE)
}

prototypes <- anthrCases("anthropometry", "trimowa", res_trimowa, oneSize = FALSE, bustSizes$nsizes)

bustVariable <- "bust"
xlim <- c(70, 150)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", 
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "chest"
range(dataTrimowa[,variable])
#[1] 76.7755 135.8580
ylim <- c(70,140)
title <- "Prototypes \n bust vs chest"

plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)

variable <- "hip"
range(dataTrimowa[,variable])
#[1] 83.6 152.1
ylim <- c(80,160)
title <- "Prototypes \n bust vs hip"

plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)

bustVariable <- "bust"
xlim <- c(70, 150)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", 
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "necktoground"
range(dataTrimowa[,variable])
#[1] 117.6 154.9
ylim <- c(110, 160)
title <- "Prototypes \n bust vs neck to ground"

plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)

variable <- "waist"
range(dataTrimowa[,variable])
#[1]  58.6 133.0
ylim <- c(50,140)
title <- "Prototypes \n bust vs waist"

plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, TRUE)


#AN EXAMPLE FOR HIPAM ALGORITHM:
dataHipam <- sampleSpanishSurvey
bust <- dataHipam$bust
bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

type <- "IMO" #type <- "MO" for $HIPAM_{MO}$
maxsplit <- 5 ; orness <- 0.7 
ah <- c(23, 28, 20, 25, 25)

set.seed(2013)
res_hipam <- list()
for(i in 1 : (bustSizes$nsizes - 1)){
 data =  dataHipam[(bust >= bustSizes$bustCirc[i]) & (bust < bustSizes$bustCirc[i + 1]), ]   
 dataMat <- as.matrix(data)
 res_hipam[[i]] <- hipamAnthropom(dataMat, maxsplit = maxsplit, orness = orness, type = type,
                                   ah = ah, verbose = FALSE) 
}  

str(res_hipam)

fitmodels <- anthrCases("anthropometry", "HipamAnthropom", res_hipam, 
                         oneSize = FALSE, bustSizes$nsizes)

bustVariable <- "bust"
xlim <- c(70, 150)
color <- c("black", "red", "green", "blue", "cyan", "brown", "gray", 
           "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")

variable <- "hip"
ylim <- c(80, 160)
title <- "Fit models HIPAM_IMO \n bust vs hip"

plotPrototypes(dataHipam, fitmodels, bustSizes$nsizes, bustVariable, 
               variable, color, xlim, ylim, title, FALSE)
}
}
\keyword{dplot}
