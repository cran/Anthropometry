\name{plotMedoids}
\alias{plotMedoids}
\title{
Medoids representation
}
\description{
This function represents the scatter plots of bust circumference against other selected variable (chest, hip, neck to ground or waist) jointly with the medoids obtained for each bust class provided by either \code{\link{trimowa}} or \code{\link{hipamAnthropom}}. In addition, the prototypes defined by the European standard on sizing systems. Size designation of clothes. Part 3: Measurements and intervals can be also displayed.
}
\usage{
plotMedoids(x,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,EN)
}
\arguments{
\item{x}{
Data frame. It should contain the chest, neck to ground, waist, hip and bust measurements of the individuals. In order to be able to represent them, the name of the columns of the database must be 'chest', 'necktoground', 'waist', 'hip' and 'bust' respectively, see \code{\link{dataDemo}}. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{medoids}{
Medoids i.e., typical persons within the sample, obtained with \code{\link{trimowa}} or \code{\link{hipamAnthropom}}.
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
\item{color}{
A specification for the medoids color in each bust class.
}
\item{xlim}{
Axis lenght of the x axis according to the range of the bust variable.
} 
\item{ylim}{
Axis lenght of the y axis according to the range of the selected variable among chest, hip, neck to ground and waist.
}
\item{title}{
Main title of the plot.
}
\item{EN}{
A logical value. If TRUE, the prototypes defined by the European standard for each variable are represented. See section \emph{Details} for more details.
}
}
\details{
Ir order to check the goodness of \code{\link{trimowa}}, the sizes defined by the medoids can be compared with those defined by the European standard to sizing system. This standard establishes 12 sizes according to the combinations of the bust, waist and hip measurements and does not fix neither chest nor height standard measurements. We can approximate the chest measurements through a linear regression analysis, taking the bust measurements detailed in the standard as independent variable. Besides, we take as neck to ground measurements for the standard sizing system, the values 132, 136 and 140 cm because those are the most repeated values and they are those which best cover our data set. See Ibanez et al. (2012) for a complete explanation.
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
\code{\link{dataDemo}}, \code{\link{WeightsMixtureUB}}, \code{\link{trimowa}}, \code{\link{GetDistMatrix}}, \code{\link{trimmedoid}}, \code{\link{hipamAnthropom}}
}
\examples{
\dontrun{
#TRIMOWA ALGORITHM:
#Loading the data to apply the trimowa algorithm:
dataDef <- dataDemo
num.variables <- dim(dataDef)[2]
bust <- dataDef$bust

orness <- 0.7
w <- WeightsMixtureUB(orness,num.variables)

bustCirc_4 <- seq(74,102,4)  
bustCirc_6 <- seq(107,131,6)  
bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
K <- 3 ; alpha <- 0.01 ; niter <- 6 ; Ksteps <- 7

ahVect <- c(23,28,20,25,25)

res_trimowa <- list()
for (i in 1 : (nsizes-1)){ 
  data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  res_trimowa[[i]] <- trimowa(data,w,K,alpha,niter,Ksteps,ahVect=ahVect)
}

medoids <- list()
for (i in 1 : (nsizes-1)){ 
  medoids[[i]] <- res_trimowa[[i]]$meds #or res_trimowa[[i]][[1]]
}

bustVariable <- "bust"
xlim <- c(70,150)
color <- c("black","red","green", "blue","cyan","brown","gray","deeppink3",
           "orange","springgreen4","khaki3","steelblue1")

variable <- "chest"
range(dataDef[,variable])
#[1] 76.7755 135.8580
ylim <- c(70,140)
title <- "Medoids \n bust vs chest"

plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,FALSE)
plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,TRUE)

variable <- "hip"
range(dataDef[,variable])
#[1] 83.6 152.1
ylim <- c(80,160)
title <- "Medoids \n bust vs hip"

plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,FALSE)
plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,TRUE)

variable <- "necktoground"
range(dataDef[,variable])
#[1] 117.6 154.9
ylim = c(110,160)
title <- "Medoids \n bust vs neck to ground"

plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,FALSE)
plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,TRUE)

variable <- "waist"
range(dataDef[,variable])
#[1]  58.6 133.0
ylim <- c(50,140)
title <- "Medoids \n bust vs waist"

plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,FALSE)
plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,xlim,ylim,title,TRUE)

#AN EXAMPLE FOR HIPAM ALGORITHM:
dataDef <- dataDemo
bust <- dataDef$bust

bustCirc_4 <- seq(74,102,4)  
bustCirc_6 <- seq(107,131,6)  
bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
maxsplit <- 5 ; orness <- 0.7 ; alpha <- 0.01 ; type <- "MO" #type <- "IMO" for $HIPAM_{IMO}$

ahVect <- c(23, 28, 20, 25, 25)

hip <- list()
for(i in 1 : (nsizes - 1)){
  data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  d <- as.matrix(data)
  hip[[i]] <- hipamAnthropom(d,maxsplit=maxsplit,orness=orness,type=type,ahVect=ahVect) 
}   
str(hip)

ress <- list()
for(i in 1 : length(hip)){
  ress[[i]] <- table(hip[[i]]$clustering)
}
ress

list.meds <- lapply(1:(nsizes - 1), FUN = hipamBigGroups, hip)  

bustVariable <- "bust"
xlim <- c(70,150)
color <- c("black","red","green","blue","cyan","brown","gray","deeppink3",
           "orange","springgreen4","khaki3","steelblue1")

variable <- "hip"
ylim <- c(80,160)
title <- "Medoids \n bust vs hip"

plotMedoids(dataDef,list.meds,nsizes,bustVariable,variable,color,xlim,ylim,title,FALSE)
}
}
\keyword{dplot}
