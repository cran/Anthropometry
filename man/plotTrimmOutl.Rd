\name{plotTrimmOutl}
\alias{plotTrimmOutl}
\title{
Trimmed or outlier observations representation
}
\description{
This function represents the scatter plots of bust circumference against other selected variable (chest,hip,neck to ground or waist) jointly with the trimmed individuals discarded in each bust class provided by \code{\link{trimowa}} or with the outlier individuals provided by \code{\link{hipamAnthropom}}.
}
\usage{
plotTrimmOutl(x,rows,nsizes,bustVariable,variable,color,xlim,ylim,title)
}
\arguments{
\item{x}{
Data frame. It should contain the chest, neck to ground, waist, hip and bust measurements of the individuals. In order to be able to represent them, the name of the columns of the database must be 'chest', 'necktoground', 'waist', 'hip' and 'bust' respectively, see \code{\link{dataDemo}}. Each row corresponds to an observation, and each column corresponds to a variable. All variables are numeric.
}
\item{rows}{
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
\item{color}{
A specification for the trimmed or outlier women color in each bust class.
}
\item{xlim}{
Axis lenght of the x axis according to the range of the bust variable.
} 
\item{ylim}{
Axis lenght of the y axis according to the range of the selected variable among chest, hip, neck to ground and waist.
}
\item{title}{
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
\code{\link{dataDemo}}, \code{\link{hipamAnthropom}}, \code{\link{trimowa}}
}
\examples{
\dontrun{
#TRIMOWA ALGORITHM:
#Data loading:
dataDef <- dataDemo
num.variables <- dim(dataDef)[2]
bust <- dataDef$bust

orness <- 0.7
w <- WeightsMixtureUB(orness,num.variables)

bustCirc_4 <- seq(74,102,4)  ; bustCirc_6 <- seq(107,131,6)  ; bustCirc <- c(bustCirc_4,bustCirc_6) 
nsizes <- length(bustCirc)
K <- 3 ; alpha <- 0.01 ; niter <- 6 ; Ksteps <- 7

ahVect <- c(23, 28, 20, 25, 25)

res_trimowa <- list()
for (i in 1 : (nsizes-1)){ 
  data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]   
  res_trimowa[[i]] <- trimowa(data,w,K,alpha,niter,Ksteps,ahVect=ahVect)
}

trimmed <- list()
for (i in 1 : (nsizes-1)){ 
  trimmed[[i]] <- res_trimowa[[i]][[10]] #or res_trimowa[[i]]$trimm
}

bustVariable <- "bust"
xlim <- c(70,150)
color <- c("black","red","green","blue","cyan","brown","gray","deeppink3",
           "orange","springgreen4","khaki3","steelblue1")

variable <- "chest"
range(dataDef[,variable])
#[1] 76.7755 135.8580
ylim <- c(70,140)
title <- "Trimmed women \n bust vs chest"

plotTrimmOutl(dataDef,trimmed,nsizes,bustVariable,variable,color,xlim,ylim,title)

variable <- "hip"
range(dataDef[,variable])
#[1] 83.6 152.1
ylim <- c(80,160)
title <- "Trimmed women \n bust vs hip"

plotTrimmOutl(dataDef,trimmed,nsizes,bustVariable,variable,color,xlim,ylim,title)

variable <- "necktoground"
range(dataDef[,variable])
#[1] 117.6 154.9
ylim = c(110,160)
title <- "Trimmed women \n bust vs neck to ground"

plotTrimmOutl(dataDef,trimmed,nsizes,bustVariable,variable,color,xlim,ylim,title)

variable <- "waist"
range(dataDef[,variable])
#[1]  58.6 133.0
ylim <- c(50,140)
title <- "Trimmed women \n bust vs waist"

plotTrimmOutl(dataDef,trimmed,nsizes,bustVariable,variable,color,xlim,ylim,title)

#AN EXAMPLE FOR HIPAM ALGORITHM:
dataDef <- dataDemo
bust <- dataDef$bust

bustCirc_4 <- seq(74,102,4)  ; bustCirc_6 <- seq(107,131,6)  ; bustCirc <- c(bustCirc_4,bustCirc_6) 
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

bustVariable <- "bust"
xlim <- c(70,150)
color <- c("black","red","green","blue","cyan","brown","gray","deeppink3",
           "orange","springgreen4","khaki3","steelblue1")

variable <- "hip"
ylim <- c(80,160)
title <- "Medoids \n bust vs hip"

list_outl1_2 <- sapply(1:(nsizes-1),outlierHipam,hip)
title <- "Outlier women \n bust vs hip"
plotTrimmOutl(dataDef,list_outl1_2,nsizes,bustVariable,variable,color,xlim,ylim,title)
}
}
\keyword{dplot}