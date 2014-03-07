
## ----paquete,eval=FALSE--------------------------------------------------
## library(Anthropometry)


## ----functiontrimowa,eval=FALSE,tidy=FALSE-------------------------------
## trimowa(x,w,K,alpha,niter,Ksteps,ahVect=c(23,28,20,25,25))


## ----functionTDDclust,eval=FALSE,tidy=FALSE------------------------------
## TDDclust(x,K,lambda,Th,A,T0,alpha,lplot,Trimm,data1)


## ----functionhipam,eval=FALSE,tidy=FALSE---------------------------------
## hipamAnthropom(x,asw.tol=0,maxsplit=5,local.const=NULL,orness=0.7,
##                type,ahVect=c(23,28,20,25,25),...)


## ----functionLloyd,eval=FALSE,tidy=FALSE---------------------------------
## LloydShapes(dg,K,Nsteps=10,niter=10,stopCr=0.0001,simul,print)


## ----functionHW,eval=FALSE,tidy=FALSE------------------------------------
## HartiganShapes(dg,K,Nsteps=10,niter=10,stopCr=0.0001,simul,initLl,
##                initials,print)


## ----functionkmeansSSA,eval=FALSE,tidy=FALSE-----------------------------
## trimmedLloydShapes(dg,n,alpha,K,Nsteps=10,niter=10,
##                    stopCr=0.0001,print)


## ----archetUSAF,eval=FALSE,tidy=FALSE------------------------------------
## archetypesBoundary(data,numArchet,verbose,nrep)


## ----Arch,eval=FALSE,tidy=FALSE------------------------------------------
## archetypoids(i,data,huge=200,step,init,ArchObj,nearest,sequ,aux)


## ----stepArch,eval=FALSE,tidy=FALSE--------------------------------------
## stepArchetypoids(i,nearest,data,ArchObj)


## ----stepArchMod,eval=FALSE,tidy=FALSE-----------------------------------
## stepArchetypesMod(data,k,nrep=3,verbose=TRUE)


## ----trimowa,eval=FALSE,tidy=FALSE---------------------------------------
## #Data loading:
## dataDef <- dataDemo
## num.variables <- dim(dataDef)[2]
## bust <- dataDef$bust
## 
## #Aggregation weights calculation:
## orness <- 0.7
## w <- WeightsMixtureUB(orness,num.variables)
## 
## #Bust classes definition according to the European standard:
## bustCirc_4 <- seq(74,102,4)
## bustCirc_6 <- seq(107,131,6)
## bustCirc <- c(bustCirc_4,bustCirc_6)
## 
## #Trimowa parameters:
## nsizes <- length(bustCirc)
## K <- 3 ; alpha <- 0.01 ; niter <- 10 ; Ksteps <- 7
## ahVect <- c(23,28,20,25,25)
## 
## #For reproducing results, seed for randomness:
## set.seed(2014)
## res_trimowa <- list()
## for (i in 1 : (nsizes-1)){
## data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]
## res_trimowa[[i]] <- trimowa(data,w,K,alpha,niter,Ksteps,ahVect=ahVect)
## }
## 
## #Medoids provided for each bust class:
## medoids <- list()
## for (i in 1 : (nsizes-1)){
## medoids[[i]] <- res_trimowa[[i]]$meds
## }
## 
## #Plotting arguments:
## bustVariable <- "bust"
## xlim <- c(70,150)
## color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
##            "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
## 
## variable <- "necktoground"
## ylim = c(110,160) #see range(dataDef[,variable])
## title <- "Medoids \n bust vs neck to ground"
## 
## #Figure 1 (left and right):
## plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,
##             xlim,ylim,title,FALSE)
## plotMedoids(dataDef,medoids,nsizes,bustVariable,variable,color,
##             xlim,ylim,title,TRUE)


## ----hipam,eval=FALSE,tidy=FALSE-----------------------------------------
## #Data loading:
## dataDef <- dataDemo
## bust <- dataDef$bust
## 
## #Bust classes definition according to the European standard:
## bustCirc_4 <- seq(74,102,4)
## bustCirc_6 <- seq(107,131,6)
## bustCirc <- c(bustCirc_4,bustCirc_6)
## 
## #HipamAnthropom parameters:
## nsizes <- length(bustCirc)
## maxsplit <- 5 ; orness <- 0.7 ; type <- "IMO"
## #type <- "MO" for HIPAM_{MO}
## ahVect <- c(23, 28, 20, 25, 25)
## 
## #For reproducing results, seed for randomness:
## set.seed(2013)
## hip <- list()
## for(i in 1 : (nsizes - 1)){
## data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]
## d <- as.matrix(data)
## hip[[i]] <- hipamAnthropom(d,maxsplit=maxsplit,orness=orness,
##                            type=type,ahVect=ahVect)
## }
## 
## #hipamBigGroups is a function of Anthropometry that returns the medoids
## #of the clusters with more than 2 elements:
## list.meds <- lapply(1:(nsizes - 1), FUN = hipamBigGroups, hip)
## #outlierHipam is a function of Anthropometry that returns the individuals
## #of the clusters with 1 or 2 elements (outliers):
## list_outl1_2 <- sapply(1 : (nsizes - 1), FUN = outlierHipam, hip)
## 
## #Plotting arguments:
## bustVariable <- "bust"
## xlim <- c(70,150)
## color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
##            "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
## 
## variable <- "hip"
## ylim <- c(80,160)
## title <- "Medoids HIPAM_IMO \n bust vs hip"
## title_outl <- "Outlier women HIPAM_IMO \n bust vs hip"
## 
## #Figure 2 (left and right):
## plotMedoids(dataDef,list.meds,nsizes,bustVariable,variable,color,
##             xlim,ylim,title,FALSE)
## plotTrimmOutl(dataDef,list_outl1_2,nsizes,bustVariable,variable,color,
##               xlim,ylim,title_outl)


## ----TDDclust,eval=FALSE,tidy=FALSE--------------------------------------
## #In the interests of simplicity of the computation involved
## #only a small sample (the first 50 individuals) is selected:
## dataDef <- dataDemo[1:50,c(2,3,5)] #Neck to ground, waist and bust variables.
## data1 <- dataDemo[1:50,c(2,3,5)]
## 
## #TDDclust parameters:
## K=3     ; lambda=0.5    ; Th=0
## A=5     ; T0=0          ; alpha=.9
## lplot=0 ; percTrimm=0.1
## 
## Dout <- TDDclust(x=dataDef,K=K,lambda=lambda,Th=Th,A=A,T0=T0,alpha=alpha,
##                  lplot=lplot,Trimm=percTrimm,data1=data1)
## 
## #Clustering results:
## table(Dout$NN[1,])
## #Final value of the optimal partition:
## Dout$Cost
## #Iteration in which the optimal partition was found:
## Dout$klBest
## #Trimmed observations:
## Dout$indivTrimmed


## ----ssa,eval=FALSE,tidy=FALSE-------------------------------------------
## landmarks1 <- na.exclude(landmarks)
## num.points <- (dim(landmarks1)[2]) / 3
## #In the interests of simplicity of the computation involved
## #only a small sample (the first 50 individuals) is selected:
## landmarks2 <- landmarks1[1:50,]
## n <- dim(landmarks2)[1]
## 
## #Array with the 3D landmarks of the sample objects:
## dg <- array(0,dim = c(num.points,3,n))
## for(k in 1:n){
##  for(l in 1:3){
##   dg[,l,k] <- as.matrix(as.vector(landmarks2[k,][seq(l,dim(landmarks2)[2]+
##                         (l-1),by=3)]),ncol=1,byrow=T)
##  }
## }
## 
## #kmeansProcrustes parameters:
## K <- 3 ; alpha <- 0.01 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
## #For reproducing results, seed for randomness:
## set.seed(2013)
## res <- trimmedLloydShapes(dg,n,alpha,K,Nsteps,niter,stopCr,TRUE)
## 
## #Numerical and graphical results:
## asig <- res$asig #table(asig) shows the clustering results.
## copt <- res$copt #optimal centers.
## 
## #To identify the trimmed individuals of the optimal iteration:
## iter_opt <- res$trimmsIter[length(res$trimmsIter)]
## trimm <- res$trimmWomen[[iter_opt]][[res$betterNstep]]
## 
## #Generalised Procrustes analysis into each cluster:
## out_proc <- list()
## for(h in 1 : K){
##  out_proc[[h]] = shapes::procGPA(dg[, , asig == h], distances = T,
##                                  pcaoutput = T)
## }
## 
## data <- dataDemo[1:50,]
## data <- data[-trimm,]
## #Figure 3 (left):
## boxplot(data$necktoground ~ as.factor(asig), main = "Neck to ground")
## #Figure 3 (right):
## plotshapes(out_proc[[1]]$rotated)
## points(copt[,,1], col = 2)
## legend("topleft", c("Registrated data", "Mean shape"), pch = 1,
##        col = 1:2, text.col = 1:2)
## title("Procrustes registrated data for cluster 1 \n
##       with its mean shape superimposed", sub = "Plane xy")


## ----AA,eval=FALSE,tidy=FALSE--------------------------------------------
## #Cockpit design problem:
## #In the interests of simplicity of the computation involved
## #only a small sample (the first 50 individuals) is selected:
## m <- dataUSAF[1:50,]
## #Variable selection (cockpit dimensions):
## sel <- c(48,40,39,33,34,36)
## #Changing to inches:
## mpulg <- m[,sel] / (10 * 2.54)
## 
## #Data preprocessing:
## preproc <- accommodation(mpulg,TRUE,0.95,TRUE)
## 
## #For reproducing results, seed for randomness:
## set.seed(2010)
## #Run archetype algorithm repeatedly from 1 to numArch archetypes:
## numArch <- 10 ; nrep <- 20
## lass <- stepArchetypesMod(data=preproc$data,k=1:numArch,verbose=FALSE,
##                           nrep=nrep)
## screeplot(lass)
## 
## i <- 3 #number of archetypoids to compute.
## res <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,
##                     nearest=TRUE,sequ=TRUE)
## res_which <- archetypoids(i,preproc$data,huge=200,step=FALSE,ArchObj=lass,
##                           nearest=FALSE,sequ=TRUE)
## 
## aux <- res$archet
## aux_wh <- res_which$archet
## #In this case, the nearest and which archetypoids match
## #(although the nearest and which archetypes do not),
## #so it's enought to represent a single percentiles plot:
## 
## percs <- list()
## #In case the nearest and which archetypoids don't match:
## #percs_wh <- list()
## for(j in 1:length(aux)){
##  percs[[j]] <- sapply(1:dim(preproc$data)[2],compPerc,aux[j],preproc$data,0)
##  #percs_wh[[j]] <- sapply(1:dim(preproc$data)[2],compPerc,aux_wh[j],
##                         #preproc$data,0)
## }
## m <- matrix(unlist(percs),nrow=6,ncol=length(percs),byrow=F)
## #m1 <- matrix(unlist(percs_wh),nrow=6,ncol=length(percs_wh),byrow=F)
## 
## #Figure 4:
## barplot(m,beside=TRUE,
##         main = paste(i, " archetypoids", sep = ""),
##         ylim=c(0,100), ylab="Percentile")


