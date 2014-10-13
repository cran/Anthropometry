
## ----functiontrimowa,eval=FALSE,tidy=FALSE-------------------------------
## trimowa(x, w, K, alpha, niter, Ksteps, ahVect = c(23, 28, 20, 25, 25))


## ----functionTDDclust,eval=FALSE,tidy=FALSE------------------------------
## TDDclust(x, K, lambda, Th, A, T0, alpha, Trimm, data1)


## ----functionhipam,eval=FALSE,tidy=FALSE---------------------------------
## hipamAnthropom(x, asw.tol = 0, maxsplit = 5, local.const = NULL,
##                orness = 0.7, type, ahVect = c(23, 28, 20, 25, 25), ...)


## ----functionLloyd,eval=FALSE,tidy=FALSE---------------------------------
## LloydShapes(dg, K, Nsteps = 10, niter = 10, stopCr = 0.0001, simul, print)


## ----functionHW,eval=FALSE,tidy=FALSE------------------------------------
## HartiganShapes(dg, K, Nsteps = 10, niter = 10, stopCr = 0.0001, simul,
##                initLl, initials, print)


## ----functionkmeansSSA,eval=FALSE,tidy=FALSE-----------------------------
## trimmedLloydShapes(dg, n, alpha, K, Nsteps = 10, niter = 10,
##                    stopCr = 0.0001, print)


## ----archetUSAF,eval=FALSE,tidy=FALSE------------------------------------
## archetypesBoundary(data, numArchet, verbose, nrep)


## ----Arch,eval=FALSE,tidy=FALSE------------------------------------------
## archetypoids(i, data, huge = 200, step, init, ArchObj, nearest, sequ, aux)


## ----stepArch,eval=FALSE,tidy=FALSE--------------------------------------
## stepArchetypoids(i, nearest, data, ArchObj)


## ----stepArchMod,eval=FALSE,tidy=FALSE-----------------------------------
## stepArchetypesMod(data, k, nrep = 3, verbose = TRUE)


## ----paquete,eval=FALSE--------------------------------------------------
## library("Anthropometry")


## ----trimowa1,eval=FALSE,tidy=FALSE--------------------------------------
## dataDef <- dataDemo
## num.variables <- dim(dataDef)[2]
## bust <- dataDef$bust
## bustCirc_4 <- seq(74, 102, 4)
## bustCirc_6 <- seq(107, 131, 6)
## bustCirc <- c(bustCirc_4, bustCirc_6)
## nsizes <- length(bustCirc)


## ----trimowa2,eval=FALSE,tidy=FALSE--------------------------------------
## orness <- 0.7
## w <- WeightsMixtureUB(orness, num.variables)


## ----trimowa3,eval=FALSE,tidy=FALSE--------------------------------------
## K <- 3 ; alpha <- 0.01 ; niter <- 10 ; Ksteps <- 7
## ahVect <- c(23, 28, 20, 25, 25)
## 
## set.seed(2014)
## res_trimowa <- list()
## for (i in 1 : (nsizes - 1)){
##  data = dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]
##  res_trimowa[[i]] <- trimowa(data, w, K, alpha, niter,
##                              Ksteps, ahVect = ahVect)
## }


## ----trimowa4,eval=FALSE,tidy=FALSE--------------------------------------
## medoids <- list()
## for (i in 1 : (nsizes - 1)){
## medoids[[i]] <- res_trimowa[[i]]$meds
## }


## ----trimowa5,eval=FALSE,tidy=FALSE--------------------------------------
## bustVariable <- "bust"
## xlim <- c(70, 150)
## color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
##            "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
## 
## variable <- "necktoground"
## ylim <- c(110, 160)
## title <- "Medoids \n bust vs neck to ground"
## 
## plotMedoids(dataDef, medoids, nsizes, bustVariable, variable, color,
##             xlim, ylim, title, FALSE)
## plotMedoids(dataDef, medoids, nsizes, bustVariable, variable, color,
##             xlim, ylim, title, TRUE)


## ----hipam,eval=FALSE,tidy=FALSE-----------------------------------------
## dataDef <- dataDemo
## bust <- dataDef$bust
## bustCirc_4 <- seq(74, 102, 4)
## bustCirc_6 <- seq(107, 131, 6)
## bustCirc <- c(bustCirc_4, bustCirc_6)
## nsizes <- length(bustCirc)


## ----hipam2,eval=FALSE,tidy=FALSE----------------------------------------
## type <- "IMO"
## maxsplit <- 5 ; orness <- 0.7
## ahVect <- c(23, 28, 20, 25, 25)
## 
## set.seed(2013)
## hip <- list()
## for(i in 1 : (nsizes - 1)){
## data =  dataDef[(bust >= bustCirc[i]) & (bust < bustCirc[i + 1]), ]
## d <- as.matrix(data)
## hip[[i]] <- hipamAnthropom(d, maxsplit = maxsplit, orness = orness,
##                            type = type, ahVect = ahVect)
## }


## ----hipam3,eval=FALSE,tidy=FALSE----------------------------------------
## list.meds <- lapply(1:(nsizes - 1), FUN = hipamBigGroups, hip)
## list_outl1_2 <- sapply(1 : (nsizes - 1), FUN = outlierHipam, hip)


## ----hipam4,eval=FALSE,tidy=FALSE----------------------------------------
## bustVariable <- "bust"
## xlim <- c(70, 150)
## color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
##            "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
## 
## variable <- "hip"
## ylim <- c(80, 160)
## title <- "Medoids HIPAM_IMO \n bust vs hip"
## title_outl <- "Outlier women HIPAM_IMO \n bust vs hip"
## 
## plotMedoids(dataDef, list.meds, nsizes, bustVariable, variable, color,
##             xlim, ylim, title, FALSE)
## plotTrimmOutl(dataDef, list_outl1_2, nsizes, bustVariable, variable, color,
##               xlim, ylim, title_outl)


## ----TDDclust,eval=FALSE,tidy=FALSE--------------------------------------
## dataDef <- dataDemo[1 : 25, c(2, 3, 5)]
## data1 <- dataDemo[1 : 25, c(2, 3, 5)]


## ----TDDclust2,eval=FALSE,tidy=FALSE-------------------------------------
## K <- 3 ; percTrimm <- 0.01 ; lambda <- 0.5 ; A <- 5
## Th <- 0 ; T0 <- 0 ; alpha <- 0.9
## 
## set.seed(2014)
## Dout <- TDDclust(x = dataDef, K = K, lambda = lambda, Th = Th, A = A,
##                  T0 = T0, alpha = alpha, Trimm = percTrimm, data1 = data1)


## ----TDDclust3,eval=FALSE,tidy=FALSE-------------------------------------
## table(Dout$NN[1,])
## Dout$Cost
## Dout$klBest
## Dout$indivTrimmed


## ----ssa,eval=FALSE,tidy=FALSE-------------------------------------------
## landmarks1 <- na.exclude(landmarks)
## num.points <- (dim(landmarks1)[2]) / 3
## landmarks2 <- landmarks1[1 : 50, ]
## n <- dim(landmarks2)[1]


## ----ssa1,eval=FALSE,tidy=FALSE------------------------------------------
## dg <- array(0, dim = c(num.points, 3, n))
## for(k in 1 : n){
##  for(l in 1 : 3){
##   dg[, l, k] <- as.matrix(as.vector(landmarks2[k, ][seq(l, dim(landmarks2)[2]
##                                                         + (l - 1), by = 3)]),
##                           ncol = 1, byrow = T)
##  }
## }


## ----ssa2,eval=FALSE,tidy=FALSE------------------------------------------
## K <- 3 ; alpha <- 0.01 ; Nsteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
## set.seed(2013)
## res <- trimmedLloydShapes(dg, n, alpha, K, Nsteps, niter, stopCr, TRUE)


## ----ssa3,eval=FALSE,tidy=FALSE------------------------------------------
## asig <- res$asig
## table(asig)
## copt <- res$copt


## ----ssa4,eval=FALSE,tidy=FALSE------------------------------------------
## iter_opt <- res$trimmsIter[length(res$trimmsIter)]
## trimm <- res$trimmWomen[[iter_opt]][[res$betterNstep]]


## ----ssa5,eval=FALSE,tidy=FALSE------------------------------------------
## data <- dataDemo[1 : 50, ]
## data <- data[-trimm, ]
## boxplot(data$necktoground ~ as.factor(asig), main = "Neck to ground")


## ----ssa6,eval=FALSE,tidy=FALSE------------------------------------------
## out_proc <- list()
## for(h in 1 : K){
##  out_proc[[h]] = shapes::procGPA(dg[, , asig == h], distances = T,
##                                  pcaoutput = T)
## }
## 
## shapes::plotshapes(out_proc[[1]]$rotated)
## points(copt[, , 1], col = 2)
## legend("topleft", c("Registrated data", "Mean shape"), pch = 1,
##        col = 1:2, text.col = 1:2)
## title("Procrustes registrated data for cluster 1 \n
##       with its mean shape superimposed", sub = "Plane xy")


## ----AA,eval=FALSE,tidy=FALSE--------------------------------------------
## m <- dataUSAF[1 : 50, ]
## sel <- c(48, 40, 39, 33, 34, 36)
## mpulg <- m[,sel] / (10 * 2.54)
## preproc <- accommodation(mpulg, TRUE, 0.95, TRUE)


## ----AA3,eval=FALSE,tidy=FALSE-------------------------------------------
## set.seed(2010)
## numArch <- 10 ; nrep <- 20
## lass <- stepArchetypesMod(data = preproc$data, k = 1 : numArch,
##                           verbose = FALSE, nrep = nrep)
## screeplot(lass)


## ----AA4,eval=FALSE,tidy=FALSE-------------------------------------------
## i <- 3
## res <- archetypoids(i, preproc$data, huge = 200, step = FALSE,
##                     ArchObj = lass, nearest = TRUE, sequ = TRUE)
## res_which <- archetypoids(i, preproc$data, huge = 200, step = FALSE,
##                           ArchObj = lass, nearest = FALSE, sequ = TRUE)
## 
## aux <- res$archet
## aux_wh <- res_which$archet


## ----AA5,eval=FALSE,tidy=FALSE-------------------------------------------
## percs <- list()
## for(j in 1 : length(aux)){
##  percs[[j]] <- sapply(1 : dim(preproc$data)[2], compPerc, aux[j],
##                       preproc$data, 0)
## }
## m <- matrix(unlist(percs), nrow = 6, ncol = length(percs), byrow = F)


## ----AA6,eval=FALSE,tidy=FALSE-------------------------------------------
## barplot(m, beside = TRUE, main = paste(i, " archetypoids", sep = ""),
##         ylim = c(0, 100), ylab = "Percentile")


## ----comp1,eval=FALSE,tidy=FALSE-----------------------------------------
## set.seed(1900)
## rand <- sample(1:600,20)
## 
## dataDef <- dataDemo[rand, c(2, 3, 5)]
## data1 <- dataDemo[rand, c(2, 3, 5)]
## 
## K <- 3 ; lambda <- 0.5 ; Th <- 0
## A <- 5 ; T0 <- 0 ; alpha <- 0.9
## percTrimm <- 0.01 ;ahVect <- c(28, 25, 25)
## orness <- 0.7 ; niter <- 10 ; Ksteps <- 7
## 
## #TDDclust:
## Dout <- TDDclust(x = dataDef, K = K, lambda = lambda, Th = Th, A = A,
##                  T0 = T0, alpha = alpha, Trimm = percTrimm,
##                  data1 = data1)
## Dout$Y
## 
## #Trimowa:
## num.variables <- dim(dataDef)[2]
## w <- WeightsMixtureUB(orness, num.variables)
## res_trimowa <- trimowa(dataDef, w, K, percTrimm, niter, Ksteps,
##                        ahVect = ahVect)
## dataDemo[res_trimowa$meds,]


