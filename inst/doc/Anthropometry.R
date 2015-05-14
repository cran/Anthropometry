## ----comp1,eval=FALSE,tidy=FALSE-----------------------------------------
#  library("Anthropometry")
#  
#  set.seed(1900)
#  rand <- sample(1:600,20)
#  
#  dataComp <- sampleSpanishSurvey[rand, c(2, 3, 5)]
#  numVar <- dim(dataComp)[2]
#  dataComp_aux <- sampleSpanishSurvey[rand, c(2, 3, 5)]
#  
#  numClust <- 3 ; alpha <- 0.01 ; lambda <- 0.5 ; niter <- 5
#  Th <- 0 ; T0 <- 0 ; simAnn <- 0.9
#  ah <- c(28, 25, 25) ; orness <- 0.7 ; algSteps <- 7
#  
#  #TDDclust:
#  set.seed(1900)
#  TDDcl <- TDDclust(dataComp, numClust, lambda, Th, niter, T0,
#                   simAnn, alpha, dataComp_aux, verbose = FALSE)
#  prototypes_TDDcl <- anthrCases("anthropometry", "TDDclust", TDDcl)
#  
#  #Trimowa:
#  weightsTrimowa <- weightsMixtureUB(orness, numVar)
#  set.seed(1900)
#  Trimowa <- trimowa(dataComp, weightsTrimowa, numClust, alpha, niter,
#                     algSteps, ah, verbose = FALSE)
#  prototypes_Trimowa <- anthrCases("anthropometry", "trimowa",
#                                   Trimowa, oneSize = TRUE)
#  
#  #Table 1 is generated with:
#  #dataComp[which(rownames(dataComp) %in% prototypes_TDDcl),]
#  #dataComp[which(rownames(dataComp) %in% prototypes_Trimowa ),]

## ----paquete,eval=FALSE--------------------------------------------------
#  library("Anthropometry")

## ----trimowa1,eval=FALSE,tidy=FALSE--------------------------------------
#  dataTrimowa <- sampleSpanishSurvey
#  numVar <- dim(dataTrimowa)[2]
#  bust <- dataTrimowa$bust
#  bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

## ----trimowa2,eval=FALSE,tidy=FALSE--------------------------------------
#  orness <- 0.7
#  weightsTrimowa <- weightsMixtureUB(orness, numVar)

## ----trimowa3,eval=FALSE,tidy=FALSE--------------------------------------
#  numClust <- 3 ; alpha <- 0.01 ; niter <- 10 ; algSteps <- 7
#  ah <- c(23, 28, 20, 25, 25)
#  
#  set.seed(2014)
#  res_trimowa <- list()
#  for (i in 1 : (bustSizes$nsizes - 1)){
#   data = dataTrimowa[(bust >= bustSizes$bustCirc[i]) &
#                      (bust < bustSizes$bustCirc[i + 1]), ]
#   res_trimowa[[i]] <- trimowa(data, weightsTrimowa, numClust, alpha, niter,
#                               algSteps, ah, verbose = FALSE)
#  }

## ----trimowa4,eval=FALSE,tidy=FALSE--------------------------------------
#  prototypes <- anthrCases("anthropometry", "trimowa", res_trimowa,
#                           oneSize = FALSE, bustSizes$nsizes)

## ----trimowa5,eval=FALSE,tidy=FALSE--------------------------------------
#  bustVariable <- "bust"
#  xlim <- c(70, 150)
#  color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
#             "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
#  
#  variable <- "necktoground"
#  ylim <- c(110, 160)
#  title <- "Prototypes \n bust vs neck to ground"
#  
#  plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable,
#                 variable, color, xlim, ylim, title, FALSE)
#  plotPrototypes(dataTrimowa, prototypes, bustSizes$nsizes, bustVariable,
#                 variable, color, xlim, ylim, title, TRUE)

## ----hipam,eval=FALSE,tidy=FALSE-----------------------------------------
#  dataHipam <- sampleSpanishSurvey
#  bust <- dataHipam$bust
#  bustSizes <- bustSizesStandard(seq(74, 102, 4), seq(107, 131, 6))

## ----hipam2,eval=FALSE,tidy=FALSE----------------------------------------
#  type <- "IMO"
#  maxsplit <- 5 ; orness <- 0.7
#  ah <- c(23, 28, 20, 25, 25)
#  
#  set.seed(2013)
#  res_hipam <- list()
#  for(i in 1 : (bustSizes$nsizes - 1)){
#   data =  dataHipam[(bust >= bustSizes$bustCirc[i]) &
#                    (bust < bustSizes$bustCirc[i + 1]), ]
#   dataMat <- as.matrix(data)
#   res_hipam[[i]] <- hipamAnthropom(dataMat, maxsplit = maxsplit,
#                                   orness = orness, type = type,
#                                   ah = ah, verbose = FALSE)
#  }

## ----hipam3,eval=FALSE,tidy=FALSE----------------------------------------
#  fitmodels <- anthrCases("anthropometry", "HipamAnthropom", res_hipam,
#                          oneSize = FALSE, bustSizes$nsizes)
#  outliers <- trimmOutl("HipamAnthropom", res_hipam, oneSize = FALSE,
#                        bustSizes$nsizes)

## ----hipam4,eval=FALSE,tidy=FALSE----------------------------------------
#  bustVariable <- "bust"
#  xlim <- c(70, 150)
#  color <- c("black", "red", "green", "blue", "cyan", "brown", "gray",
#             "deeppink3", "orange", "springgreen4", "khaki3", "steelblue1")
#  
#  variable <- "hip"
#  ylim <- c(80, 160)
#  title <- "Fit models HIPAM_IMO \n bust vs hip"
#  title_outl <- "Outlier women HIPAM_IMO \n bust vs hip"
#  
#  plotPrototypes(dataHipam, fitmodels, bustSizes$nsizes, bustVariable,
#                 variable, color, xlim, ylim, title, FALSE)
#  plotTrimmOutl(dataHipam, outliers, bustSizes$nsizes, bustVariable,
#                variable, color, xlim, ylim, title_outl)

## ----TDDclust,eval=FALSE,tidy=FALSE--------------------------------------
#  dataTDDcl <- sampleSpanishSurvey[1 : 25, c(2, 3, 5)]
#  dataTDDcl_aux <- sampleSpanishSurvey[1 : 25, c(2, 3, 5)]

## ----TDDclust2,eval=FALSE,tidy=FALSE-------------------------------------
#  numClust <- 3 ; alpha <- 0.01 ; lambda <- 0.5 ; niter <- 5
#  Th <- 0 ; T0 <- 0 ; simAnn <- 0.9
#  
#  set.seed(2014)
#  res_TDDcl <- TDDclust(dataTDDcl, numClust, lambda, Th, niter, T0, simAnn,
#                        alpha, dataTDDcl_aux, verbose = FALSE)

## ----TDDclust3,eval=FALSE,tidy=FALSE-------------------------------------
#  table(res_TDDcl$NN[1,])
#  #1  2  3
#  #5 10  9
#  res_TDDcl$Cost
#  #[1] 0.3717631
#  res_TDDcl$klBest
#  #[1] 3

## ----TDDclust4,eval=FALSE,tidy=FALSE-------------------------------------
#  prototypes <- anthrCases("anthropometry", "TDDclust", res_TDDcl)
#  trimmed <- trimmOutl("TDDclust", res_TDDcl, oneSize = FALSE)

## ----ssa,eval=FALSE,tidy=FALSE-------------------------------------------
#  landmarksNoNa <- na.exclude(landmarksSampleSpaSurv)
#  numLandmarks <- (dim(landmarksNoNa)[2]) / 3
#  landmarksNoNa_First50 <- landmarksNoNa[1 : 50, ]
#  numIndiv <- dim(landmarksNoNa_First50)[1]

## ----ssa1,eval=FALSE,tidy=FALSE------------------------------------------
#  array3D <- array3Dlandm(numLandmarks, numIndiv, landmarksNoNa_First50)

## ----ssa2,eval=FALSE,tidy=FALSE------------------------------------------
#  numClust <- 3 ; alpha <- 0.01 ; algSteps <- 5 ; niter <- 5 ; stopCr <- 0.0001
#  set.seed(2013)
#  res_kmeansProc <- trimmedLloydShapes(array3D, numIndiv, alpha, numClust,
#                                       algSteps, niter, stopCr,
#                                       verbose = FALSE)

## ----ssa3,eval=FALSE,tidy=FALSE------------------------------------------
#  clust_kmeansProc <- res_kmeansProc$asig
#  table(clust_kmeansProc)
#  #1  2  3
#  #19 18 12
#  prototypes <- anthrCases("anthropometry", "kmeansProcrustes", res_kmeansProc)

## ----ssa4,eval=FALSE,tidy=FALSE------------------------------------------
#  trimmed <- trimmOutl("kmeansProcrustes", res_kmeansProc, oneSize = FALSE)

## ----ssa5,eval=FALSE,tidy=FALSE------------------------------------------
#  data_First50 <- sampleSpanishSurvey[1 : 50, ]
#  data_First50_notrimm <- data_First50[-trimmed, ]
#  boxplot(data_First50_notrimm$necktoground ~ as.factor(clust_kmeansProc),
#          main = "Neck to ground")

## ----ssa6,eval=FALSE,tidy=FALSE------------------------------------------
#  projShapes(1, array3D, clust_kmeansProc, prototypes)
#  legend("topleft", c("Registrated data", "Mean shape"),
#                  pch = 1, col = 1:2, text.col = 1:2)
#  title("Procrustes registrated data for cluster 1 \n
#                  with its mean shape superimposed", sub = "Plane xy")

## ----AA,eval=FALSE,tidy=FALSE--------------------------------------------
#  USAFSurvey_First50 <- USAFSurvey[1 : 50, ]
#  variabl_sel <- c(48, 40, 39, 33, 34, 36)
#  USAFSurvey_First50_inch <- USAFSurvey_First50[,variabl_sel] / (10 * 2.54)
#  USAFSurvey_preproc <- preprocessing(USAFSurvey_First50_inch, TRUE,
#                                      0.95, TRUE)

## ----AA3,eval=FALSE,tidy=FALSE-------------------------------------------
#  set.seed(2010)
#  numArch <- 10 ; numRep <- 20
#  lass <- stepArchetypesMod(data = USAFSurvey_preproc$data, numArch=1:numArch,
#                            numRep = numRep, verbose = FALSE)
#  screeplot(lass)

## ----AA4,eval=FALSE,tidy=FALSE-------------------------------------------
#  numArchoid <- 3
#  res_archoids_ns <- archetypoids(numArchoid, USAFSurvey_preproc$data,
#                                  huge = 200, step = FALSE, ArchObj = lass,
#                                  nearest = "cand_ns" , sequ = TRUE)
#  res_archoids_alpha <- archetypoids(numArchoid, USAFSurvey_preproc$data,
#                                     huge = 200, step = FALSE, ArchObj = lass,
#                                     nearest = "cand_alpha", sequ = TRUE)
#  res_archoids_beta <- archetypoids(numArchoid, USAFSurvey_preproc$data,
#                                    huge = 200, step = FALSE, ArchObj = lass,
#                                    nearest = "cand_beta", sequ = TRUE)
#  
#  
#  boundaries_ns <- anthrCases("ergonomics", resMethod = res_archoids_ns)
#  boundaries_alpha <- anthrCases("ergonomics", resMethod = res_archoids_alpha)
#  boundaries_beta <- anthrCases("ergonomics", resMethod = res_archoids_beta)

## ----AA5,eval=FALSE,tidy=FALSE-------------------------------------------
#  matPer <- matPercs(boundaries_ns, USAFSurvey_preproc$data)

## ----AA6,eval=FALSE,tidy=FALSE-------------------------------------------
#  barplot(matPer, beside = TRUE, main = paste(numArchoid,
#                                              " archetypoids", sep = ""),
#          ylim = c(0, 100), ylab = "Percentile")

