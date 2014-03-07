HartiganShapes <- function(dg,K,Nsteps=10,niter=10,stopCr=0.0001,simul,initLl,initials,print){

 time_iter <- list() 
 comp_time <- c()
 list_ic1 <- list()
 list_ic1_step <- list()
 vect_all_rate <- c()

 ll <- 1 : K
 dist <- matrix(0, dim(dg)[3], K)

 if(print){
  print(Sys.time())
 }  
 time_ini <- Sys.time()

 #Initialize the objective function by a large enough value:
 vopt <- 1e+08

 #Ramdon restarts:
 for(iter in 1 : niter){

 wss_step <- list() 
 
 if(print){
  cat("New iteration")
  print(iter)

 cat("Optimal value with which this iteration starts:")
 print(vopt)
 }
 #STEP 1: For each point I, find its two closest centers, IC1(I) and IC2(I). Assign the point to IC1(I):
 meanshapes <- 0 ; mean_sh <- list()
 ic1 <- c() ; ic2 <- c() ; dt <- c()  ; nc <- c() #number of points in each cluster.
 an1 <- c()   ; an2 <- c() ; itran <- c() ; ncp <- c()
 indx <- c() ; d <- c() ; live <- c() ; wss <- c()

 n <- dim(dg)[3]

 initials_hart <- list()
 if(initLl){  
  initials_hart[[iter]] <- initials[[iter]]
 }else{
   initials_hart[[iter]] <- sample(1:n, K, replace = F)
  }
 if(print){
  cat("Initial values of this iteration:")
  print(initials_hart[[iter]]) 
 }
 meanshapes <- dg[,,initials_hart[[iter]]] 

  for(i in 1 : n){
   ic1[i] = 1
   ic2[i] = 2
 
   for(il in 1 : 2){
    dt[il] = (riemdist(dg[,,i], meanshapes[,,il]))^2
   }

   if(dt[2] < dt[1]){
    ic1[i] = 2
    ic2[i] = 1
    temp = dt[1]
    dt[1] = dt[2]
    dt[2] = temp
   }

   if(simul == FALSE){
    for(l in 3 : K){
     db = (riemdist(dg[,,i], meanshapes[,,l]))^2
     if(db < dt[2]){
       if(dt[1] <= db){
        dt[2] = db
        ic2[i] = l
       }else{
         dt[2] = dt[1]
         ic2[i] = ic1[i]
         dt[1] = db
         ic1[i] = l
        }
     }
    }
   } 
  }
  
  #STEP 2: Update the cluster centres to be the averages of points contained within them.
  #Check to see if there is any empty cluster at this stage:
  for(l in 1 : K){
   nc[l] <- table(ic1)[l]

   if(nc[l] == 0){
   stop("At least one cluster is empty after the initial assignment. A better set of initial cluster centers is needed.")
   } 
  }

  for(l in 1 : K){
   aa = nc[l] 
   meanshapes[,,l] = procGPA(dg[, , ic1 == l], distances = T, pcaoutput = T)$mshape

   #Initialize AN1, AN2, ITRAN and NCP.
   #AN1(L) = NC(L) / (NC(L) - 1)
   #AN2(L) = NC(L) / (NC(L) + 1)
   #ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
   #         = 0 otherwise

   #In the optimal-transfer stage, NCP(L) stores the step at which cluster L is last updated.
   #In the quick-transfer stage, NCP(L) stores the step at which cluster L is last updated plus M:
   an2[l] = aa / (aa + 1)
   if(1 < aa){
    an1[l] = aa / (aa - 1)
   }else{
     an1[l] = Inf
    }
   itran[l] = 1
   ncp[l] = -1
  }

  indx <- 0
  d[1:n] = 0
  live[1:K] = 0

  for(step in 1 : Nsteps){
   #In this stage, there is only one pass through the data. Each point is re-allocated, if necessary, to the 
   #cluster that will induce the maximum reduction in within-cluster sum of squares:
   lis <- optraProcrustes(dg,n,meanshapes,K,ic1,ic2,nc,an1,an2,ncp,d,itran,live,indx)
   
   meanshapes <- lis[[1]] ; ic1 <- lis[[2]] ; ic2 <- lis[[3]] ; nc <- lis[[4]] ; an1 <- lis[[5]] ; an2 <- lis[[6]] ; ncp <- lis[[7]]
   d <- lis[[8]] ; itran <- lis[[9]] ; live <- lis[[10]] ; indx <- lis[[11]] 

   #Each point is tested in turn to see if it should be re-allocated to the cluster to which it is most likely 
   #to be transferred, IC2(I), from its present cluster, IC1(I). Loop through the data until no further change 
   #is to take place:
   lis1 <- qtranProcrustes(dg,n,meanshapes,K,ic1,ic2,nc,an1,an2,ncp,d,itran,indx)
   
   meanshapes <- lis1[[1]] ; ic1 <- lis1[[2]] ; ic2 <- lis1[[3]] ; nc <- lis1[[4]] ; an1 <- lis1[[5]] ; an2 <- lis1[[6]] ; ncp <- lis1[[7]] 
   d <- lis1[[8]] ; itran <- lis1[[9]]  ;  indx <- lis1[[10]] ; icoun <- lis1[[11]] 

   mean_sh[[step]] <- meanshapes

   #NCP has to be set to 0 before entering OPTRA:
   for( l in 1 : K ){
    ncp[l] = 0
   }
   
   #Compute the within-cluster sum of squares for each cluster:
   wss <- vector("list", K) 
    for(num_cl in 1 : K){ 
     wss[[num_cl]] <- 0
     dg_cl <- array(0, dim = c(n, 3, table(ic1)[num_cl])) #table(ic1)[num_cl] is the number of observations that
     #belong to each cluster.
     dg_cl <- dg[,,ic1 == num_cl]
     
     distances <- c()
     for(num_mujs_cl in 1:table(ic1)[num_cl]){
      distances[num_mujs_cl] <- riemdist(dg_cl[,,num_mujs_cl], meanshapes[,,num_cl])^2
     }

     wss[[num_cl]] <-  sum(distances) / n
    }
    
    #Total within-cluster sum of squares:
    wss_step[[step]] <- sum(unlist(wss)) 
    list_ic1_step[[step]] <- ic1 

    if(print){
     paste(cat("Clustering of the Nstep", step, ":\n"))
     print(table(list_ic1_step[[step]])) 
    }
    if(print){
     if(iter <= 10){ 
      paste(cat("Objective function of the Nstep", step))
      print(wss_step[[step]]) 
     }
    } 
    
     if(step > 1){
      aux <- wss_step[[step]] 
      aux1 <- wss_step[[step-1]]
      if( ((aux1 - aux) / aux1) < stopCr ){
       break         
      }
     }
   }#The Nsteps loop ends here.

    #Calculus of the objective function (the total within-cluster sum of squares):
    wss1 <- vector("list", K)
    for(num_cl in 1 : K){ 
     wss1[[num_cl]] <- 0
     dg_cl1 <- array(0, dim = c(n, 3, table(ic1)[num_cl])) 
     dg_cl1 <- dg[,,ic1 == num_cl]

     distances1 <- c()
 
     for(num_mujs_cl in 1:table(ic1)[num_cl]){ 
      distances1[num_mujs_cl] <- riemdist(dg_cl1[,,num_mujs_cl], meanshapes[,,num_cl])^2
     }

     wss1[[num_cl]] <-  sum(distances1) / n
    }
    #Total within-cluster sum of squares:
    wss_step1 <- 0
    wss_step1 <- sum(unlist(wss1)) 

    #Change the optimal value and the optimal centers (copt) if a reduction in the objective function happens:
    if(wss_step1 > min(unlist(wss_step))){ 
     if(min(unlist(wss_step)) < vopt){
      vopt <- min(unlist(wss_step))
      if(print){
       #Improvements in the objective functions are printed:
       cat("optimal")
       print(vopt)
      }
      optim_wss <- which.min(unlist(wss_step)) 
      copt <- mean_sh[[optim_wss]] #optimal centers.
      ic1_opt <- list_ic1_step[[optim_wss]]
     }
    }else if(wss_step1 < vopt){
     vopt <- wss_step1
     if(print){
      #Improvements in the objective functions are printed:
      cat("optimal")
      print(vopt)
     }
     optim_wss <- which.min(unlist(wss_step)) 
     copt <- mean_sh[[optim_wss]] #optimal centers.
     ic1_opt <- list_ic1_step[[optim_wss]]
    }

     time_iter[[iter]] <- Sys.time()

     if(iter == 1){
      comp_time[1] <- difftime(time_iter[[iter]], time_ini, units = "mins")
      if(print){
       cat("Computational time of this iteration: \n")
       print(time_iter[[iter]] - time_ini)
      } 
     }else{
       comp_time[iter] <- difftime(time_iter[[iter]], time_iter[[iter-1]], units = "mins")
       if(print){
        cat("Computational time of this iteration: \n")    
        print(time_iter[[iter]] - time_iter[[iter - 1]])
       }  
      }   
   if(print){
    cat("Optimal clustering of this iteration: \n")
   }  
   optim_wss <- which.min(unlist(wss_step)) 
   list_ic1[[iter]] <- list_ic1_step[[optim_wss]] 
   if(print){
    print(table(list_ic1[[iter]]))
   } 

   if(simul){
    #Allocation rate: 
    as1 <- table(list_ic1[[iter]][1:(n/2)])                
    as2 <- table(list_ic1[[iter]][seq(n/2 + 1,n)]) 
    if( max(as1) != n/2 & max(as2) != n/2 ){ 
     suma <- min(as1) + min(as2)
     all_rate <- 1 - suma / n
    }else if( (max(as1) == n/2 & max(as2) != n/2) || (max(as1) != n/2 & max(as2) == n/2) ){
      minim <- min(min(as1),min(as2))
      all_rate <- 1 - minim / n
     }else if( max(as1) == n/2 & max(as2) == n/2 ){
       all_rate <- 1
      }
    vect_all_rate[iter] <- all_rate 
    if(print){
     cat("Optimal allocation rate in this iteration:")
     print(all_rate)
    }    
   }
 }#The niter loop ends here.

 if(simul){
  return(list(compTime=comp_time,AllRate=vect_all_rate))
 }else{
   return(list(ic1=ic1_opt,copt=copt,vopt=vopt))
  }
}

