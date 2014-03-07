WeightsMixtureUB <- function(orness,dimension){
  if(orness == .5){
    w = rep(1/dimension,dimension)
   }else{
     lambda = 0.5
     prob0 = 3/2 - 2 * orness
     w = lambda * dbinom(0:(dimension-1), size = dimension-1, prob = prob0) + 
       (1 - lambda) * rep(1/dimension,dimension)
    }
  w
}

