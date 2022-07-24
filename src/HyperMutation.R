N <- 30
popSize <- 100
Ng <- 500
tg <- 30
mr <-  0.005
mh <- 0.3
population <- matrix(round(runif(popSize*N)), ncol=N)
population2 <- matrix(runif(popSize*N), ncol=N)
               for (i in 1:popSize) {
               for (j in 1:N) {
               	if (population2[i,j] < 0.1) {
               		population2[i,j] <- 1
               	} else {
               		population2[i,j] <- 0
               	}
               }
               }
set.seed(123)
a <- runif(30)*500
c <- runif(30)*500
b1 <- sum(a) * 0.5
lmb <- c(b1, b1-b1*0.2, b1-b1*0.4, b1-b1*0.6, b1-b1*0.8)
ts <- 1
wl <- lmb[1]

m <- mr
mut <- c(m*10000)
mfx <- c()

#  calculation of fitness
calcFitness <- function(v, wl, a, c)  {
   if(sum(v*a) <= wl)  {
   	  fitness <- sum(v*c)
   } else {
   	  fitness <- (sum(a) - sum(v*a))*0.01 
   }
   
   return (fitness)
}

calcFitness2 <- function(v, w, wl, a, c)  {
	v2 <- c() 
	if(all(w == 1) && all(v == 0)) {
		v <- 1
	}
	if(all(w == 0) && all(v == 0)) {
		v2 <- 0
	}
	if(sum(v*a) <= wl)  {
   	  fitness <- sum(v2*c)
    } else {
   	  fitness <- (sum(a) - sum(v2*a))*0.01 
    }
    return (fitness)
}


#population3 <- function(population2, population) {
#	for (j in 1:popSize) {
#	if(population2[j,] == 1) {
#		if(population[j,] == 0) {
#			population[j,] == 1
#		} 
#	}
#	}
#}

fitness <- c()
indiv <- 0
for (j in 1:popSize)  {
    fv <- calcFitness(population[j,], wl, a, c)
    if (j == 1) {
    	fitness <- c(fitness, fv)
    	indiv <- 1
    } else {
       if (fv > max(fitness))  indiv <- j
       fitness <- c(fitness, fv)
       }
       
     fs <- calcFitness2(population[j,], population2[j,], wl, a, c)
     if (j == 1) {
    	fitness <- c(fitness, fs)
    	indiv <- 1
    } else {
       if (fs > max(fitness))  indiv <- j
       fitness <- c(fitness, fs)
    } 
}  
mfx <- c(mfx, max(fitness))
wlList <- c(wl)

for (i in 1:Ng)  {
# random perturbation of total weight 
   if ((i-ts) > 30)  {
   	   if (runif(1) < 0.05) {
   	   	   ts <- i
   	   	   wl <- lmb[sample(1:5)[1]]
   	   }
   }
   wlList <- c(wlList, wl)
   	   
# mutation rate setting     
   if (i > 1) {
   	  if ((mean(mfx[ts:i]) - mean(mfx[ts:(i-1)])) < 0)  {
   	  	  m <- mh
   	  }  else  {
   	  	  m <- mr
   	  } 
   }	
   mut <- c(mut, m*10000)
   
# recombination
  v <- sample(1:popSize)
  popr1 <- matrix(rep(0, popSize*N), ncol=N)
  popr2 <- matrix(rep(0, popSize*N), ncol=N)
  
  for (j in 1:popSize)  {
  	msk <- round(runif(N))
  	for (k in 1:N) {
  		if (msk[k] == 1) {
  		    popr1[j, k] <- population[v[j], k]	
  		    popr2[j, k] <- population[j, k]	  		    
  		} else {
  		    popr2[j, k] <- population[v[j], k]	
  		    popr1[j, k] <- population[j, k]	    			
  		}
  	}
  }

# mutation

  for (j in 1:popSize)  {
     for (k in 1:N) {
   	    if (runif(1) < m)  popr1[j, k] <- abs(popr1[j, k]-1)
   	    if (runif(1) < m)  popr2[j, k] <- abs(popr2[j, k]-1)
     }
  }

# tornament + elite
  v2 <- sample(1:popSize)
  indiv <- 1
  popS <- population[indiv,]
  mx <- calcFitness(popS, wl, a, c)
  for (j in 1:(popSize-1))  {
     f1 <- calcFitness(popr1[j,], wl, a, c)
     f2 <- calcFitness(popr2[v2[j],], wl, a, c)
     if (f1 > f2)  {
     	popS <- rbind(popS, popr1[j,])
     	if (f1 > mx)  {
     		mx <- f1
     		indiv <- (j + 1)
     	}
     } else {
     	popS <- rbind(popS, popr2[v2[j],])
     	if (f2 > mx) {
     		mx <- f2
     		indiv <- (j + 1)
     	}
     }
	warnings()
  }
  mfx <- c(mfx, mx)
  population <- popS
}

plot(1:501, mfx, ty='l', xlim=c(0,502), ylim=c(0,5500))
par(new=T)
plot(1:501, wlList, ty='l', xlim=c(0,502), ylim=c(0,5500), col="red")
par(new=T)
plot(1:501, mut, ty='l', xlim=c(0,502), ylim=c(0,5500), col="blue")