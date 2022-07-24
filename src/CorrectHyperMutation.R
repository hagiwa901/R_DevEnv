N <- 30 #荷物数
popSize <- 100 #集団サイズ
Ng <- 500 #世代数
tg <- 30 #30世代ごとに変異が入る
mr <-  0.005 #通常の突然変異率
mh <- 0.3 #下回った時の突然変異率
population <- matrix(round(runif(popSize*N)), ncol=N)
set.seed(123)
a <- runif(30)*500 #荷物の重さ
c <- runif(30)*500 #荷物の価値
b1 <- sum(a) * 0.5 #ナップサックに収納できる総重量
lmb <- c(b1, b1-b1*0.2, b1-b1*0.4, b1-b1*0.6, b1-b1*0.8)
ts <- 1 #回した世代の数
wl <- lmb[1]

# capacitor gene
capacitor <- matrix(runif(popSize*N), ncol=N)
               for (i in 1:popSize) {
               for (j in 1:N) {
               	if (capacitor[i,j] < 0.1) {
               		capacitor[i,j] <- 1
               	} else {
               		capacitor[i,j] <- 0
               	}
               }
               }

m <- mr
mut <- c(m*10000)
mfx <- c()

# calculation of fitness
calcFitness <- function(v, wl, a, c)  {
   if(sum(v*a) <= wl)  {
   	  fitness <- sum(v*c)
   } else {
   	  fitness <- (sum(a) - sum(v*a))*0.01 
   }
   
   return (fitness)
}


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
   
   # capacitor geneによる遺伝子補正
   if((mean(mfx[ts:i])-mean(mfx[ts:(i-1)])) < 0){
   	for(h in 1:popSize*N){
   		if(population[h]==0 & capacitor[h]==1){
   			population[h] <- capacitor[h]
   		}
   	}
   }
   
  # general geneの交叉元
  v <- sample(1:popSize)
  popr1 <- matrix(rep(0, popSize*N), ncol=N)
  popr2 <- matrix(rep(0, popSize*N), ncol=N)
  
  # capacitor geneの交叉元
  popr3 <- matrix(rep(0, popSize*N), ncol=N)
  popr4 <- matrix(rep(0, popSize*N), ncol=N)
  
  # recombination
  for (j in 1:popSize)  {
  	msk <- round(runif(N))
  	for (k in 1:N) {
  		if (msk[k] == 1) {
  		    popr1[j, k] <- population[v[j], k]	
  		    popr2[j, k] <- population[j, k]
  		    popr3[j, k] <- population[v[j], k]	
  		    popr4[j, k] <- population[j, k]	  		    
  		} else {
  		    popr2[j, k] <- population[v[j], k]	
  		    popr1[j, k] <- population[j, k]
  		    popr4[j, k] <- population[v[j], k]	
  		    popr3[j, k] <- population[j, k]	    			
  		}
  	}
  }
  
  # mutation
  for (j in 1:popSize)  {
     for (k in 1:N) {
   	    if (runif(1) < m)  popr1[j, k] <- abs(popr1[j, k]-1)
   	    if (runif(1) < m)  popr2[j, k] <- abs(popr2[j, k]-1)
   	    if (runif(1) < m)  popr3[j, k] <- abs(popr3[j, k]-1)
   	    if (runif(1) < m)  popr4[j, k] <- abs(popr4[j, k]-1)
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
     f3 <- calcFitness(popr3[j,], wl, a, c)
     f4 <- calcFitness(popr4[v2[j],], wl, a, c)
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
     if(f3 > f4){
     	capacitor <- popr3
     } else{
     	capacitor <- popr4
     }
  }
  mfx <- c(mfx, mx)
  population <- popS
}

plot(1:501, mfx, ty='l', xlim=c(0,502), ylim=c(0,5500))
par(new=T)
plot(1:501, wlList, ty='l', xlim=c(0,502), ylim=c(0,5500), col="red")
par(new=T)
plot(1:501, mut, ty='l', xlim=c(0,502), ylim=c(0,5500), col="blue")
