#### simulation population & sample ####
T1 = T2 = 100
S = 300
population = function(TT,theta,S){
  Z = sapply(1:S, function(j) rbinom(1,TT,theta[j]))
  population = matrix(0,S,TT) 
  for(i in 1:S){
    exist = sample(TT,Z[i])
    population[i,exist] = 1
  }
  return(population)
}
Sample = function(TT,t,population){
  sample_exist = sample(TT,t,T)
  sample = population[,sample_exist]
  # sample = rowSums(population[,sample_exist])
  return(sample)
}
set.seed(123)
p2.1 = runif(S,0,.24)

# Broken-Stick
c = .9
set.seed(123)
p3.1 <- rexp(S)
p3.1 <- p3.1/(max(p3.1)*c)
for(i in 1:S){
  p3.1[i] = ifelse(p3.1[i]>1, 1, p3.1[i])
}

Z1 = population(T1,p2.1,S)
Z2 = population(T1,p3.1,S)

data1 = Sample(T1,50,Z1)
data2 = Sample(T2,50,Z2)

#### function ####
Est <-  function(data1,data2){
  X <-  rowSums(data1)
  Y <-  rowSums(data2)
  f.fun <-  function(X,Y){
    f <-  c()
    # X = X[I] #X的共有物種
    # Y = Y[I] #Y的共有物種
    #f1.
    f[1] <- sum(X==1 & Y!=0)
    #f.1
    f[2] <- sum(X!=0 & Y==1)
    #f11
    f[3] <- sum(X==1 & Y==1)
    #f2.
    f[4] <- sum(X==2 & Y!=0)
    #f.2
    f[5] <- sum(X!=0 & Y==2)
    #f22
    f[6] <- sum(X==2 & Y==2)
    #f3.
    f[7] <- sum(X==3 & Y!=0)
    #f.3
    f[8] <- sum(X!=0 & Y==3)
    #f33
    f[9] <- sum(X==3 & Y==3)
    #D
    f[10] <- sum(X==0 | Y==0)
    return(f)
  }
  f  <- f.fun(X,Y)
  I <-  which(X*Y>0)
  s.obs <-  length(I)
  
  t1 <-  ncol(data1)
  t2 <-  ncol(data2)
  
  K1 <-  (t1-1)/t1
  K2 <- (t2-1)/t2
  
  # bias大，SE小
  k1 <- max(1/2,min(1,2*f[4]^2/(3*max(1,f[1])*max(1,f[7]))))
  k2 <- max(1/2,min(1,2*f[5]^2/(3*max(1,f[2])*max(1,f[8]))))
  
  
  ans <- if(f[4]==0 | f[5]==0 | f[6]==0){s.obs +
      K1*(f[1]*(f[1]-1)/(2*(f[4]+1)))*(2-k1) +
      K2*(f[2]*(f[2]-1)/(2*(f[5]+1)))*(2-k2) +
      K1*K2*(f[3]*(f[3]-1)/(4*(f[6]+1)))*(2-k1)*(2-k2)
  }else{s.obs +
      K1*(f[1]*(f[1]-1)/(2*f[4]))*(2-k1) +
      K2*(f[2]*(f[2]-1)/(2*f[5]))*(2-k2) +
      K1*K2*(f[3]*(f[3]-1)/(4*f[6]))*(2-k1)*(2-k2)
  }
  return(ans)
}
Var <-  function(data1,data2){
  f.fun <-  function(X,Y){
    f <-  c()
    # X = X[I] #X的共有物種
    # Y = Y[I] #Y的共有物種
    #f1.
    f[1] <- sum(X==1 & Y!=0)
    #f.1
    f[2] <- sum(X!=0 & Y==1)
    #f11
    f[3] <- sum(X==1 & Y==1)
    #f2.
    f[4] <- sum(X==2 & Y!=0)
    #f.2
    f[5] <- sum(X!=0 & Y==2)
    #f22
    f[6] <- sum(X==2 & Y==2)
    #f3.
    f[7] <- sum(X==3 & Y!=0)
    #f.3
    f[8] <- sum(X!=0 & Y==3)
    #f33
    f[9] <- sum(X==3 & Y==3)
    #D
    f[10] <- sum(X==0 | Y==0)
    return(f)
  }
  var <-  function(s.obs,f1,f2,f3,f4,f5,f6,f7,f8,t1,t2){
    K1 = (t1-1)/t1
    K2 = (t2-1)/t2
    
    # bias大，SE小
    k1 <- max(1/2,min(1,2*f4^2/(3*max(1,f1)*max(1,f7))))
    k2 <- max(1/2,min(1,2*f5^2/(3*max(1,f2)*max(1,f8))))
    
    ans <- if(f4==0 | f5==0 | f6==0){s.obs +
        K1*(f1*(f1-1)/(2*(f4+1)))*(2-k1) +
        K2*(f2*(f2-1)/(2*(f5+1)))*(2-k2) +
        K1*K2*(f3*(f3-1)/(4*(f6+1)))*(2-k1)*(2-k2)
    }else{s.obs +
        K1*(f1*(f1-1)/(2*f4))*(2-k1) +
        K2*(f2*(f2-1)/(2*f5))*(2-k2) +
        K1*K2*(f3*(f3-1)/(4*f6))*(2-k1)*(2-k2)
    }
    return(ans)
  }
  SD  <-  function(X,Y,t1,t2,O,d){
    f <-f.fun(X,Y)
    V <- c((var(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7]+d,f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
          (var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8]+d,t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,-1)
    return(V)
  }
  covmatrix <-  function(X,Y,s,O){
    f <- c(f.fun(X,Y)) #用Sobs去建立covarance matrix
    f <- c(f[1:8],s-O)
    cov_matrix <- matrix(0, nrow = 9, ncol = 9)
    for(i in 1:length(f)){
      for(j in 1:length(f)){
        if(i == j){
          cov_matrix[i,i] = f[i]*(1-(f[i]/s))
        }else{
          cov_matrix[i,j] = -f[i]*(f[j]/s)
        }
      }
    }
    return(cov_matrix)
  }
  
  X  <-  rowSums(data1)
  Y <-  rowSums(data2)
  
  f  <- f.fun(X,Y)
  I <-  which(X*Y>0)
  O <-  length(I)
  
  t1 <-  ncol(data1)
  t2 <-  ncol(data2)
  
  sd = SD(X,Y,t1,t2,O,.01)
  matrix = covmatrix(X,Y,Est(data1,data2),O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}

#### test result ####
Est(data1,data2)
Var(data1,data2)
