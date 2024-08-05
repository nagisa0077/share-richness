#### function ####
Est <-  function(data1,data2,T1,T2){
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
  
  w1 <- t1/T1
  w2 <- t2/T2
  beta1 <- max(0,(f[1]/max(1,f[4])-1)*t1)
  beta2 <- max(0,(f[2]/max(1,f[5])-1)*t2)
  
  #Q00
  Q00 <- ((t1-1)/t1) * ((t2-1)/t2)* (((beta1/t1)+1)*((T1-t1)/(T1+beta1))) * (((beta2/t2)+1)*((T2-t2)/(T2+beta2))) * f[3]
  
  #Q0+
  Q0_ <- ((t1-1)/t1)*(((beta1/t1)+1)*((T1-t1)/(T1+beta1)))* f[1]
  
  #Q+0
  Q_0 <- ((t2-1)/t2)* (((beta2/t2)+1)*((T2-t2)/(T2+beta2))) * f[2]
  
  # S
  ans <- s.obs + Q00 + Q0_ + Q_0
  return(ans)
}
Var <-  function(data1,data2,T1,T2){
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
  var <-  function(s.obs,f1,f2,f3,f4,f5,t1,t2,T1,T2){
    w1 = t1/T1
    w2 = t2/T2
    beta1 <- max(0,((f1/max(1,f4))-1)*t1)
    beta2 <- max(0,((f2/max(1,f4))-1)*t2)
    
    #Q00
    Q00 <- ((t1-1)/t1)* ((t2-1)/t2)* ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f3
    
    #Q0+
    Q0_ <- ((t1-1)/t1)*  ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * f1
    
    #Q+0
    Q_0 <- ((t2-1)/t2)* ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f2
    
    ans <- s.obs + Q00 + Q0_ + Q_0
    return(ans)
  }
  SD  <-  function(X,Y,t1,t2,T1,T2,O,d){
    f <- f.fun(X,Y)
    # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5]
    V <- c((var(O,f[1]+d,f[2],f[3],f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
           (var(O,f[1],f[2]+d,f[3],f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
           (var(O,f[1],f[2],f[3]+d,f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
           (var(O,f[1],f[2],f[3],f[4]+d,f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
           (var(O,f[1],f[2],f[3],f[4],f[5]+d,t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,-1)
    return(V)
  }
  covmatrix <-  function(X,Y,s,O){
    f <- c(f.fun(X,Y)) #用Sobs去建立covarance matrix
    f <- c(f[1:5],s-O)
    cov_matrix <- matrix(0, nrow = 6, ncol = 6)
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
  
  sd <-  SD(X,Y,t1,t2,T1,T2,O,.01)
  matrix <-  covmatrix(X,Y,Est(data1,data2,T1,T2),O)
  SD <-  sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}

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
  sample_exist = sample(TT,t)
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

#### test result ####
Est(data1,data2,T1,T2)
Var(data1,data2,T1,T2)
