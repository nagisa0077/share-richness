################################# function #####################################
f.fun = function(X,Y){
  f = c()
  # X = X[I] #X的共有物種
  # Y = Y[I] #Y的共有物種
  #f1.
  f[1] = sum(X==1 & Y!=0)
  #f.1
  f[2] = sum(X!=0 & Y==1)
  #f11
  f[3] = sum(X==1 & Y==1)
  #f2.
  f[4] = sum(X==2 & Y!=0)
  #f.2
  f[5] = sum(X!=0 & Y==2)
  #f22
  f[6] = sum(X==2 & Y==2)
  #f3.
  f[7] = sum(X==3 & Y!=0)
  #f.3
  f[8] = sum(X!=0 & Y==3)
  #f33
  f[9] = sum(X==3 & Y==3)
  #D
  f[10] = sum(X==0 | Y==0)
  return(f)
}
f = function(X){
  f = c()
  f[1] = sum(X==1)
  f[2] = sum(X==2)
  f[3] = sum(X==3)
  f[4] = sum(X==4)
  f[5] = sum(X>=5)
  return(f)
}
obs = function(X,Y){
  I = which(X*Y>0)
  s.obs = length(I)
  return(s.obs)
}
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
  sample = rowSums(population[,sample_exist])
  return(sample)
}
fill = function(Z,S12,TT){
  I = which(rowSums(Z)[1:S12] == 0)
  II = sample(TT,length(I))
  for(i in 1:length(I)){
    Z[I[i],II[i]] = 1
  }
  return(Z)
}

# estimater
Pan = function(X,Y,t1,t2){
  f = f.fun(X,Y)
  I = which(X*Y>0)
  s.obs = length(I)
  
  K1 = (t1-1)/t1
  K2 = (t2-1)/t2
  
  #
  if(f[4]==0 | f[5]==0 | f[6]==0){
    ans = s.obs + K1*f[1]*(f[1]-1)/(2*(f[4]+1)) +K2*f[2]*(f[2]-1)/(2*(f[5]+1)) + K1*K2*f[3]*(f[3]-1)/(4*(f[6]+1))
  }else{
    ans = s.obs + K1*(f[1]^2)/(2*f[4]) + K2*(f[2]^2)/(2*f[5]) + K1*K2*(f[3]^2)/(4*f[6]) 
  }
  return(ans)
} 
BB = function(X,Y,t1,t2){
  f = f.fun(X,Y)
  I = which(X*Y>0)
  s.obs = length(I)
  
  K1 = (t1-1)/t1
  K2 = (t2-1)/t2
  
  # bias大，SE小
  k1 = max(1/2,min(1,2*f[4]^2/(3*max(1,f[1])*max(1,f[7]))))
  k2 = max(1/2,min(1,2*f[5]^2/(3*max(1,f[2])*max(1,f[8]))))
  
  # bias小，SE大
  # k1 = max(1/2,2*f[4]^2/(3*f[1]*f[7]))
  # k2 = max(1/2,2*f[5]^2/(3*f[2]*f[8]))
  
  ans = if(f[4]==0 | f[5]==0 | f[6]==0){s.obs +
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
more = function(X,t){
  f = f(X)
  I = which(X>0)
  s.obs = length(I)
  
  K = (t-1)/t
  
  k = max(1/2,min(1,2*f[2]^2/(3*max(1,f[1])*max(1,f[3]))))
  
  ans = if(f[2]==0){s.obs +
      K * ((f[1]*(f[1]-1))/2) * (2-k)
  }else{s.obs +
      K * (f[1]^2/(2*f[2])) * (2-k)
  }
  return(ans)
}

# var
var = function(s.obs,f1,f2,f3,f4,f5,f6,f7,f8,t1,t2){
  K1 = (t1-1)/t1
  K2 = (t2-1)/t2
  
  # bias大，SE小
  k1 = max(1/2,min(1,2*f4^2/(3*max(1,f1)*max(1,f7))))
  k2 = max(1/2,min(1,2*f5^2/(3*max(1,f2)*max(1,f8))))
  
  # bias小，SE大
  # k1 = max(1/2,2*f[4]^2/(3*f[1]*f[7]))
  # k2 = max(1/2,2*f[5]^2/(3*f[2]*f[8]))
  
  ans = if(f4==0 | f5==0 | f6==0){s.obs +
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
var_Pan = function(s.obs,f1,f2,f3,f4,f5,f6,t1,t2){
  K1 = (t1-1)/t1
  K2 = (t2-1)/t2
  
  #
  if(f4==0 | f5==0 | f6==0){
    ans = s.obs + K1*f1*(f1-1)/(2*(f4+1)) +K2*f2*(f2-1)/(2*(f5+1)) + K1*K2*f3*(f3-1)/(4*(f6+1))
  }else{
    ans = s.obs + K1*(f1^2)/(2*f4) + K2*(f2^2)/(2*f5) + K1*K2*(f3^2)/(4*f6) 
  }
  return(ans)
}
var_m = function(s.obs,f1,f2,f3,t){
  K = (t-1)/t
  
  k = max(1/2,min(1,2*f2^2/(3*max(1,f1)*max(1,f3))))
  
  ans = if(f2==0){s.obs +
      K * ((f1*(f2-1))/2) * (2-k)
  }else{s.obs +
      K * (f1^2/(2*f2)) * (2-k)
  }
  return(ans)
}

# Covmatrix
covmatrix = function(X,Y,s,O){
  f = c(f.fun(X,Y)) #用Sobs去建立covarance matrix
  f = c(f[1:8],s-O)
  cov_matrix = matrix(0, nrow = 9, ncol = 9)
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
covmatrix_Pan = function(X,Y,s,O){
  f = c(f.fun(X,Y)) #用Sobs去建立covarance matrix
  f = c(f[1:6],s-O)
  cov_matrix = matrix(0, nrow = 7, ncol = 7)
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
covmatrix_m = function(X,s,O){
  f = c(f(X)) #用Sobs去建立covarance matrix
  f = c(f[1:3],s-O)
  cov_matrix = matrix(0, nrow = 4, ncol = 4)
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

# population SD
SD  = function(X,Y,t1,t2,O,d){
  f = f.fun(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5]
  V = c((var(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,f[7],f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7]+d,f[8],t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,
        (var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8]+d,t1,t2)-var(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2))/d,-1)
  return(V)
}
SD_Pan  = function(X,Y,t1,t2,O,d){
  f = f.fun(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5];f6=f[6];f7=f[8];f7=f[8]
  V = c((var_Pan(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,
        (var_Pan(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,
        (var_Pan(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,
        (var_Pan(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,
        (var_Pan(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,
        (var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,t1,t2)-var_Pan(O,f[1],f[2],f[3],f[4],f[5],f[6],t1,t2))/d,-1)
  return(V)
}
SD_m  = function(X,t,O,d){
  f = f(X)
  V = c((var_m(O,f[1]+d,f[2],f[3],t)-var_m(O,f[1],f[2],f[3],t))/d,
        (var_m(O,f[1],f[2]+d,f[3],t)-var_m(O,f[1],f[2],f[3],t))/d,
        (var_m(O,f[1],f[2],f[3]+d,t)-var_m(O,f[1],f[2],f[3],t))/d,-1)
  return(V)
}

# SE
se = function(X,Y,t1,t2,O,E,d){
  sd = SD(X,Y,t1,t2,O,d)
  matrix = covmatrix(X,Y,E,O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}
se_Pan = function(X,Y,t1,t2,O,E_Pan,d){
  sd_Pan = SD_Pan(X,Y,t1,t2,O,d)
  matrix_Pan = covmatrix_Pan(X,Y,E_Pan,O)
  SD_Pan = sqrt(t(sd_Pan) %*% matrix_Pan %*% sd_Pan)
  return(SD_Pan)
}
se_m = function(X,t,O,E,d){
  sd = SD_m(X,t,O,d)
  matrix = covmatrix_m(X,E,O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}

# 95 % CI
CI_coverage = function(E,O,SD,s){
  R = as.numeric(exp(1.96*( log(1+SD^2/max(0.1^10,(E-O)^2)))^(1/2)))
  U = as.numeric(O+(E-O)*R)
  L = as.numeric(O+(E-O)/R)
  # U = as.numeric(E + (1.96*SD))
  # L = as.numeric(E - (1.96*SD))
  CI_coverage = sum(ifelse(L<s & s< U ,1,0)) / length(s)
  return(CI_coverage)
}

# cv
cv = function(X,t){
  C =  1 - (sum(X==1)/sum(X))
  Si = sum(X>0)
  f = c()
  for(i in 1:max(X)){
    f[i] = sum(X==i)
  }
  I = sum(c(1:length(f))*(c(1:length(f))-1)*f)
  II = sum(c(1:length(f))*f)
  III = sum(c(1:length(f))*f - 1)
  a = (Si/C)*(t/(t-1))* (I/(II*III))
  cv = max(a-1,0)
  return(sqrt(cv))
}

# 整理
wor.O = function(X,Y,t1,t2,Z1,Z2){
  # set sample
  X = rowSums(Z1)
  Y = rowSums(Z2)
  X = X[1:S12]
  Y = Y[1:S12]
  
  # estimate
  o = obs(X,Y)
  
  result = o
  
  return(result)
}
wor = function(X,Y,T1,T2,t1,t2,Z1,Z2){
  # set sample
  X = sapply(1:times, function(j) Sample(T1,t1,Z1))
  Y = sapply(1:times, function(j) Sample(T2,t2,Z2))
  X = X[1:S12,]
  Y = Y[1:S12,]
  
  # sample coverage
  CX = mean(sapply(1:times, function(k) 1 - (sum(X[,k]==1)/sum(X[,k]))))
  CY = mean(sapply(1:times, function(k) 1 - (sum(Y[,k]==1)/sum(Y[,k]))))
  
  # sample cv
  CvX = mean(sapply(1:times, function(k) cv(X[,k],t1)))
  CvY = mean(sapply(1:times, function(k) cv(Y[,k],t2)))
  
  
  # estimate
  s = sapply(1:times, function(k) BB(X[,k],Y[,k],t1,t2))
  s_Pan = sapply(1:times, function(k) Pan(X[,k],Y[,k],t1,t2))
  o = sapply(1:times, function(k) obs(X[,k],Y[,k]))
  
  O = rbind(mean(o))
  E = mean(s);B = mean(s)-S12;SE = sd(s);RMSE = sqrt(sum((S12-s)^2)/times)
  E_Pan = mean(s_Pan);B_Pan = mean(s_Pan)-S12;SE_Pan = sd(s_Pan);RMSE_Pan = sqrt(sum((S12-s_Pan)^2)/times)
  
  
  d = .01
  
  # SD
  SD = mean(sapply(1:times, function(k) se(X[,k],Y[,k],t1,t2,O,E,d)))
  SD_Pan = mean(sapply(1:times, function(k) se_Pan(X[,k],Y[,k],t1,t2,O,E_Pan,d)))
  
  # CI
  CI = CI_coverage(E,O,SD,s)
  CI_Pan = CI_coverage(E_Pan,O,SD_Pan,s_Pan)
  
  I = cbind(O,E,B,SE,SD,RMSE,CI,CX,CY,CvX,CvY)
  II = cbind(O,E_Pan,B_Pan,SE_Pan,SD_Pan,RMSE_Pan,CI_Pan,CX,CY,CvX,CvY)
  result = rbind(I,II)
  return(result)
}
realdata = function(P_X,P_Y,t1,t2){
  share = which(rowSums(P_X)*rowSums(P_Y)>0)
  S12 = obs(rowSums(P_X[share,]),rowSums(P_Y[share,]))
  population1 = P_X[share,]
  population2 = P_Y[share,]
  
  T1 = ncol(P_X)
  T2 = ncol(P_Y)
  
  
  # set sample
  X = sapply(1:times, function(j) Sample(T1,t1,population1))
  Y = sapply(1:times, function(j) Sample(T2,t2,population2))
  
  # estimate
  s = sapply(1:times, function(k) BB(X[,k],Y[,k],t1,t2))
  s_Pan = sapply(1:times, function(k) Pan(X[,k],Y[,k],t1,t2))
  o = sapply(1:times, function(k) obs(X[,k],Y[,k]))
  
  O = rbind(mean(o))
  E = mean(s);B = mean(s)-S12;SE = sd(s);RMSE = sqrt(sum((S12-s)^2)/times)
  E_Pan = mean(s_Pan);B_Pan = mean(s_Pan)-S12;SE_Pan = sd(s_Pan);RMSE_Pan = sqrt(sum((S12-s_Pan)^2)/times)
  
  
  d = .01
  
  # SD
  SD = mean(sapply(1:times, function(k) se(X[,k],Y[,k],t1,t2,O,E,d)))
  SD_Pan = mean(sapply(1:times, function(k) se_Pan(X[,k],Y[,k],t1,t2,O,E_Pan,d)))
  
  # CI
  CI = CI_coverage(E,O,SD,s)
  CI_Pan = CI_coverage(E_Pan,O,SD_Pan,s_Pan)
  
  I = cbind(O,E,B,SE,SD,RMSE,CI)
  II = cbind(O,E_Pan,B_Pan,SE_Pan,SD_Pan,RMSE_Pan,CI_Pan)
  result = rbind(I,II)
  return(result)
}
realdata1 = function(P_X,P_Y){
  T1 = t1 = ncol(P_X)
  T2 = t2 = ncol(P_Y)
  
  # set sample
  X = rowSums(P_X)
  Y = rowSums(P_Y)
  
  # sample coverage
  CX = 1 - (sum(X==1)/sum(X))
  CY = 1 - (sum(Y==1)/sum(Y))
  
  # sample cv
  CvX = cv(X,t1)
  CvY = cv(Y,t2)
  
  # estimate
  E =  BB(X,Y,t1,t2)
  E_Pan = Pan(X,Y,t1,t2)
  O = obs(X,Y)
  
  d = .01
  
  # SD
  SD_ = se(X,Y,t1,t2,O,E,d)
  SD_Pan = se_Pan(X,Y,t1,t2,O,E_Pan,d)
  
  # log CI
  R = as.numeric(exp(1.96*( log(1+SD_^2/max(0.1^10,(E-O)^2)))^(1/2)))
  U = as.numeric(O+(E-O)*R)
  L = as.numeric(O+(E-O)/R)
  
  R1 = as.numeric(exp(1.96*( log(1+SD_Pan^2/max(0.1^10,(E_Pan-O)^2)))^(1/2)))
  U1 = as.numeric(O+(E_Pan-O)*R1)
  L1 = as.numeric(O+(E_Pan-O)/R1)
  
  I = cbind(O,E,SD_,L,U,CX,CY,CvX,CvY)
  II = cbind(O,E_Pan,SD_Pan,L1,U1,CX,CY,CvX,CvY)
  
  result = rbind(I,II)
  return(result)
}
################################## 參數 ########################################
times = 1000

S1 = 400
S2 = 400

# S1 = 600
# S2 = 600

# S1 = 400
# S2 = 600

S12 = 300
S = S1 + S2 - S12
S1.only = S1 - S12 ; S2.only = S2 - S12

T1 = 100
T2 = 100

################################ 物種分配 ######################################
r.s.12 = c(1:S12)
r.s.1 = c((S12+1):(S12+S1.only))
r.s.2 = c((S12+S1.only+1):S)

################################## 機率 ########################################
set.seed(123)
# homogeneous
set.seed(123)
Z1.1 = matrix(0,S1,T1)
x = matrix(0,S1,T1*0.1)
for(i in 1:S1){
  x[i,] = sample(100,T1*0.1)
}
for (i in 1:S1){
  Z1.1[i, x[i,]] = 1
}
set.seed(123)
Z1.2 = matrix(0,S2,T2)
x = matrix(0,S2,T2*0.1)
for(i in 1:S2){
  x[i,] = sample(100,T2*0.1)
}
for (i in 1:S2){
  Z1.2[i, x[i,]] = 1
}

rm(x)

# Random Uniform
set.seed(123)
p2.1 = runif(S1,0,.24)
p2.2 = runif(S2,0,.24)
mean(p2.1);sd(p2.1) / mean(p2.1)
mean(p2.2);sd(p2.2) / mean(p2.2)

# Broken-Stick
c = .9 # mean=0.073(1.9)/0.06(2.3)/0.051(2.7)/0.14(1)/0.11(1.2)/0.15(.9)
set.seed(123)
p3.1 <- rexp(S1)
p3.1 <- p3.1/(max(p3.1)*c)
for(i in 1:S1){
  p3.1[i] = ifelse(p3.1[i]>1, 1, p3.1[i])
}
p3.2 <- rexp(S2)
p3.2 <- p3.2/(max(p3.2)*c)
for(i in 1:S2){
  p3.2[i] = ifelse(p3.2[i]>1, 1, p3.2[i])
}

mean(p3.1);sd(p3.1) / mean(p3.1)
mean(p3.2);sd(p3.2) / mean(p3.2)

# log normal
c = .42 # mean=0.071(.9)/0.065(1)/0.05(1.3)/0.17(.35)/0.08(.8)/0.1(.6)/0.15(.42)
set.seed(123)
p4.1 <- rlnorm(S1)
p4.1 <- p4.1/(max(p4.1)*c)
for(i in 1:S1){
  p4.1[i] = ifelse(p4.1[i]>1, 1, p4.1[i])
}
p4.2 <- rlnorm(S2)
p4.2 <- p4.2/(max(p4.2)*c)
for(i in 1:S2){
  p4.2[i] = ifelse(p4.2[i]>1, 1, p4.2[i])
}

mean(p4.1);sd(p4.1) / mean(p4.1)
mean(p4.2);sd(p4.2) / mean(p4.2)

# beta
# c = 3 # mean=0.071(.9)/0.065(1)
# set.seed(123)
# p5.1 <- rbeta(S1,.5,4)
# p5.1 <- p5.1/(max(p5.1)*c)
# for(i in 1:S1){
#   p5.1[i] = ifelse(p5.1[i]>1, 1, p5.1[i])
# }
# p5.2 <- sample(p5.1,length(p5.1))
# 
# mean(p5.1);sd(p5.1) / mean(p5.1)
# mean(p5.2);sd(p5.2) / mean(p5.2)



# 特有種設定
Z1.1 = rbind(Z1.1, matrix(0,S1.only,T1))
Z1.2 = rbind(Z1.2[1:S12,], matrix(0,S2.only,T2),Z1.2[(S12+1):S2,])

p2.1 = c(p2.1,rep(0,S2.only))
p2.2 = c(p2.2[1:S12],rep(0,S1.only),p2.2[(S12+1):S2])

p3.1 = c(p3.1,rep(0,S2.only))
p3.2 = c(p3.2[1:S12],rep(0,S1.only),p3.2[(S12+1):S2])

p4.1 = c(p4.1,rep(0,S2.only))
p4.2 = c(p4.2[1:S12],rep(0,S1.only),p4.2[(S12+1):S2])

# p5.1 = c(p5.1,rep(0,S1.only))
# p5.2 = c(p5.2[1:S12],rep(0,S2.only),p4.2[(S12+1):S2])

############################### 機率排列 #######################################
# 出線機率高的作為共同種，特有種設定
# p2.1=sort(p2.1,T);p3.1=sort(p3.1,T);p4.1=sort(p4.1,T);
# p2.2=sort(p2.2,T);p3.2=sort(p3.2,T);p4.2=sort(p4.2,T);

############################### 設定母體 #######################################
# set population
Z1.1 = Z1.1;Z1.2 = Z1.2
Z2.1 = population(T1,p2.1,S);Z2.2 = population(T2,p2.2,S)
Z3.1 = population(T1,p3.1,S);Z3.2 = population(T2,p3.2,S)
Z4.1 = population(T1,p4.1,S);Z4.2 = population(T2,p4.2,S)
# Z5.1 = population(T1,p5.1,S);Z5.2 = population(T2,p5.2,S)

# 填補空缺
Z2.1 = fill(Z2.1,S12,T1);Z2.2 = fill(Z2.2,S12,T2)
Z3.1 = fill(Z3.1,S12,T1);Z3.2 = fill(Z3.2,S12,T2)
Z4.1 = fill(Z4.1,S12,T1);Z4.2 = fill(Z4.2,S12,T2)
# Z5.1 = fill(Z5.1,S12,T1);Z5.2 = fill(Z5.2,S12,T2)

# 母體參數
P = cbind(c(mean(rowSums(Z1.1)[1:S1]/T1),mean(rowSums(Z2.1)[1:S1]/T1),mean(rowSums(Z3.1)[1:S1]/T1),mean(rowSums(Z4.1)[1:S1]/T1)), # mean(rowSums(Z5.1)[1:S1]/T1)),
          c(sd(rowSums(Z1.1)[1:S1]/T1)/mean(rowSums(Z1.1)[1:S1]/T1),sd(rowSums(Z2.1)[1:S1]/T1)/mean(rowSums(Z2.1)[1:S1]/T1),
            sd(rowSums(Z3.1)[1:S1]/T1)/mean(rowSums(Z3.1)[1:S1]/T1),sd(rowSums(Z4.1)[1:S1]/T1)/mean(rowSums(Z4.1)[1:S1]/T1)))
# ,sd(rowSums(Z5.1)[1:S1]/T1)/mean(rowSums(Z5.1)[1:S1]/T1)))
P
cbind(c(mean(rowSums(Z1.2)[c(1:S12,(S1+1):S)]/T2),mean(rowSums(Z2.2)[c(1:S12,(S1+1):S)]/T2),mean(rowSums(Z3.2)[c(1:S12,(S1+1):S)]/T2),mean(rowSums(Z4.2)[c(1:S12,(S1+1):S)]/T2)),
      c(sd(rowSums(Z1.2)[c(1:S12,(S1+1):S)]/T2)/mean(rowSums(Z1.2)[c(1:S12,(S1+1):S)]/T2),sd(rowSums(Z2.2)[c(1:S12,(S1+1):S)]/T2)/mean(rowSums(Z2.2)[c(1:S12,(S1+1):S)]/T2),
        sd(rowSums(Z3.2)[c(1:S12,(S1+1):S)]/T2)/mean(rowSums(Z3.2)[c(1:S12,(S1+1):S)]/T2),sd(rowSums(Z4.2)[c(1:S12,(S1+1):S)]/T2)/mean(rowSums(Z4.2)[c(1:S12,(S1+1):S)]/T2)))

################################### test #######################################
# TT = T1 = T2
# t1 = t2 = TT
# a = Sys.time()
# set.seed(123)
# # set.seed(123)
# round(rbind(wor.O(X,Y,t1,t2,Z1.1,Z2.1),
#             wor.O(X,Y,t1,t2,Z2.1,Z2.2),
#             wor.O(X,Y,t1,t2,Z2.1,Z3.1),
#             wor.O(X,Y,t1,t2,Z3.1,Z4.2)),2)
# 
# t1 = 0.1*T1
# t2 = 0.1*T2
# round(wor(X,Y,T1,T2,t1,t2,Z1.1,Z2.1),2)
# round(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),2)
# round(wor(X,Y,T1,T2,t1,t2,Z1.1,Z4.1),2)
# Sys.time()-a
################################ 估計 ########################################
a = Sys.time()
set.seed(123)
t1 = 0.1*T1
t2 = 0.1*T2
list1 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.2*T1
t2 = 0.2*T2
list2 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.3*T1
t2 = 0.3*T2
list3 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.4*T1
t2 = 0.4*T2
list4 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.5*T1
t2 = 0.5*T2
list5 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)
t1 = 0.6*T1
t2 = 0.6*T2
list6 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)
t1 = 0.7*T1
t2 = 0.7*T2
list7 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.8*T1
t2 = 0.8*T2
list8 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

t1 = 0.9*T1
t2 = 0.9*T2
list9 = round(rbind(wor(X,Y,T1,T2,t1,t2,Z1.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
                    wor(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
                    wor(X,Y,T1,T2,t1,t2,Z3.1,Z4.1)),2)

Sys.time()-a;gc()
list = rbind(list1,list2,list3,list4,list5,list6,list7,list8,list9)
list
################################## 分類 ########################################
# 匯入表格
I = c(1:2);II = c(3:4);III = c(5:6);IV = c(7:8)
a = 8
list0 = rbind(list1,list3,list5,list7)
csv = rbind(list0[I,],list0[I+a,],list0[I+a*2,],list0[I+a*3,],
            list0[II,],list0[II+a,],list0[II+a*2,],list0[II+a*3,],
            list0[III,],list0[III+a,],list0[III+a*2,],list0[III+a*3,],
            list0[IV,],list0[IV+a,],list0[IV+a*2,],list0[IV+a*3,])
write.csv(csv[,c(1:7)], "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\table.csv")

# A = seq(from = 1, to = 31, 2)
# write.csv(csv[A,c(8:11)], "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\table.csv")

rm(I,II,III,IV,list0,A,csv);gc()
################################## plot ########################################
# 不同估計方法
A = seq(from=1,to=7,length.out=4)
B = A + 1
N = data.frame(rbind(list1[A,],list2[A,],list3[A,],list4[A,],list5[A,],list6[A,],list7[A,],list8[A,],list9[A,]))
N1 = data.frame(rbind(list1[B,],list2[B,],list3[B,],list4[B,],list5[B,],list6[B,],list7[B,],list8[B,],list9[B,]))

# 不同組合模型
I = seq(from=1,to=33,length.out=9)
II = I + 1
III = II + 1
IV = III + 1

# 跑圖的時候格子要拉到最大
# 圖說才會在正確的位置
# estimate I
par(mfrow=c(1,2),pty="s")
plot(N[I,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(120,310),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "I vs III")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[I,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[I,]$E,lwd=3,lty=2,col="blue")
points(N1[I,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(N[I,]$V1,lwd=3,lty=2,col="black")
points(N[I,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

# RMSE I
plot(N[I,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,100),ylab="RMSE",xlab="Sample size",xaxt="n",main = "I vs III")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[I,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[I,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[I,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),
       col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)


# estimate II
plot(N[II,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(120,310),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "II vs II")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[II,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[II,]$E,lwd=3,lty=2,col="blue")
points(N1[II,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(N[II,]$V1,lwd=3,lty=2,col="black")
points(N[II,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

# RMSE II
plot(N[II,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,100),ylab="RMSE",xlab="Sample size",xaxt="n",main = "II vs II")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[II,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[II,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[II,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),
       col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)


# estimate III
plot(N[III,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(120,310),ylab="Average Estimate",xlab="Sample size",xaxt="n",main ="II vs III")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[III,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[III,]$E,lwd=3,lty=2,col="blue")
points(N1[III,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(N[III,]$V1,lwd=3,lty=2,col="black")
points(N[III,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

# RMSE III
plot(N[III,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,100),ylab="RMSE",xlab="Sample size",xaxt="n",main ="II vs III")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[III,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[III,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[III,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),
       col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)


# estimate IV
plot(N[IV,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(120,310),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "III vs VI")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[IV,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[IV,]$E,lwd=3,lty=2,col="blue")
points(N1[IV,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(N[IV,]$V1,lwd=3,lty=2,col="black")
points(N[IV,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

# RMSE IV
plot(N[IV,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,110),ylab="RMSE",xlab="Sample size",xaxt="n",main = "III vs VI")
axis(1, c(1:9), labels=c('10','20','30','40','50','60','70','80','90'))
points(N[IV,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[IV,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[IV,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),
       col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

############################ real data #########################################
##### 火災後鳥類 #####
P_1 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\火災后鳥類\\data1.csv"))
P_2 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\火災后鳥類\\data2.csv"))
P_3 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\火災后鳥類\\data3.csv"))
P_1 = P_1[,-1]
P_2 = P_2[,-1]
P_3 = P_3[,-1]
common_rows1 = which(rowSums(P_1)*rowSums(P_2)>0)
common_rows2 = which(rowSums(P_1)*rowSums(P_3)>0)
common_rows3 = which(rowSums(P_2)*rowSums(P_3)>0)
O1 = obs(rowSums(P_1[common_rows1,]),rowSums(P_2[common_rows1,]))
O2 = obs(rowSums(P_1[common_rows2,]),rowSums(P_3[common_rows2,]))
O3 = obs(rowSums(P_2[common_rows3,]),rowSums(P_3[common_rows3,]))
p1 = rowSums(P_1)/ncol(P_1)
p2 = rowSums(P_2)/ncol(P_2)
p3 = rowSums(P_3)/ncol(P_3)
mean(p1);sd(p1)/mean(p1)
mean(p2);sd(p2)/mean(p2)
mean(p3);sd(p3)/mean(p3)

set.seed(123)
P_X = P_1;P_Y = P_2
r1 = data.frame(round(rbind(realdata(P_X,P_Y,20,20),realdata(P_X,P_Y,40,40),realdata(P_X,P_Y,60,60),realdata(P_X,P_Y,80,80),
                            realdata(P_X,P_Y,100,100),realdata(P_X,P_Y,120,120),realdata(P_X,P_Y,140,140),realdata(P_X,P_Y,160,160)),2));r1
r.1 = data.frame(rbind(r1[c(1,2),],r1[c(5,6),],r1[c(9,10),],r1[c(13,14),],r1[c(17,18),]))

P_X = P_1;P_Y = P_3
r2 = data.frame(round(rbind(realdata(P_X,P_Y,20,20),realdata(P_X,P_Y,40,40),realdata(P_X,P_Y,60,60),realdata(P_X,P_Y,80,80),
                            realdata(P_X,P_Y,100,100),realdata(P_X,P_Y,120,120),realdata(P_X,P_Y,140,140),realdata(P_X,P_Y,160,160)),2));r2
r.2 = data.frame(rbind(r2[c(1,2),],r2[c(5,6),],r2[c(9,10),],r2[c(13,14),],r2[c(17,18),]))

P_X = P_2;P_Y = P_3
r3 = data.frame(round(rbind(realdata(P_X,P_Y,20,20),realdata(P_X,P_Y,40,40),realdata(P_X,P_Y,60,60),realdata(P_X,P_Y,80,80),
                            realdata(P_X,P_Y,100,100),realdata(P_X,P_Y,120,120),realdata(P_X,P_Y,140,140),realdata(P_X,P_Y,160,160)),2));r3
r.3 = data.frame(rbind(r3[c(1,2),],r3[c(5,6),],r3[c(9,10),],r3[c(13,14),],r3[c(17,18),]))

r. = rbind(r.1,r.2,r.3)
write.csv(r., "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\real.csv")

##  real plot ##
# 不同模型
A = seq(from=1,to=15,length.out=8)
B = A + 1

par(mfrow=c(1,3),pty="s")
r=r1; O=O1
plot(r[A,]$E,type="l",lwd=3,lty=2,col="red2", ylim = c(20,60),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "I vs II")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$E,lwd=3,lty=2,col="blue")
points(r[B,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[A,]$V1,lwd=3,lty=2,col="black")
points(r[A,]$V1,lwd=3,pch=4,col="black")
abline(h= O,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

plot(r[A,]$RMSE,type="l",lwd=3,lty=2,col="red2", ylim = c(0,20),ylab="RMSE",xlab="Sample size",xaxt="n",main = "I vs II")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$RMSE,lwd=3,lty=2,col="blue")
points(r[B,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

plot(r[A,]$CI,type="l",lwd=3,lty=2,col="red2",ylab="95 % CI Coverage",xlab="Sample size",ylim = c(0.6,.9),xaxt="n",main = "I vs II")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$CI,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$CI,lwd=3,lty=2,col="blue")
points(r[B,]$CI,lwd=5,pch=17,cex=1.2,col="blue")
legend("bottomright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

r=r2; O=O2
plot(r[A,]$E,type="l",lwd=3,lty=2,col="red2", ylim = c(20,55),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "I vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$E,lwd=3,lty=2,col="blue")
points(r[B,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[A,]$V1,lwd=3,lty=2,col="black")
points(r[A,]$V1,lwd=3,pch=4,col="black")
abline(h= O,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

plot(r[A,]$RMSE,type="l",lwd=3,lty=2,col="red2", ylim = c(0,20),ylab="RMSE",xlab="Sample size",xaxt="n",main = "I vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$RMSE,lwd=3,lty=2,col="blue")
points(r[B,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

plot(r[A,]$CI,type="l",lwd=3,lty=2,col="red2",ylab="95 % CI Coverage",xlab="Sample size",ylim = c(0.6,.9),xaxt="n",main = "I vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$CI,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$CI,lwd=3,lty=2,col="blue")
points(r[B,]$CI,lwd=5,pch=17,cex=1.2,col="blue")
legend("bottomright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

r=r3; O=O3
plot(r[A,]$E,type="l",lwd=3,lty=2,col="red2", ylim = c(20,60),ylab="Average Estimate",xlab="Sample size",xaxt="n",main = "II vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$E,lwd=3,lty=2,col="blue")
points(r[B,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[A,]$V1,lwd=3,lty=2,col="black")
points(r[A,]$V1,lwd=3,pch=4,col="black")
abline(h= O,lwd=3,col="gray")
legend("bottomright",legend  = c("BB","Pan","Obs"),
       col= c('red','blue','black'),lty = c(2,2,2),
       pch=c(16,17,4),merge = TRUE)

plot(r[A,]$RMSE,type="l",lwd=3,lty=2,col="red2", ylim = c(0,20),ylab="RMSE",xlab="Sample size",xaxt="n",main = "II vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$RMSE,lwd=3,lty=2,col="blue")
points(r[B,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
legend("topright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

plot(r[A,]$CI,type="l",lwd=3,lty=2,col="red2",ylab="95 % CI Coverage",xlab="Sample size",ylim = c(0.6,.9),xaxt="n",main = "II vs III")
axis(1, c(1:8), labels=c("20", "40", "60", "80", "100",'120','140','160'))
points(r[A,]$CI,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$CI,lwd=3,lty=2,col="blue")
points(r[B,]$CI,lwd=5,pch=17,cex=1.2,col="blue")
legend("bottomright",legend  = c("BB","Pan"),col= c('red','blue'),lty = c(2,2),
       pch=c(16,17),merge = TRUE)

##### Sequoia National Park #####
P_1 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\微生物 內華達\\data1.csv"))
P_2 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\微生物 內華達\\data2.csv"))
P_3 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\微生物 內華達\\data3.csv"))
P_4 = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\微生物 內華達\\data4.csv"))
P_1 = P_1[,-1];P_2 = P_2[,-1];P_3 = P_3[,-1];P_4 = P_4[,-1]
p1 = rowSums(P_1)/ncol(P_1);p2 = rowSums(P_2)/ncol(P_2)
p3 = rowSums(P_3)/ncol(P_3);p4 = rowSums(P_4)/ncol(P_4)
mean(p1);mean(p2);mean(p3);mean(p4)
sd(p1)/mean(p1);sd(p2)/mean(p2);sd(p3)/mean(p3);sd(p4)/mean(p4)

# T
T1 = ncol(P_1);T2 = ncol(P_2);T3 = ncol(P_3);T4 = ncol(P_4)

# Q_i
f(rowSums(P_1));f(rowSums(P_2));f(rowSums(P_3));f(rowSums(P_4))

## non estimate
# richness
S1_O = length(which(rowSums(P_1)>0))
S2_O = length(which(rowSums(P_2)>0))
S3_O = length(which(rowSums(P_3)>0))
S4_O = length(which(rowSums(P_4)>0))

# share
S12_O = length(which(rowSums(P_1)*rowSums(P_2)>0))
S13_O = length(which(rowSums(P_1)*rowSums(P_3)>0))
S14_O = length(which(rowSums(P_1)*rowSums(P_4)>0))
S23_O = length(which(rowSums(P_2)*rowSums(P_3)>0))
S24_O = length(which(rowSums(P_2)*rowSums(P_4)>0))
S34_O = length(which(rowSums(P_3)*rowSums(P_4)>0))


## estimate
set.seed(123)
r = data.frame(round(rbind(realdata1(P_1,P_2),realdata1(P_1,P_3),realdata1(P_1,P_4),
                           realdata1(P_2,P_3),realdata1(P_2,P_4),realdata1(P_3,P_4)),2));r
write.csv(r, "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\real.csv")
S12 = r$E[1];S13 = r$E[3];S14 = r$E[5];S23 = r$E[7];S24 = r$E[9];S34 = r$E[11]


## one community richness
# estimate
S1 = more(rowSums(P_1),T1)
S2 = more(rowSums(P_2),T2)
S3 = more(rowSums(P_3),T3)
S4 = more(rowSums(P_4),T4)
S = round(c(S1,S2,S3,S4),1);S

# SD
a = .5
Sd = round(c(se_m(rowSums(P_1),T1,S1_O,S1,.01),
             se_m(rowSums(P_2),T2,S2_O,S2,.01),
             se_m(rowSums(P_3),T3,S3_O,S3,.01),
             se_m(rowSums(P_4),T4,S4_O,S4,.01)),2)
cbind(S,Sd)



## species richness of two community
# obs
S.1_O = S1_O+S2_O-S12_O
S.2_O = S1_O+S3_O-S13_O
S.3_O = S1_O+S4_O-S14_O
S.4_O = S2_O+S3_O-S23_O
S.5_O = S2_O+S4_O-S24_O
S.6_O = S3_O+S4_O-S34_O

# estimate 1
S.1 = S1+S2-S12
S.2 = S1+S3-S13
S.3 = S1+S4-S14
S.4 = S2+S3-S23
S.5 = S2+S4-S24
S.6 = S3+S4-S34

# estimate 2
S..1 = more((rowSums(P_1)+rowSums(P_2)),T1+T2)
S..2 = more((rowSums(P_1)+rowSums(P_3)),T1+T3)
S..3 = more((rowSums(P_1)+rowSums(P_4)),T1+T4)
S..4 = more((rowSums(P_2)+rowSums(P_3)),T2+T3)
S..5 = more((rowSums(P_2)+rowSums(P_4)),T2+T4)
S..6 = more((rowSums(P_3)+rowSums(P_4)),T2+T4)

round(c(S.1,S.2,S.3,S.4,S.5,S.6),1)
round(c(S..1,S..2,S..3,S..4,S..5,S..6),1)
round(c(S.1_O,S.2_O,S.3_O,S.4_O,S.5_O,S.6_O),1)

## beta diversity
# obs
B1_O = 1 - (S12_O/S.1_O)
B2_O = 1 - (S13_O/S.2_O)
B3_O = 1 - (S14_O/S.3_O)
B4_O = 1 - (S23_O/S.4_O)
B5_O = 1 - (S24_O/S.5_O)
B6_O = 1 - (S34_O/S.6_O)
B_O = c(B1_O,B2_O,B3_O,B4_O,B5_O,B6_O)

# estimate 1
B1 = 1 - (S12/S.1)
B2 = 1 - (S13/S.2)
B3 = 1 - (S14/S.3)
B4 = 1 - (S23/S.4)
B5 = 1 - (S24/S.5)
B6 = 1 - (S34/S.6)
B = c(B1,B2,B3,B4,B5,B6)

# estimate 2
B.1 = 1 - (S12/S..1)
B.2 = 1 - (S13/S..2)
B.3 = 1 - (S14/S..3)
B.4 = 1 - (S23/S..4)
B.5 = 1 - (S24/S..5)
B.6 = 1 - (S34/S..6)
B. = c(B.1,B.2,B.3,B.4,B.5,B.6)

round(B,2)
round(B.,2)
round(B_O,2)

## beta diversity plot
# non estimate
j_O = matrix(c(NA,B_O[1:3],
               B_O[1],NA,B_O[4:5],
               B_O[2],B_O[4],NA,B_O[6],
               B_O[3],B_O[5],B_O[6],NA),4)
row.names(j_O) = colnames(j_O) = c('foothill','lower conifer','upper conifer','high country')
jaccard_dist_O = as.dist(j_O);jaccard_dist_O

# Hierarchical clustering based on Jaccard distance
hc_O = hclust(jaccard_dist_O, method = "average")

# estumate
j = matrix(c(NA,B[1:3],
             B[1],NA,B[4:5],
             B[2],B[4],NA,B[6],
             B[3],B[5],B[6],NA),4)
row.names(j) = colnames(j) = c('foothill','lower conifer','upper conifer','high country')
jaccard_dist = as.dist(j);jaccard_dist

# Hierarchical clustering based on Jaccard distance
hc = hclust(jaccard_dist, method = "average")

# Plot the dendrogram
par(mfrow=c(1,2))
plot(hc_O, xlab = "Samples", ylab = "Distance", main = "non-Estimate")
plot(hc, xlab = "Samples", ylab = "Distance", main = "Estimate")

