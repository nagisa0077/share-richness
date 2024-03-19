################################# function #####################################
f.fun = function(X,Y){
  f = c()
  
  # X = X[I] #X的共有物種
  # Y = Y[I] #Y的共有物種
  #f1.
  f[1] = sum(X==1 & Y!=0)
  #f.1
  f[2] = sum(X!=0 & Y==1)
  #f7
  f[3] = sum(X==1 & Y==1)
  #f2.
  f[4] = sum(X==2 & Y!=0)
  #f.2
  f[5] = sum(X!=0 & Y==2)
  #f22
  f[6] = sum(X==2 & Y==2)
  #f12
  f[7] = sum(X==1 & Y==2)
  #f21
  f[8] = sum(X==2 & Y==1)
  #D
  f[9] = sum(X==0 | Y==0)
  
  return(f)
}
f.fun2 = function(X,Y){
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
  return(f)
}
obs = function(X,Y){
  I = which(X*Y>0)
  s.obs = length(I)
  return(s.obs)
}
population = function(TT,theta,S){
  set.seed(0000)
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
wor_share_2012 = function(X,Y,T1,T2,t1,t2){
  f = f.fun(X,Y)
  s.obs = obs(X,Y)
  
  q1 = t1/T1;q2 = t2/T2
  w1 = t1/(t1-1);w2 = t2/(t2-1)
  r1 = q1/(1-q1);r2 = q2/(1-q2)
  
  Q0_ = f[1]^2 / max(1,(2*w1*f[4] + r1*f[1]))
  Q_0 = f[2]^2 / max(1,(2*w2*f[5] + r2*f[2]))
  Q00 = f[3]^2 / max(1,(4*w1*w2*f[6] + 2*w1*r2*f[7] + 2*w2*r1*f[8] + r1*r2*f[3]))
  
  ans = s.obs + Q00 + Q0_ + Q_0
  
  return(ans)
}
wor_share = function(X,Y,T1,T2,t1,t2){
  f = f.fun(X,Y)
  s.obs = obs(X,Y)
  w1 = t1/T1
  w2 = t2/T2
  beta1 =(f[1]/max(1,f[4])-1)*t1
  beta2 =(f[2]/max(1,f[5])-1)*t2
  # beta1 =((f[1]-f[4])/max(1,f[4]))*t1
  # beta2 =((f[2]-f[5])/max(1,f[5]))*t2
  # beta1 =max(1,(f[1]/max(1,f[4])-1)*t1)
  # beta2 =max(1,(f[2]/max(1,f[5])-1)*t2)
  
  #Q00
  Q00 = ((t1-1)/t1) * ((t2-1)/t2)* (((beta1/t1)+1)*((T1-t1)/(T1+beta1))) * (((beta2/t2)+1)*((T2-t2)/(T2+beta2))) * f[3]
  # Q00 = ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f[3]
  
  #Q0+
  Q0_ = ((t1-1)/t1)*(((beta1/t1)+1)*((T1-t1)/(T1+beta1)))* f[1]
  
  # Q0_ = ((t1-1)/t1) * ((1-w1)*f[1]^2)/max(1,(f[4] + (w1*max(1,f[1]-f[4]))))
  
  #Q+0
  Q_0 = ((t2-1)/t2)* (((beta2/t2)+1)*((T2-t2)/(T2+beta2))) * f[2]
  
  # Q_0 = ((t2-1)/t2) * ((1-w2)*f[2]^2)/max(1,(f[5] + (w2*max(1,f[2]-f[5]))))
  
  # S
  ans = s.obs + Q00 + Q0_ + Q_0
  return(ans)
}
wor_share1 = function(X,Y,T1,T2,t1,t2){
  f = f.fun(X,Y)
  s.obs = obs(X,Y)
  w1 = t1/T1
  w2 = t2/T2
  # beta1 = t1*(sqrt(max(1,f[3]*f[7])/max(1,f[6]*f[8]))-1)
  # beta2 = t2*(sqrt(max(1,f[3]*f[8])/max(1,f[6]*f[7]))-1)
  # beta1 = max(1,t1*(sqrt(max(1,f[3]*f[7])/max(1,f[6]*f[8]))-1))
  # beta2 = max(1,t2*(sqrt(max(1,f[3]*f[8])/max(1,f[6]*f[7]))-1))
  beta1 = t1*(sqrt(max(1,f[3])*max(1,f[7])/max(1,f[6]*f[8]))-1)
  beta2 = t2*(sqrt(max(1,f[3])*max(1,f[8])/max(1,f[6]*f[7]))-1)
  
  #Q00
  Q00 = ((t1-1)/t1)* ((t2-1)/t2) * ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f[3]
  # Q00 = ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f[3]
  
  #Q0+
  Q0_ = ((t1-1)/t1)*(((beta1/t1)+1)*((T1-t1)/(T1+beta1)))* f[1]
  
  # Q0_ = ((t1-1)/t1) * ((1-w1)*f[1]^2)/max(1,(f[4] + (w1*max(1,f[1]-f[4]))))
  
  #Q+0
  Q_0 = ((t2-1)/t2)* (((beta2/t2)+1)*((T2-t2)/(T2+beta2))) * f[2]
  
  # Q_0 = ((t2-1)/t2) * ((1-w2)*f[2]^2)/max(1,(f[5] + (w2*max(1,f[2]-f[5]))))
  
  # S
  ans = s.obs + Q00 + Q0_ + Q_0
  return(ans)
}
BB = function(X,Y,T1,T2,t1,t2){
  f = f.fun2(X,Y)
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
more = function(X,TT,p){
  f = f(X)
  s.obs = sum(X>0)
  t = ceiling(p*T1)
  w = t/TT
  a = max((f[1]-f[2]),1)
  
  if(f[2]==0){
    Q0 = (t-1)/t * (1-w)/w * (f[1]-1)
  }else{
    Q0 = (t-1)/t * (1-w)*f[1]^2/(f[2] + w* a)
  }
  
  # S
  ans = s.obs + Q0
  return(ans)
}

# var
var = function(s.obs,f1,f2,f3,f4,f5,t1,t2,T1,T2){
  w1 = t1/T1
  w2 = t2/T2
  beta1 = max(0,((f1/max(1,f4))-1)*t1)
  beta2 = max(0,((f2/max(1,f4))-1)*t2)
  
  #Q00
  Q00 = ((t1-1)/t1)* ((t2-1)/t2)* ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f3
  
  #Q0+
  Q0_ = ((t1-1)/t1)*  ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * f1
  
  #Q+0
  Q_0 = ((t2-1)/t2)* ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f2
  
  ans = s.obs + Q00 + Q0_ + Q_0
  return(ans)
}
var1 = function(s.obs,f1,f2,f3,f4,f5,f6,f7,f8,t1,t2,T1,T2){
  w1 = t1/T1
  w2 = t2/T2
  beta1 = max(0, -t1 + t1*sqrt((f3*f7)/max(1,(f6*f8))))
  beta2 = max(0, -t2 + t2*sqrt((f3*f8)/max(1,(f6*f7))))
  
  #Q00
  Q00 = ((t1-1)/t1)* ((t2-1)/t2)* ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f3
  
  #Q0+
  Q0_ = ((t1-1)/t1)*  ((beta1/t1 +1)*((T1-t1)/(T1+beta1))) * f1
  
  #Q+0
  Q_0 = ((t2-1)/t2)* ((beta2/t2 +1)*((T2-t2)/(T2+beta2))) * f2
  
  ans = s.obs + Q00 + Q0_ + Q_0
  return(ans)
}
var12 = function(s.obs,f1,f2,f3,f4,f5,f6,f7,f8,t1,t2,T1,T2){
  q1 = t1/T1;q2 = t2/T2
  w1 = t1/(t1-1);w2 = t2/(t2-1)
  r1 = q1/(1-q1);r2 = q2/(1-q2)
  
  Q0_ = f1^2 / max(1,(2*w1*f4 + r1*f1))
  Q_0 = f2^2 / max(1,(2*w2*f5 + r2*f2))
  Q00 = f3^2 / max(1,(4*w1*w2*f6 + 2*w1*r2*f7 + 2*w2*r1*f8 + r1*r2*f3))
  
  ans = s.obs + Q00 + Q0_ + Q_0
  
  return(ans)
}
var3 = function(s.obs,f1,f2,f3,f4,f5,f6,f7,f8,t1,t2,T1,T2){
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
var_m = function(s.obs,f1,f2,t,TT){
  w = t/TT
  a = max((f1-f2),1)
  
  if(f2==0){
    Q0 = (t-1)/t * (1-w)/w * (f2-1)
  }else{
    Q0 = (t-1)/t * (1-w)*f2^2/(f2 + w * a)
  }
  
  # S
  ans = s.obs + Q0
  return(ans)
}

# Covmatrix
covmatrix = function(X,Y,s,O){
  f = c(f.fun(X,Y)) #用Sobs去建立covarance matrix
  f = c(f[1:5],s-O)
  cov_matrix = matrix(0, nrow = 6, ncol = 6)
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
covmatrix12 = function(X,Y,s,O){
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
covmatrix3 = function(X,Y,s,O){
  f = c(f.fun2(X,Y)) #用Sobs去建立covarance matrix
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
covmatrix_m = function(X,s,O){
  f = c(f(X)) #用Sobs去建立covarance matrix
  f = c(f[1:2],s-O)
  cov_matrix = matrix(0, nrow = 3, ncol = 3)
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
SD  = function(X,Y,t1,t2,T1,T2,O,d){
  f = f.fun(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5]
  V = c((var(O,f[1]+d,f[2],f[3],f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
        (var(O,f[1],f[2]+d,f[3],f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
        (var(O,f[1],f[2],f[3]+d,f[4],f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
        (var(O,f[1],f[2],f[3],f[4]+d,f[5],t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,
        (var(O,f[1],f[2],f[3],f[4],f[5]+d,t1,t2,T1,T2)-var(O,f[1],f[2],f[3],f[4],f[5],t1,t2,T1,T2))/d,-1)
  return(V)
}
SD1  = function(X,Y,t1,t2,T1,T2,O,d){
  f = f.fun(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5];f6=f[6];f7=f[8];f7=f[8]
  V = c((var1(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,f[7],f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7]+d,f[8],t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8]+d,t1,t2,T1,T2)-var1(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,-1)
  return(V)
}
SD12  = function(X,Y,t1,t2,T1,T2,O,d){
  f = f.fun(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5];f6=f[6];f7=f[7];f8=f[8]
  V = c((var12(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,f[7],f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7]+d,f[8],t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8]+d,t1,t2,T1,T2)-var12(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,-1)
  return(V)
}
SD3  = function(X,Y,t1,t2,T1,T2,O,d){
  f = f.fun2(X,Y)
  # f1=f[1];f2=f[2];f3=f[3];f4=f[4];f5=f[5]
  V = c((var3(O,f[1]+d,f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2]+d,f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3]+d,f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3],f[4]+d,f[5],f[6],f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3],f[4],f[5]+d,f[6],f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3],f[4],f[5],f[6]+d,f[7],f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7]+d,f[8],t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,
        (var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8]+d,t1,t2,T1,T2)-var3(O,f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],t1,t2,T1,T2))/d,-1)
  return(V)
}
SD_m  = function(X,t,TT,O,d){
  f = f(X)
  V = c((var_m(O,f[1]+d,f[2],t,TT)-var_m(O,f[1],f[2],t,TT))/d,
        (var_m(O,f[1],f[2]+d,t,TT)-var_m(O,f[1],f[2],t,TT))/d,-1)
  return(V)
}

# SE
se = function(X,Y,t1,t2,T1,T2,O,E,d){
  sd = SD(X,Y,t1,t2,T1,T2,O,d)
  matrix = covmatrix(X,Y,E,O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}
se1 = function(X,Y,t1,t2,T1,T2,O,E1,d){
  sd1 = SD1(X,Y,t1,t2,T1,T2,O,d)
  matrix1 = covmatrix12(X,Y,E1,O)
  SD1 = sqrt(t(sd1) %*% matrix1 %*% sd1)
  return(SD1)
}
se12 = function(X,Y,t1,t2,T1,T2,O,E12,d){
  sd12 = SD12(X,Y,t1,t2,T1,T2,O,d)
  matrix12 = covmatrix12(X,Y,E12,O)
  SD12 = sqrt(t(sd12) %*% matrix12 %*% sd12)
  return(SD12)
}
se3 = function(X,Y,t1,t2,T1,T2,O,E3,d){
  sd = SD3(X,Y,t1,t2,T1,T2,O,d)
  matrix = covmatrix3(X,Y,E3,O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}
se_m = function(X,t,TT,O,E,d){
  sd = SD_m(X,t,TT,O,d)
  matrix = covmatrix_m(X,E,O)
  SD = sqrt(abs(t(sd) %*% matrix %*% sd))
  return(SD)
}

# 95 % CI
CI_coverage1 = function(E,O,SD_,s){
  U = as.numeric(E + (1.96*SD_))
  L = as.numeric(E - (1.96*SD_))
  CI_coverage = sum(ifelse((L<s)&(s<U), 1, 0)) / length(s)
  return(CI_coverage)
}
CI_coverage2 = function(E,O,SD_,s){
  R = as.numeric(exp(1.96*( log(1+SD_^2/max(0.1^10,(E-O)^2)))^(1/2)))
  U = as.numeric(O+(E-O)*R)
  L = as.numeric(O+(E-O)/R)
  CI_coverage = sum(ifelse((L<s)&(s<U), 1, 0)) / length(s)
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
wor.O = function(X,Y,T1,T2,t1,t2,Z1,Z2){
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
  CX = sapply(1:times, function(k) 1 - (sum(X[,k]==1)/sum(X[,k])))
  CY = sapply(1:times, function(k) 1 - (sum(Y[,k]==1)/sum(Y[,k])))
  
  # sample cv
  CvX = sapply(1:times, function(k) cv(X[,k],t1))
  CvY = sapply(1:times, function(k) cv(Y[,k],t2))

  # estimate
  s = sapply(1:times, function(k) wor_share(X[,k],Y[,k],T1,T2,t1,t2))
  s1 = sapply(1:times, function(k) wor_share1(X[,k],Y[,k],T1,T2,t1,t2))
  s12 = sapply(1:times, function(k) wor_share_2012(X[,k],Y[,k],T1,T2,t1,t2))
  sBB = sapply(1:times, function(k) BB(X[,k],Y[,k],T1,T2,t1,t2))
  o = sapply(1:times, function(k) obs(X[,k],Y[,k]))
  
  O = rbind(mean(o))
  E = mean(s);B = mean(s)-S12;SE = sd(s);RMSE = sqrt(sum((S12-s)^2)/times)
  E1 = mean(s1);B1 = mean(s1)-S12;SE1 = sd(s1);RMSE1 = sqrt(sum((S12-s1)^2)/times)
  E12 = mean(s12);B12 = mean(s12)-S12;SE12 = sd(s12);RMSE12 = sqrt(sum((S12-s12)^2)/times)
  EBB = mean(sBB);BBB = mean(sBB)-S12;SEBB = sd(sBB);RMSEBB = sqrt(sum((S12-sBB)^2)/times)
  
  d = .01
  
  # SD
  SD_ = mean(sapply(1:times, function(k) se(X[,k],Y[,k],t1,t2,T1,T2,O,E,d)))
  SD1_ = mean(sapply(1:times, function(k) se1(X[,k],Y[,k],t1,t2,T1,T2,O,E1,d)))
  SD12_ = mean(sapply(1:times, function(k) se12(X[,k],Y[,k],t1,t2,T1,T2,O,E12,d)))
  SD3_ = mean(sapply(1:times, function(k) se3(X[,k],Y[,k],t1,t2,T1,T2,O,E12,d)))
  
  # CI
  CI = CI_coverage1(E,O,SD_,s)
  CI1 = CI_coverage1(E1,O,SD1_,s1)
  CI12 = CI_coverage2(E12,O,SD12_,s12)
  CI3 = CI_coverage2(EBB,O,SD3_,sBB)
  
  I = cbind(O,E,B,SE,SD_,RMSE,CI,mean(CX),mean(CY),mean(CvX),mean(CvY))
  II = cbind(O,E1,B1,SE1,SD1_,RMSE1,CI1,mean(CX),mean(CY),mean(CvX),mean(CvY))
  IV = cbind(O,E12,B12,SE12,SD12_,RMSE12,CI12,mean(CX),mean(CY),mean(CvX),mean(CvY))
  V = cbind(O,EBB,BBB,SEBB,SD3_,RMSEBB,CI3,mean(CX),mean(CY),mean(CvX),mean(CvY))
  
  # I = cbind(O,E,B,SE,SD_,RMSE,CI)
  # II = cbind(O,E1,B1,SE1,SD1_,RMSE1,CI1)
  # IV = cbind(O,E12,B12,SE12,SD12_,RMSE12,CI12)
  # V = cbind(O,EBB,BBB,SEBB,SD3_,RMSEBB,CI3)
  
  result = rbind(I,II,IV,V)
  return(result)
}
realdata = function(P_X,P_Y,a){
  share = which(rowSums(P_X)*rowSums(P_Y)>0)
  S12 = obs(rowSums(P_X[share,]),rowSums(P_Y[share,]))
  population1 = P_X[share,]
  population2 = P_Y[share,]
  
  T1 = ncol(P_X)
  T2 = ncol(P_Y)
  t1 = ceiling(a*T1)
  t2 = ceiling(a*T2)
  
  # set sample
  X = sapply(1:times, function(j) Sample(T1,t1,population1))
  Y = sapply(1:times, function(j) Sample(T2,t2,population2))
  
  # estimate
  s = sapply(1:times, function(k) wor_share(X[,k],Y[,k],T1,T2,t1,t2))
  s1 = sapply(1:times, function(k) wor_share1(X[,k],Y[,k],T1,T2,t1,t2))
  s12 = sapply(1:times, function(k) wor_share_2012(X[,k],Y[,k],T1,T2,t1,t2))
  sBB = sapply(1:times, function(k) BB(X[,k],Y[,k],T1,T2,t1,t2))
  o = sapply(1:times, function(k) obs(X[,k],Y[,k]))
  
  O = rbind(mean(o))
  E = mean(s);B = mean(s)-S12;SE = sd(s);RMSE = sqrt(sum((S12-s)^2)/times)
  E1 = mean(s1);B1 = mean(s1)-S12;SE1 = sd(s1);RMSE1 = sqrt(sum((S12-s1)^2)/times)
  E12 = mean(s12);B12 = mean(s12)-S12;SE12 = sd(s12);RMSE12 = sqrt(sum((S12-s12)^2)/times)
  EBB = mean(sBB);BBB = mean(sBB)-S12;SEBB = sd(sBB);RMSEBB = sqrt(sum((S12-sBB)^2)/times)
  
  d = .01
  
  # SD
  SD_ = mean(sapply(1:times, function(k) se(X[,k],Y[,k],t1,t2,T1,T2,O,E,d)))
  SD1_ = mean(sapply(1:times, function(k) se1(X[,k],Y[,k],t1,t2,T1,T2,O,E1,d)))
  SD12_ = mean(sapply(1:times, function(k) se12(X[,k],Y[,k],t1,t2,T1,T2,O,E12,d)))
  SD3_ = mean(sapply(1:times, function(k) se3(X[,k],Y[,k],t1,t2,T1,T2,O,E12,d)))
  
  # CI
  CI = CI_coverage1(E,O,SD_,s)
  CI1 = CI_coverage1(E1,O,SD1_,s1)
  CI12 = CI_coverage2(E12,O,SD12_,s12)
  CI3 = CI_coverage2(EBB,O,SD3_,sBB)
  
  I = cbind(O,E,B,SE,SD_,RMSE,CI)
  II = cbind(O,E1,B1,SE1,SD1_,RMSE1,CI1)
  IV = cbind(O,E12,B12,SE12,SD12_,RMSE12,CI12)
  V = cbind(O,EBB,BBB,SEBB,SD3_,RMSEBB,CI3)
  result = rbind(I,II,IV,V)
  return(result)
}
realdata1 = function(P_X,P_Y,a){
  # T and t about sample fraction
  t1 = ncol(P_X)
  t2 = ncol(P_Y)
  T1 = ceiling(t1/a)
  T2 = ceiling(t2/a)
  
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
  E = wor_share(X,Y,T1,T2,t1,t2)
  E1 = wor_share1(X,Y,T1,T2,t1,t2)
  E12 = wor_share_2012(X,Y,T1,T2,t1,t2)
  BB = BB(X,Y,T1,T2,t1,t2)
  O = obs(X,Y)

  
  d = .01
  
  # SD
  SD_ = se(X,Y,t1,t2,T1,T2,O,E,d)
  SD1_ = se1(X,Y,t1,t2,T1,T2,O,E1,d)
  SD12_ = se12(X,Y,t1,t2,T1,T2,O,E12,d)
  SDBB_ = se3(X,Y,t1,t2,T1,T2,O,E12,d)
  
  # log CI
  U = as.numeric(E + (1.96*SD_))
  L = as.numeric(E - (1.96*SD_))
  
  U1 = as.numeric(E1 + (1.96*SD1_))
  L1 = as.numeric(E1 - (1.96*SD1_))
  
  R12 = as.numeric(exp(1.96*( log(1+SD12_^2/max(0.1^10,(E12-O)^2)))^(1/2)))
  U12 = as.numeric(O+(E12-O)*R12)
  L12 = as.numeric(O+(E12-O)/R12)
  
  RBB = as.numeric(exp(1.96*( log(1+SDBB_^2/max(0.1^10,(BB-O)^2)))^(1/2)))
  UBB = as.numeric(O+(BB-O)*RBB)
  LBB = as.numeric(O+(BB-O)/RBB)
  
  
  I = cbind(O,CX,CY,CvX,CvY,E,SD_,L,U)
  II = cbind(O,CX,CY,CvX,CvY,E1,SD1_,L1,U1)
  IV = cbind(O,CX,CY,CvX,CvY,E12,SD12_,L12,U12)
  V = cbind(O,CX,CY,CvX,CvY,BB,SDBB_,LBB,UBB)
  
  result = rbind(I,II,IV,V)
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

# ################################### test #######################################
# TT = T1 = T2
# t1 = t2 = TT
a = Sys.time()
set.seed(123)
# set.seed(123)
round(rbind(wor.O(X,Y,T1,T2,t1,t2,Z1.1,Z2.1),
            wor.O(X,Y,T1,T2,t1,t2,Z2.1,Z2.2),
            wor.O(X,Y,T1,T2,t1,t2,Z2.1,Z3.1),
            wor.O(X,Y,T1,T2,t1,t2,Z3.1,Z4.2)),2)
Sys.time()-a
################################## 估計 ########################################
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
I = c(1:4);II = c(5:8);III = c(9:12);IV = c(13:16)
list0 = rbind(list1,list3,list5,list7)
a = 16
csv = rbind(list0[I,],list0[I+a,],list0[I+a*2,],list0[I+a*3,],
            list0[II,],list0[II+a,],list0[II+a*2,],list0[II+a*3,],
            list0[III,],list0[III+a,],list0[III+a*2,],list0[III+a*3,],
            list0[IV,],list0[IV+a,],list0[IV+a*2,],list0[IV+a*3,])
write.csv(csv[,c(1:7)], "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\table.csv")


# A = seq(from = 1, to = 61, 4)
# write.csv(csv[A,c(8:11)], "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\table.csv")

rm(I,II,III,IV,list0,csv);gc()
################################## plot ########################################
# 不同估計方法
A = seq(from=1,to=13,length.out=4)
B = A + 1
C = A + 2
D = A + 3
N = data.frame(rbind(list1[A,],list2[A,],list3[A,],list4[A,],list5[A,],list6[A,],list7[A,],list8[A,],list9[A,]))
N1 = data.frame(rbind(list1[B,],list2[B,],list3[B,],list4[B,],list5[B,],list6[B,],list7[B,],list8[B,],list9[B,]))
W12 = data.frame(rbind(list1[C,],list2[C,],list3[C,],list4[C,],list5[C,],list6[C,],list7[C,],list8[C,],list9[C,]))
N3 = data.frame(rbind(list1[D,],list2[D,],list3[D,],list4[D,],list5[D,],list6[D,],list7[D,],list8[D,],list9[D,]))

# 不同組合模型
I = seq(from=1,to=33,length.out=9)
II = I + 1
III = II + 1
IV = III + 1

# 跑圖的時候格子要拉到最大
# 圖說才會在正確的位置
# estimate I
par(mfrow=c(1,2),pty="s")
plot(N[I,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(60,450),ylab="Average Estimate",xlab="Sample fraction",xaxt="n",main = "I vs III")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[I,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[I,]$E,lwd=3,lty=2,col="blue")
points(N1[I,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[I,]$E,lwd=3,lty=2,col="darkorchid")
points(W12[I,]$E,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[I,]$E,lwd=3,lty=2,col="green")
points(N3[I,]$E,lwd=5,pch=15,cex=1.2,col="green")
lines(N[I,]$V1,lwd=3,lty=2,col="black")
points(N[I,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2",'BB',"Obs"),
       col= c('red','blue','darkorchid','green','black'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE,cex = .9)

# RMSE I
plot(N[I,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,100),ylab="RMSE",xlab="Sample fraction",xaxt="n",main = "I vs III")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[I,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[I,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[I,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[I,]$RMSE,lwd=3,lty=2,col="darkorchid")
points(W12[I,]$RMSE,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[I,]$RMSE,lwd=3,lty=2,col="green")
points(N3[I,]$RMSE,lwd=5,pch=15,cex=1.2,col="green")
legend("topright",legend  = c("wBB1","wBB2","wChao2",'BB'),
       col= c('red','blue','darkorchid','green'),lty = c(2,2,2,2),
       pch=c(16,17,18,15),merge = TRUE,cex = .9)


# estimate II
plot(N[II,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(95,350),ylab="Average Estimate",xlab="Sample fraction",xaxt="n",main = "II vs II")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[II,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[II,]$E,lwd=3,lty=2,col="blue")
points(N1[II,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[II,]$E,lwd=3,lty=2,col="darkorchid")
points(W12[II,]$E,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[II,]$E,lwd=3,lty=2,col="green")
points(N3[II,]$E,lwd=5,pch=15,cex=1.2,col="green")
lines(N[II,]$V1,lwd=3,lty=2,col="black")
points(N[II,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2",'BB',"Obs"),
       col= c('red','blue','darkorchid','green','black'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE,cex = .9)

# RMSE II
plot(N[II,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,110),ylab="RMSE",xlab="Sample fraction",xaxt="n",main = "II vs II")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[II,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[II,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[II,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[II,]$RMSE,lwd=3,lty=2,col="darkorchid")
points(W12[II,]$RMSE,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[II,]$RMSE,lwd=3,lty=2,col="green")
points(N3[II,]$RMSE,lwd=5,pch=15,cex=1.2,col="green")
legend("topright",legend  = c("wBB1","wBB2","wChao2",'BB'),
       col= c('red','blue','darkorchid','green'),lty = c(2,2,2,2),
       pch=c(16,17,18,15),merge = TRUE,cex = .9)


# estimate III
plot(N[III,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(90,360),ylab="Average Estimate",xlab="Sample fraction",xaxt="n",main ="II vs III")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[III,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[III,]$E,lwd=3,lty=2,col="blue")
points(N1[III,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[III,]$E,lwd=3,lty=2,col="darkorchid")
points(W12[III,]$E,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[III,]$E,lwd=3,lty=2,col="green")
points(N3[III,]$E,lwd=5,pch=15,cex=1.2,col="green")
lines(N[III,]$V1,lwd=3,lty=2,col="black")
points(N[III,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2","BB","Obs"),
       col= c('red','blue','darkorchid','green','black'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE,cex = .9)

# RMSE III
plot(N[III,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,110),ylab="RMSE",xlab="Sample fraction",xaxt="n",main ="II vs III")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[III,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[III,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[III,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[III,]$RMSE,lwd=3,lty=2,col="darkorchid")
points(W12[III,]$RMSE,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[III,]$RMSE,lwd=3,lty=2,col="green")
points(N3[III,]$RMSE,lwd=5,pch=15,cex=1.2,col="green")
legend("topright",legend  = c("wBB1","wBB2","wChao2","BB"),
       col= c('red','blue','darkorchid','green'),lty = c(2,2,2,2),
       pch=c(16,17,18,15),merge = TRUE,cex = .9)


# estimate IV
plot(N[IV,]$E,type="l",lwd=3,lty=2,col="red2",ylim=c(80,340),ylab="Average Estimate",xlab="Sample fraction",xaxt="n",main = "III vs VI")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[IV,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[IV,]$E,lwd=3,lty=2,col="blue")
points(N1[IV,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[IV,]$E,lwd=3,lty=2,col="darkorchid")
points(W12[IV,]$E,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[IV,]$E,lwd=3,lty=2,col="green")
points(N3[IV,]$E,lwd=5,pch=15,cex=1.2,col="green")
lines(N[IV,]$V1,lwd=3,lty=2,col="black")
points(N[IV,]$V1,lwd=3,pch=4,col="black")
abline(h=S12,lwd=3,col="gray")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2","BB","Obs"),
       col= c('red','blue','darkorchid','green','black'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE,cex = .9)

# RMSE IV
plot(N[IV,]$RMSE,type="l",lwd=3,lty=2,col="red2",ylim=c(0,125),ylab="RMSE",xlab="Sample fraction",xaxt="n",main = "III vs VI")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(N[IV,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(N1[IV,]$RMSE,lwd=3,lty=2,col="blue")
points(N1[IV,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
lines(W12[IV,]$RMSE,lwd=3,lty=2,col="darkorchid")
points(W12[IV,]$RMSE,lwd=5,pch=18,cex=1.2,col="darkorchid")
lines(N3[IV,]$RMSE,lwd=3,lty=2,col="green")
points(N3[IV,]$RMSE,lwd=5,pch=15,cex=1.2,col="green")
legend("topright",legend  = c("wBB1","wBB2","wChao2","BB"),
       col= c('red','blue','darkorchid','green'),lty = c(2,2,2,2),
       pch=c(16,17,18,15),merge = TRUE,cex = .9)

############################ real data #########################################
##### BCI #####
P_X = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\BCI\\bci.tree\\BCI1.csv"))
P_Y = data.frame(read.csv("D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\data\\BCI\\bci.tree\\BCI8.csv"))
P_X = P_X[,-1]
P_Y = P_Y[,-1]
common_rows = which(rowSums(P_X)*rowSums(P_Y)>0)
O = obs(rowSums(P_X[common_rows,]),rowSums(P_Y[common_rows,]))
cbind(rowSums(P_X),rowSums(P_Y))
cbind(rowSums(P_X[common_rows,]),rowSums(P_Y[common_rows,]))
rbind(c(length(which(rowSums(P_X) == 1)),length(which(rowSums(P_X) == 2)),length(which(rowSums(P_X) == 3)),length(which(rowSums(P_X) > 3)),length(which(rowSums(P_X) > 0))),
      c(length(which(rowSums(P_Y) == 1)),length(which(rowSums(P_Y) == 2)),length(which(rowSums(P_Y) == 3)),length(which(rowSums(P_Y) > 3)),length(which(rowSums(P_Y) > 0))))
p1 = rowSums(P_X)/ncol(P_X)
p2 = rowSums(P_Y)/ncol(P_Y)
mean(p1);sd(p1)/mean(p1)
mean(p2);sd(p2)/mean(p2)

set.seed(123)
r = data.frame(round(rbind(realdata(P_X,P_Y,.1),realdata(P_X,P_Y,.2),realdata(P_X,P_Y,.3),realdata(P_X,P_Y,.4),
                           realdata(P_X,P_Y,.5),realdata(P_X,P_Y,.6),realdata(P_X,P_Y,.7),realdata(P_X,P_Y,.8),realdata(P_X,P_Y,.9)),2));r
r. = data.frame(rbind(r[1:3,],r[7:9,],r[13:15,],r[19:21,],r[25:27,]))
write.csv(r., "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\real.csv")

##  real plot ##
# 不同模型
A = seq(from=1,to=25,length.out=9)
B = A + 1
C =  A + 2
A;B;C

par(mfrow=c(1,3),pty="s")
plot(r[A,]$E,type="l",lwd=3,lty=2,col="red2", ylim = c(200,315),ylab="Average Estimate",xlab="Sample fraction",xaxt="n",main = "BCI")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(r[A,]$E,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$E,lwd=3,lty=2,col="blue")
points(r[B,]$E,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[C,]$E,lwd=3,lty=2,col="darkgreen")
points(r[C,]$E,lwd=5,pch=18,cex=1.2,col="darkgreen")
lines(r[A,]$V1,lwd=3,lty=2,col="black")
points(r[A,]$V1,lwd=3,pch=4,col="black")
abline(h= O,lwd=3,col="gray")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2","Obs"),
       col= c('red','blue','darkgreen','black'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE)

plot(r[A,]$RMSE,type="l",lwd=3,lty=2,col="red2", ylim = c(0,37),ylab="RMSE",xlab="Sample fraction",xaxt="n",main = "BCI")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(r[A,]$RMSE,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$RMSE,lwd=3,lty=2,col="blue")
points(r[B,]$RMSE,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[C,]$RMSE,lwd=3,lty=2,col="darkgreen")
points(r[C,]$RMSE,lwd=5,pch=18,cex=1.2,col="darkgreen")
legend("topright",legend  = c("wBB1","wBB2","wChao2"),
       col= c('red','blue','darkgreen'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE)

plot(r[A,]$CI,type="l",lwd=3,lty=2,col="red2",ylab="95 % CI Coverage",xlab="Sample fraction",ylim = c(0.75,.95),xaxt="n",main = "BCI")
axis(1, c(1:9), labels=c(".1", ".2", ".3", ".4", ".5",'.6',',7','.8','.9'))
points(r[A,]$CI,lwd=5,pch=16,cex=1.2,col="red2")
lines(r[B,]$CI,lwd=3,lty=2,col="blue")
points(r[B,]$CI,lwd=5,pch=17,cex=1.2,col="blue")
lines(r[C,]$CI,lwd=3,lty=2,col="darkgreen")
points(r[C,]$CI,lwd=5,pch=18,cex=1.2,col="darkgreen")
legend("bottomright",legend  = c("wBB1","wBB2","wChao2"),
       col= c('red','blue','darkgreen'),lty = c(2,2,2,2,2),
       pch=c(16,17,18,15,4),merge = TRUE)


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


## estiamte
r1 = data.frame(round(rbind(realdata1(P_1,P_2,.1),realdata1(P_1,P_2,.2),
                            realdata1(P_1,P_2,.3),realdata1(P_1,P_2,.4),
                            realdata1(P_1,P_2,.5),realdata1(P_1,P_2,.6),
                            realdata1(P_1,P_2,.7),realdata1(P_1,P_2,.8),realdata1(P_1,P_2,.9)),2));r1
r2 = data.frame(round(rbind(realdata1(P_1,P_3,.1),realdata1(P_1,P_3,.2),
                            realdata1(P_1,P_3,.3),realdata1(P_1,P_3,.4),
                            realdata1(P_1,P_3,.5),realdata1(P_1,P_3,.6),
                            realdata1(P_1,P_3,.7),realdata1(P_1,P_3,.8),realdata1(P_1,P_3,.9)),2));r2
r3 = data.frame(round(rbind(realdata1(P_1,P_4,.1),realdata1(P_1,P_4,.2),
                            realdata1(P_1,P_4,.3),realdata1(P_1,P_4,.4),
                            realdata1(P_1,P_4,.5),realdata1(P_1,P_4,.6),
                            realdata1(P_1,P_4,.7),realdata1(P_1,P_4,.8),realdata1(P_1,P_4,.9)),2));r3
r4 = data.frame(round(rbind(realdata1(P_2,P_3,.1),realdata1(P_2,P_3,.2),
                            realdata1(P_2,P_3,.3),realdata1(P_2,P_3,.4),
                            realdata1(P_2,P_3,.5),realdata1(P_2,P_3,.6),
                            realdata1(P_2,P_3,.7),realdata1(P_2,P_3,.8),realdata1(P_2,P_3,.9)),2));r4
r5 = data.frame(round(rbind(realdata1(P_2,P_4,.1),realdata1(P_2,P_4,.2),
                            realdata1(P_2,P_4,.3),realdata1(P_2,P_4,.4),
                            realdata1(P_2,P_4,.5),realdata1(P_2,P_4,.6),
                            realdata1(P_2,P_4,.7),realdata1(P_2,P_4,.8),realdata1(P_2,P_4,.9)),2));r5
r6 = data.frame(round(rbind(realdata1(P_3,P_4,.1),realdata1(P_3,P_4,.2),
                            realdata1(P_3,P_4,.3),realdata1(P_3,P_4,.4),
                            realdata1(P_3,P_4,.5),realdata1(P_3,P_4,.6),
                            realdata1(P_3,P_4,.7),realdata1(P_3,P_4,.8),realdata1(P_3,P_4,.9)),2));r6

I = c(9:12,17:20,25:28)
r = rbind(r1[I,],r2[I,],r3[I,],r4[I,],r5[I,],r6[I,])
write.csv(r, "D:\\nagisa\\NAGISA\\學校\\碩班\\論文\\code\\table\\real.csv")
S12 = r1$E[17];S13 = r2$E[17];S14 = r3$E[17];S23 = r4$E[17];S24 = r5$E[17];S34 = r6$E[17]

## one community richness
# estimate
S1 = more(rowSums(P_1),T1,.5)
S2 = more(rowSums(P_2),T2,.5)
S3 = more(rowSums(P_3),T3,.5)
S4 = more(rowSums(P_4),T4,.5)
S = round(c(S1,S2,S3,S4),1);S

# SD
a = .5
Sd = round(se_m(rowSums(P_1),T1,ceiling(T1)/a,S1_O,S1,.01),
           se_m(rowSums(P_2),T2,ceiling(T2)/a,S2_O,S2,.01),
           se_m(rowSums(P_3),T3,ceiling(T3)/a,S3_O,S3,.01),
           se_m(rowSums(P_4),T4,ceiling(T4)/a,S4_O,S4,.01),2);Sd
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
S..1 = more((rowSums(P_1)+rowSums(P_2)),T1+T2,.5)
S..2 = more((rowSums(P_1)+rowSums(P_3)),T1+T3,.5)
S..3 = more((rowSums(P_1)+rowSums(P_4)),T1+T4,.5)
S..4 = more((rowSums(P_2)+rowSums(P_3)),T2+T3,.5)
S..5 = more((rowSums(P_2)+rowSums(P_4)),T2+T4,.5)
S..6 = more((rowSums(P_3)+rowSums(P_4)),T2+T4,.5)

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
B = c(B.1,B.2,B.3,B.4,B.5,B.6)

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

# estimate
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

