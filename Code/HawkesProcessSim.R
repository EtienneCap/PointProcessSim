###### 1D Hawkes process simulation 

T_tot = 10

exp_kernel_function = function(t, alpha = 1, beta = 1){ # Exponentially decreasing kernel
  return(alpha * exp(-beta*t) * (t>0))
}

base_rate = 2
alpha = 0.7
beta = 3

time_points = c()
s=0
while(s < T_tot){
  lambda = base_rate + sum(exp_kernel_function(s-time_points, alpha = alpha, beta = beta))
  u = runif(1)
  w = -log(u)/lambda # Exponentially distributed waiting time with rate lambda
  s = s+w
  test = runif(1)
  if (test*lambda < base_rate + sum(exp_kernel_function(s-time_points, alpha = alpha, beta = beta))){
    time_points = c(time_points, s)
  }
}

time_points = time_points[time_points <= T_tot]

t = seq(0, 10, length.out = 500)
lambda_t = c()
for (t_i in t){
  l = base_rate
  for (t_event in time_points){
    l = l + exp_kernel_function(t_i - t_event, alpha = alpha, beta = beta)
  }
  lambda_t = c(lambda_t, l)
}

N_t = matrix(0, ncol = length(t))
for (point in time_points){
  N_t = N_t + as.numeric(point < t)
}


par(mfrow = c(2,1))
plot(t, lambda_t, type = 'l', lty = 'dashed', ylim = c(0, max(lambda_t)), main = "Realisation of a 1D Hawkes point process", xlab = "t", ylab = "Intensity function")
points(time_points, rep(0, length(time_points)), col = 'blue', pch = 8)

plot(t, N_t, col = "red", type = 'l', xlab = "t", ylab = expression(N[t]))

###### 2D Hawkes process simulation 

kernel_function = function(t, alpha, beta){
  return(alpha * exp(-beta * t) * (t>0))
}

d = 3

T_tot = 2

base_rate = matrix(data = c(1,6,3), nrow = d)
alpha = matrix(data = 1, nrow = d, ncol = d)
beta = matrix(data = 3, nrow = d, ncol = d)

print(base_rate)
print(alpha)
print(beta)

time_points = vector("list", 2)
for (i in 1:d){
  time_points[[i]] = numeric(0)
}

s = 0

while (s<T_tot){
  lambda = rep(0, d)
  for (m in 1:d){
    lambda[m] = base_rate[m] 
    for (n in 1:d){
      lambda[m] = lambda[m] + sum(kernel_function(s-time_points[[n]], alpha = alpha[m,n], beta = beta[m,n])) #+ sum(alpha[m, n] * exp(-beta[m, n] * (s-time_points[[n]]))) 
    }
  }
  lambda0 = sum(lambda)
  # print(paste0("lambda =", lambda0))
  u = runif(1)
  w = -log(u)/lambda0
  s = s + w
  test = runif(1)
  lambda_test = rep(0, d)
  for (m in 1:d){
    lambda_test[m] = base_rate[m] 
    for (n in 1:d){
      lambda_test[m] = lambda_test[m] + sum(kernel_function(s-time_points[[n]], alpha = alpha[m,n], beta = beta[m,n])) #+ sum(alpha[m, n] * exp(-beta[m, n] * (s-time_points[[n]]))) 
    }
  }
  if (test * lambda0 <= sum(lambda_test)){
    k = 1
    while (test * lambda0 > sum(lambda_test[1:k])){
      k = k+1
    }
    time_points[[k]] = c(time_points[[k]], s)
  }
}

time_points = sapply(time_points, function(x) x[x<=T_tot])
N = 750
t = seq(0, T_tot, length.out = N)

lambda_t = matrix(NA, nrow = d, ncol = N)

for (i in 1:N){
  lambda = rep(0, d)
  for (m in 1:d){
    lambda[m] = base_rate[m] 
    for (n in 1:d){
      lambda[m] = lambda[m] + sum(kernel_function(t[i]-time_points[[n]], alpha = alpha[m,n], beta = beta[m,n])) #+ sum(alpha[m, n] * exp(-beta[m, n] * (s-time_points[[n]]))) 
    }
  }
  lambda_t[,i]= lambda
}

par(mfrow = c(d,1))

for (i in 1:d){
  plot(t, lambda_t[i,], type = 'l', col = 'red', ylim = c(0, max(lambda_t[i,])), xlab = "", ylab = "Intensity function")
  points(time_points[[i]], rep(0, length(time_points[[i]])), col = "blue", pch = 8)
}




