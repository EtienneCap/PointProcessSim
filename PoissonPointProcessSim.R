######### Homogeneous Poisson Point process simulation

## 1-D Simulation (Ex: specific events in time)

T_tot = 10
lambda = 5
N = rpois(1, lambda*T_tot) # Random number of points

point_pattern = runif(N, min = 0, max = T_tot)

plot(point_pattern, rep(0, N), ylab = "", xlab = "Poisson point process")


## 2-D Simulation (Ex: trees in a forest)

x_max = 5 
y_max = 3
lambda = 5
N = rpois(1, lambda*x_max*y_max) # Random number of points

point_pattern_x = runif(N, min = 0, max = x_max)
point_pattern_y = runif(N, min = 0, max = x_max)

plot(point_pattern_x, point_pattern_y, ylab = "y", xlab = "x", main = "Poisson point process realisation in 2D")

######### Non-homogeneous Poisson Point process simulation

## 1-D Simulation (Ex: Clients arrival in a restaurant at lunch time)

intensity = function(t, T = T_tot, lam = 1){
  return(lam * sin(2*pi*t/T)^2)
}

# Homogeneous process
T_tot = 10
lambda = 5
N = rpois(1, lambda*T_tot) # Random number of points

point_pattern = runif(N, min = 0, max = T_tot)

#Thinning
final_point_pattern = c()
for (t in point_pattern){
  if (runif(1) < intensity(t, T_tot, lam = lambda)/lambda){
    final_point_pattern = c(final_point_pattern, t)
  }
}
x = seq(0, 10, length.out = 100)
plot(x, intensity(x, T_tot, lam = 5), type = 'l', ylab = "Intensity/density function", xlab = "State space")
points(final_point_pattern, rep(0, length(final_point_pattern)), pch = 8)
 

## 2-D Simulation (Ex: impact of darts on a target during a game)

intensity = function(x, y, d_tot = 1, A = 5){
  r = sqrt(x^2 + y^2)
  return(A*exp(-(r^2)/d_tot^2))
}

# Homogeneous process
x_max = 8
y_max = 8
lambda = 10 
N = rpois(1, lambda*x_max*y_max*4) # Random number of points

point_pattern_x = runif(N, min = -x_max, max = x_max)
point_pattern_y = runif(N, min = -y_max, max = x_max)

#Thinning
final_point_pattern_x = c()
final_point_pattern_y = c()

for (i in 1:N){
  if (runif(1) < intensity(point_pattern_x[i], point_pattern_y[i], d_tot = 2, A = lambda)/lambda){
    final_point_pattern_x = c(final_point_pattern_x, point_pattern_x[i])
    final_point_pattern_y = c(final_point_pattern_y, point_pattern_y[i])
  }
}

x = seq(-x_max, x_max, length.out = 50)
y = seq(-y_max, y_max, length.out = 50)

z = outer(x, y, function(x,y) intensity(x, y, d_tot = 2, A = lambda)) # Contour plot of the intensity function


plot(point_pattern_x, point_pattern_y, pch = 1, col ='red', xlab = "x", ylab = "y", main = "Point process realisation")
contour(x, y, z, xlim = c(-x_max, x_max), ylim = c(-y_max, y_max), add = T, lwd =  2)
points(final_point_pattern_x, final_point_pattern_y, pch = 8, col = "blue") 

