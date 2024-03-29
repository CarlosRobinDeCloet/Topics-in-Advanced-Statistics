rm(list = ls())

dataQ4 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q4.csv")

x = dataQ4$x
y = dataQ4$y

plot(x,y,pch=19,col='grey',main='Plot of m_hat')

k = 20 # k should be smaller than n
h = 1
n = length(x)

calculate_m <- function(u){
  
  points = data.frame(index=numeric(0), distance = numeric(0), absolute = numeric(0), x = numeric(0), y = numeric(0))
  for (i in 1:n){
    index = i
    distance = (u - x[i])
    absolute = abs(u -x[i])
    points[i,] = c(index, distance, absolute, x[i], y[i])
  }
  
  nearestNeighbours <- points[order(points$absolute),][1:k,]
  
  numerator = 0
  denominator = 0
  
  for(l in 1:k){
    
    numerator = numerator + 1/h*(1/sqrt(2*pi)*exp(-1/2*(nearestNeighbours$distance[l]/h)^2)*nearestNeighbours$y[l])
    denominator = denominator + 1/h*1/sqrt(2*pi)*exp(-1/2*(nearestNeighbours$distance[l]/h)^2)
  }
  
  numerator/denominator
}

curve(sapply(x,calculate_m),add=T,col='black',lw='2')

leave_one_out_cv <- function(x, ind,k,h){
  
  points = data.frame(index = numeric(0), distance = numeric(0), absolute = numeric(0), x = numeric(0), y = numeric(0))
  x_t = x[-ind]
  y_t = y[-ind]
  
  for(i in 1:99) {
    index = i
    distance = (x[ind] - x_t[i])
    absolute = abs(x[ind] - x_t[i])
    points[i,] <- c(index, distance, absolute, x_t[i], y_t[i])
    
  }
  nearestNeighbours <- points[order(points$absolute),][1:k,]
  
  numerator = 0
  denominator = 0
  
  for(l in 1:k) {
    
    numerator = numerator + 1/h*(1/sqrt(2*pi)*exp(-1/2*(nearestNeighbours$distance[l]/h)^2)*nearestNeighbours$y[l])
    denominator = denominator + 1/h*1/sqrt(2*pi)*exp(-1/2*(nearestNeighbours$distance[l]/h)^2)
  }
  
  y_hat = numerator/denominator
  
  (y_hat-y[ind])^2
}

ISEMatrix = matrix(0,nrow=50,ncol=200)

k.seq <- seq(1,50,1)
h.seq <- seq(0.01,2,0.01)
i.seq <- seq(1,100,1)

for(k in 1:length(k.seq)){
  
  print(paste0('K value is: ',k))
  
  for(h in 1:length(h.seq)){
    ISE = 0
    for(i in i.seq){
      ISE = ISE + leave_one_out_cv(x,i,k.seq[k],h.seq[h])
    }
    ISE = ISE/99
    ISEMatrix[k,h] = ISE
  }
}

