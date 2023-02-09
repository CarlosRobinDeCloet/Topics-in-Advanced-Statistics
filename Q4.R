rm(list = ls())

dataQ4 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q4.csv")

x = dataQ4$x
y = dataQ4$y

plot(x,y)

k = 100 # k should be smaller than n
h = 0.1
n = length(x)

calculate_m <- function(u){
  
  points = data.frame(index=numeric(0), distance = numeric(0), absolute = numeric(0), x = numeric(0), y = numeric(0))
  for (i in 1:n){
    index = i
    distance = (u - x[i])
    absolute = abs(u -x[i])
    points[i,] = c(index, distance, absolute, x[i], y[i])
  }
  
  closePoints <- points[order(points$absolute),][1:k,]
  
  m_hi = 0
  m_lo = 0
  
  for(l in 1:k){
    
    m_hi = m_hi + 1/h*(1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)*closePoints$y[l])
    m_lo = m_lo + 1/h*1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)
  }
  
  m_hi/m_lo
}

curve(sapply(x,calculate_m),add=T,col='red')

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
  closePoints <- points[order(points$absolute),][1:k,]
  
  m_hi = 0
  m_lo = 0
  
  for(l in 1:k) {
    
    m_hi = m_hi + 1/h*(1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)*closePoints$y[l])
    m_lo = m_lo + 1/h*1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)
  }
  
  y_hat = m_hi/m_lo
  
  (y_hat-y[ind])^2
}

errorMatrix = matrix(0,nrow=50,ncol=200)

k.seq <- seq(1,10,1)
h.seq <- seq(0.01,2,0.01)
i.seq <- seq(1,100,1)

for(k in 1:length(k.seq)){
  
  print(paste0('K value is: ',k))
  
  for(h in 1:length(h.seq)){
    MSE = 0
    for(i in i.seq){
      MSE = MSE + leave_one_out_cv(x,i,k.seq[k],h.seq[h])
    }
    MSE = MSE/99
    errorMatrix[k,h] = MSE
  }
}

