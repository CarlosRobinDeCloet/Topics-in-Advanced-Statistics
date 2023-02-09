rm(list = ls())

dataQ4 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q4.csv")


x = dataQ4$x
y = dataQ4$y

plot(x,y)

k = 100 # k should be smaller than n
h = 0.1
n = length(x)

calculate_m <- function(u){
  
  dataQ4 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q4.csv")
  x = dataQ4$x
  y = dataQ4$y
  
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
    
    m_hi = m_hi + (1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)*closePoints$y[l])
    m_lo = m_lo + 1/sqrt(2*pi)*exp(-1/2*(closePoints$distance[l]/h)^2)
  }
  
  m_hi/m_lo
}


leave_one_out_cv <- function(x){
  
  closestPoints = data.frame(i = numeric(0), dis = numeric(0), ABS=numeric(0), y=numeric(0))
  x_t = x[-i]
  y0 = y[-i]
  counter = 1
  for(j in 1:99) {
    distance = (x[i] - x0[j])
    absolute = abs(x[i] - x0[j])
    closest[counter,] <- c(counter, distance, absolute, y0[j])
    counter = counter +1
  }
  sorted <- closest[order(closest$ABS),][1:k,]
  
  m_hi = 0
  m_lo = 0
  
  for(l in 1:k) {
    Kh = 1/h * 1/sqrt(2*pi)*exp(-(sorted[l,2]/h)^2/2)
    
    m_hi = m_hi + Kh*sorted[l,4]
    m_lo = m_lo + Kh
  }
  m_hi / m_lo - y[i]
  
  
  
  
}

m_diff <- function(i,h,k){
  closest = data.frame(i = numeric(0), dis = numeric(0), ABS=numeric(0), y=numeric(0))
  x_t = x[-i]
  y_t = y[-i]
  counter = 1
  for(j in 1:99) {
    distance = (x[i] - x0[j])
    absolute = abs(x[i] - x0[j])
    closest[counter,] <- c(counter, distance, absolute, y0[j])
    counter = counter +1
  }
  sorted <- closest[order(closest$ABS),][1:k,]
  
  m_hi = 0
  m_lo = 0
  
  for(l in 1:k) {
    Kh = 1/h * 1/sqrt(2*pi)*exp(-(sorted[l,2]/h)^2/2)
    
    m_hi = m_hi + Kh*sorted[l,4]
    m_lo = m_lo + Kh
  }
  m_hi / m_lo - y[i]
}





MSE <- function(y_hat,y){
  
  n = length(y_hat)
  error = (y_hat-y)^2
  sum(error)/n
}

curve(sapply(x,calculate_m),add=T,col='red')
#
#cv <- function(k){
#  mx.list <- rep(NA,n)
#  for (i in 1:n){
#    y1 <- y[-i]
#    x1 <- x[-i]
#    mx.list[i] <- median(y1[order(abs(x1-x0[i]))][1:k])
#  }
#  return(mean((mx.list-y0)^2))
#}
#k.list <- 2:20
#cv.list <- sapply(k.list,cv)
#plot(k.list,cv.list)