rm(list = ls())

dataQ2 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q2.csv")

x1 = dataQ2$x1
x2 = dataQ2$x2
y = dataQ2$y

x1.seq = seq(-4, 3.99, 1/64)
x2.seq = seq(-4, 3.99, 1/64)

prior1 = sum(y)/length(y)
prior0 = 1 - prior1

dataY1 <- subset(dataQ2, y == 1)
dataY0 <- subset(dataQ2, y == 0)

x_dataY1 = dataY1[c('x1','x2')]
x_dataY0 = dataY0[c('x1','x2')]

f1xMatrix = matrix(0,nrow=512,ncol=512)

for (i in 1:length(x1.seq)){
  for (j in 1:length(x2.seq)){
    f1xMatrix[i,j] = kernel_density_estimator(x1.seq[i],x2.seq[j],h=0.5,dataY1$x1,dataY1$x2)
  }
  
  print(paste0('Current row is: ',i))
}

f0xMatrix = matrix(0,nrow=512,ncol=512)

for (i in 1:length(x1.seq)){
  for (j in 1:length(x2.seq)){
    f0xMatrix[i,j] = kernel_density_estimator(x1.seq[i],x2.seq[j],h=0.5,dataY0$x1,dataY0$x2)
  }
  
  print(paste0('Current row is: ',i))
}

p1x = f1xMatrix*prior1/(f1xMatrix*prior1+f0xMatrix*prior0)

contour(x1.seq, x2.seq, p1x,main='Contour plot of p(x1,x2)',xlab='x1',ylab='x2')

x1_bin = ceiling((x1 -(-4))/(4-(-4))*512)
x2_bin = ceiling((x2 -(-4))/(4-(-4))*512)

dataQ2['yhat'] <- 0

for (i in 1:300){
  
  if (p1x[x1_bin[i],x2_bin[i]] >= 0.5){
    dataQ2$yhat[i] = 1
  }
}

plot(dataQ2$x1,dataQ2$x2,
    pch=ifelse((dataQ2$y == 1),1,4),
    col=ifelse((dataQ2$yhat == 1),'red','blue'),
    main='Predictions for (x1,x2)',
    xlab='x1',
    ylab='x2'
    )

print(paste0('Amount of misclassified points is: ',sum(misclassified = !(dataQ2$y == dataQ2$yhat))))

kernel_density_estimator <- function(x1,x2,h,data_x1, data_x2){
  
  n = length(data_x1)
  
  kernel1 = 0
  kernel2 = 0
  
  density = 0
  
  for (i in 1:n){
    kernel1 = 1/h*gaussian_density((x1-data_x1[i])/h)
    kernel2 = 1/h*gaussian_density((x2-data_x2[i])/h)
    density = density + kernel1*kernel2
  }
  
  density / n
}

gaussian_density <- function(x){
  
  1/(sqrt(2*pi))*exp(-1/2*(x^2))
}
