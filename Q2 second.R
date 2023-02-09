rm(list = ls())

dataQ2 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q2.csv")

x1 = dataQ2$x1
x2 = dataQ2$x2
y = dataQ2$y

x1min = min(x1)
x1max = max(x1)
x2min = min(x2)
x2max = max(x2)


x1.seg = seq(-4, 3.99, 1/64)
x2.seg = seq(-4, 3.99, 1/64)

prior1 = sum(y)/length(y)
prior0 = 1 - prior1

dataY1 <- subset(dataQ2, y == 1)
dataY0 <- subset(dataQ2, y == 0)


f1x1 <- density(dataY1$x1,bw=0.5,kernel='gaussian')$y
f1x2 <- density(dataY1$x2,bw=0.5,kernel='gaussian')$y

f1x = tcrossprod(f1x1,f1x2)

f0x1 <- density(dataY0$x1,bw=0.5,kernel='gaussian')$y
f0x2 <- density(dataY0$x2,bw=0.5,kernel='gaussian')$y

f0x = tcrossprod(f0x1,f0x2)

p1x = f1x*prior1/(f1x*prior1+f0x*prior0)
p0x = f0x*prior0/(f1x*prior1+f0x*prior0)

contour(x1.seg, x2.seg, p1x)

x1 = dataQ2$x1
x2 = dataQ2$x2

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



