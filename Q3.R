rm(list = ls())
library(np)
library(mgcv)

dataQ3 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q3.csv")

y = dataQ3$y
x1 = dataQ3$x1
x2 = dataQ3$x2


bw <- npindexbw(formula=y~x1+x2,method='ichimura')

model <- npindex(bws=bw, gradients=TRUE)
summary(model)

npplot(bws=bw)
curve(x^2,add=T,col='red',lty=2)


##### GAM

c = ave(y)

y = y - c

m1 <- gam(y~s(x1)+s(x2), method = 'REML',
          family=gaussian(link = "identity"))

gam.check(m1)
plot(m1)