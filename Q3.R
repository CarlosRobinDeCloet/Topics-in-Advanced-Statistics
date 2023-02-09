rm(list = ls())


library(np)
library(mgcv)

dataQ3 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q3.csv")

y = dataQ3$y
x1 = dataQ3$x1
x2 = dataQ3$x2

SIM <- npindexbw(formula=y~x1+x2,method='ichimura')

model <- npindex(bws=SIM, gradients=TRUE)
summary(model)

npplot(bws=SIM)
curve(x^2,add=T,col='red',lty=2)


##### GAM #####
c = ave(y)

y = y - c

GAM <- gam(y~s(x1)+s(x2), method = 'REML',
          family=gaussian(link = "identity"))

gam.check(GAM)
plot(GAM)


