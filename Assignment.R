rm(list = ls())

dataQ1 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q1.csv")

x = dataQ1$x
y = dataQ1$y

plot(x,y)

n = length(x)


# Linear spline

h0 <- rep(1,n)
h1 <- x
h2 <- (x-3)
h2[h2 < 0] <- 0
h3 <- (x-7)
h3[h3 < 0] <- 0

df1 <- data.frame(y,h0,h1,h2,h3)
fit <- lm(y ~0+h0+h1+h2+h3,df1)
betaL <- coef(fit)
plot(x,y,pch=16,cex=.8,col='gray')
abline(v=3,lty=2,col='red')
abline(v=7,lty=2,col='red')
curve(betaL[1]+betaL[2]*x, from = min(x), to = 3, col='red',add=T,lwd=2)
curve(betaL[1]+betaL[2]*x+betaL[3]*(x-3), from = 3,to = 7, col='red',add=T,lwd=2)
curve(betaL[1]+betaL[2]*x+betaL[3]*(x-3)+betaL[4]*(x-7), from = 7, to = max(x), col='red',add=T,lwd=2)


#Cubic Spline

h0 <- rep(1,n)
h1 <- x
h2 <- x^2
h3 <- x^3 
h4 <- (x-5)^3
h4[h4 < 5] <- 0

df2 <- data.frame(y,h0,h1,h2,h3,h4)
fit <- lm(y ~ 0+h0+h1+h2+h3+h4,df2)
betaC <- coef(fit)
plot(x,y,pch=16,cex=.8,col='gray')
abline(v=5,lty=2,col='red')
curve(betaC[1]+betaC[2]*x+betaC[3]*x^2+betaC[4]*x^3, from = min(x), to = 5, col='red',add=T,lwd=2)
curve(betaC[1]+betaC[2]*x+betaC[3]*x^2+betaC[4]*x^3+betaC[5]*(x-5)^3, from = 5, to = max(x), col='red',add=T,lwd=2)


