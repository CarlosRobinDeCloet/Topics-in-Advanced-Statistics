rm(list = ls())

dataQ2 = read.csv("C:\\Users\\Carlos de Cloet\\Desktop\\Topics in Advanced Statistics\\Assignment\\RCode\\Q2.csv")

x1 = dataQ2$x1
x2 = dataQ2$x2
y = dataQ2$y

prior = sum(y)/length(y)

bandwidthMatrix = matrix(data=c(0.5, 0, 0, 0.5), nrow=2,ncol=2)

std_x1 = sqrt(var(x1))
std_x2 = sqrt(var(x2))
rho = cov(x1,x2)/(std_x1*std_x2)
mu_x1 = mean(x1)
mu_x2 = mean(x2)

bivariate_density <- function(x1, x2, std_x1, std_x2, rho, mu_x1, mu_x2){
  
  
  return (1/(2*pi*std_x1*std_x2*(1-rho^2))*exp((-1/(2*(1-rho^2)))*( ((x1-mu_x1)/std_x1)^2 - 2*rho*((x1-mu_x1)/std_x1)*((x2-mu_x2)/std_x2) + ((x2-mu_x2)/std_x2)^2 )))
}
  

gaussian_kernel <- function(x){
  return (1/(2*pi)*exp(-(x^2)/2))
}


