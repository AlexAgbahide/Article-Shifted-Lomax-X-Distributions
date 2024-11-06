cdf <- function(x,alpha,lambda){
  1-(1+x/lambda)^(-alpha)
}


pdf <- function(x,alpha,lambda){
  (alpha/lambda)*((1+x/lambda)^(-alpha-1))
}


alpha = c(0.5, 0.5,3.5,1)
lambda <- c(0.01,0.5,1,1)
x <- seq(0, 3, length.out = 10000)

plot(x,pdf(x,alpha[1],lambda[1]), ylab = "fdp",type ='l',lty=1,lwd=2,main = "", ylim=c(0,1))

for (i in 2:length(alpha)){
  y <- pdf(x, alpha[i], lambda[i])
  lines(x, y, col = i ,lty=i,lwd =2)
}


legend("topright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4),lwd=2, cex = 0.8)





plot(x,cdf(x,alpha[1],lambda[1]), ylab = "FDC",type ='l',lty=1,lwd=2,main = "", ylim=c(0,1))

for (i in 2:length(alpha)){
  y <- cdf(x, alpha[i], lambda[i])
  lines(x, y, col = i ,lty=i,lwd =2)
}


legend("bottomright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4),lwd = 2, cex = 0.8)




alpha = c(0.5, 1,3.5,5)
lambda <- c(0.1,0.3,0.5,0.5)
x <- seq(0, 5, length.out = 10000)
plot(x,y <- pdf(x,alpha[1],lambda[1])/(1-cdf(x,alpha[1],lambda[1])), ylab = "hrf",lty=1,  type ='l',lwd=2, main = "")

for (i in 2:length(alpha)){
  y <- pdf(x,alpha[i],lambda[i])/(1-cdf(x,alpha[i],lambda[i]))
  lines(x, y, col = i , lty=i,lwd =2)
}


legend("topright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4), cex = 0.8,lwd=2)



alpha = c(5, 5,2.5,4)
lambda <- c(1,2,0.5,0.5)
x <- seq(0, 5, length.out = 10000)
plot(x,y <- pdf(x,alpha[1],lambda[1])/(1-cdf(x,alpha[1],lambda[1])), ylab = "hrf",lty=1,  type ='l',lwd=2, main = "",ylim = c(0,5))

for (i in 2:length(alpha)){
  y <- pdf(x,alpha[i],lambda[i])/(1-cdf(x,alpha[i],lambda[i]))
  lines(x, y, col = i , lty=i,lwd =2)
}


legend("topright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4), cex = 0.8,lwd=2)

