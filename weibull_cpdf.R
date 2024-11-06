cdf <- function(x,a,k){
  1-exp(-(x/k)^(a))
}


pdf <- function(x,a,k){
  (a/k^a)*x^(a-1)*exp(-(x/k)^(a))
}


alpha = c(0.5, 3,0.5,5)
lambda <- c(2,0.5,0.5,1)
x <- seq(0, 3, length.out = 10000)

plot(x,pdf(x,alpha[1],lambda[1]), ylab = "fdp",type ='l',lty=2,lwd=2,main = "", ylim=c(0,3))

for (i in 2:length(alpha)){
  y <- pdf(x, alpha[i], lambda[i])
  lines(x, y, col = i ,lty=2,lwd =2)
}


legend("topright", legend = c(paste("a =", alpha ,"," , "k =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty = 2, cex = 0.8)



#alpha = c(0.5, 1,3.5,5)
#lambda <- c(0.1,0.3,0.5,1)
#x <- seq(0, 3, length.out = 10000)

plot(x,cdf(x,alpha[1],lambda[1]), ylab = "FDC",type ='l',lty=2,lwd=2,main = "", ylim=c(0,1))

for (i in 2:length(alpha)){
  y <- cdf(x, alpha[i], lambda[i])
  lines(x, y, col = i ,lty=i,lwd =2)
}


legend("bottomright", legend = c(paste("a =", alpha ,"," , "k =", lambda  )), 
       col = c("black", "red", "green", "blue" ), lty =c(1,2,3,4),lwd=2, cex = 0.8)


