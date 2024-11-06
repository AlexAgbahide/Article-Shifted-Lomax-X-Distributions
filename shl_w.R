w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}

alpha =1.5
lambda =c(0.5,1.5,4.5,5.5)
  m = c(0.001,0.05,0.1,0.2)
  a=c(1.5, 0.5,3.5,10.5)
  k=c(3,3.5,1.5,2.5)
  x <- seq(0, 8, length.out = 10000)
  

  plot(x,cdf(x,alpha,lambda[1],m[1],a[1],k[1]), ylab = "fdc",type ='l',lty=1,lwd=2,main = "", ylim=c(0,1))
  
  for (i in 2:length(lambda)){
    y <- cdf(x,alpha,lambda[i],m[i],a[i],k[i])
    lines(x, y, col = i ,lty=i,lwd =2)
  }
  
  
  legend("bottomright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda ,"," , "m=", m ,"," , "a=", a ,"," , "k=", k )), 
         col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4),lwd=2, cex = 0.8)
  
  x <- seq(0, 4, length.out = 10000)
  

plot(x,pdf(x,alpha,lambda[1],m[1],a[1],k[1]), ylab = "fdp",type ='l',lty=1,lwd=2,main = "", ylim=c(0,0.8))
  
for (i in 2:length(lambda)){
    y <- pdf(x,alpha,lambda[i],m[i],a[i],k[i])
    lines(x, y, col = i ,lty=i,lwd =2)
  }
  
  
legend("topright", legend = c(paste("\u03b1 =", alpha ,"," , "\u03bb =", lambda ,"," , "m=", m ,"," , "a=", a ,"," , "k=", k )), 
       col = c("black", "red", "green", "blue" ), lty = c(1,2,3,4),lwd=2, cex = 0.8)
  
