#par(mfrow=c(1,2))
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

#Fonction de répartition
cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}

#Fonction de densité
pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}

#hazard function
h<-function(x, alpha,lambda, m,a,k){
  pdf(x, alpha,lambda, m,a,k)/(1-cdf(x, alpha,lambda, m,a,k))
}

#survival finction
s<- function(x, alpha,lambda, m,a,k){
  (1-cdf(x, alpha,lambda, m,a,k))
}

a = c(2.5,5,1.8,1.9)
k = c(1.2,1.5,0.5,1)
alpha <- c(3,2.5,2.5,3)
lambda <- c(2.5,4,2.5,1.5)
m <- c(0.025, 0.05, 0.25, 0.015)
x <- seq(0, 5, length.out = 10000)

col = c("blue", "red", "green", "black")



plot(x, y <- h(x, alpha[1], lambda[1]  , m[1],a[1],k[1]), type = 'l',lty=1, ylim = c(0, 2), xlab = "x", ylab = "hrf",col= col[1],lwd=2)
legend("topright", legend = c(paste("m = ", m,",a = ",a, ",k = ",k,  ",\u03BB = ", lambda, ",\u03B1 = ",alpha)), col = c("blue", "red", "green", "black"), lty = c(1,2,3,4), cex = 0.8)

for (i in 2:length(m)){
  y <- h(x, alpha[i], lambda[i], m[i],a[i],k[i])
  lines(x, y, col = col[i],lwd=2,lty=i)
}


plot(x, y <- s(x, alpha[1], lambda[1]  , m[1],a[1],k[1]),lty=1, type = 'l', ylim = c(0, 1), xlab = "x", ylab = "Suf",col= col[1],lwd=2)
legend("topright", legend = c(paste("m =", m,",a =", a,",k =", k, ",\u03BB= ", lambda, ",\u03B1= ", alpha)), col = c("blue", "red", "green", "black"), lty = c(1,2,3,4), cex = 0.8)

for (i in 2:length(m)){
  y <- s(x, alpha[i], lambda[i], m[i],a[i],k[i])
  lines(x, y, col = col[i],lwd=2,lty=i)
}

