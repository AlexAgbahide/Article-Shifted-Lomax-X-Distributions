
#Gumbell

G <- function(x,b,k){
  exp(-exp((b-x)/k))
}

g <- function(x,b,k){
  (1/k)*exp((b-x)/k)*exp(-exp((b-x)/k))
}

cdf <- function(x,alpha, lambda,m,b,k){
  1-(1+x*(G(x,b, k)+m)/lambda)^(-alpha)
}

pdf <- function(x, alpha, lambda, m,b,k){
  (alpha/lambda)*(G(x,b,k)+m+(x*g(x,b,k)))*(1+x*(G(x,b,k)+m)/lambda)^(-alpha-1)
}


a = c(1.3,1.3,2,2.5)
k = c(0.2, 0.5, 0.5, 0.7)
alpha <- c(2,1.8,2,2.5)
lambda <- c(1,0.8,0.9,1)
m <- c(0.5, 0.005, 0.1, 0.15)
x <- seq(0, 3, length.out = 10000)
col = c("blue", "red", "green", "black")
plot(x, y <- pdf(x, alpha[1], lambda[1]  , m[1],a[1],k[1]), type = 'l', col = col[1], ylim = c(0, 1), xlab = "x", ylab = "fonction de densité",lwd=3,main = "SHL-Gumbel")
legend("topright", legend = c(paste("m =", m,",a=",a, ",k=",k, ",\u03BB=", lambda, ",\u03B1=",alpha)), col = c("blue", "red", "green", "black"), lty =c(1,2,3,4), cex = 0.8,lwd = 2)

i=2
y <- pdf(x, alpha[i], lambda[i], m[i],a[i],k[i])
lines(x, y ,lwd=3, col=col[i],lty=2)

y <- pdf(x, alpha[i+1], lambda[i+1], m[i+1],a[i+1],k[i+1])
lines(x, y ,lwd=3, col=col[i+1],lty=3)

y <- pdf(x, alpha[i+2], lambda[i+2], m[i+2],a[i+2],k[i+2])
lines(x, y ,lwd=3, col=col[i+2],lty=4)


#Gompertz avec dérive

G <- function(x,a,k){
  exp(-a*exp(-k*x))
}

g <- function(x,a,k){
  a*k*exp(-k*x)*G(x,a,k)
}

cdf <- function(x,alpha, lambda,m,a,k){
  1-(1+x*(G(x,a, k)+m)/lambda)^(-alpha)
}

pdf <- function(x, alpha, lambda, m,a,k){
  (alpha/lambda)*(G(x,a,k)+m+(x*g(x,a,k)))*(1+x*(G(x,a,k)+m)/lambda)^(-alpha-1)
}


a = c(5,4.5,10,6)
k = c(1.2, 5,4,1.1)
alpha <- c(3, 2.5,3,2)
lambda <- c(1.1,1.5,2,1.1)
m <- c(0.02, 0.05, 0.06, 0.08)
x <- seq(0, 2., length.out = 10000)


col = c("blue", "red", "green", "black")

plot(x, y <- pdf(x, alpha[1], lambda[1]  , m[1],a[1],k[1]), type = 'l', ylim = c(0, 1.5), xlab = "x",main = "SHL-Gompertz",
     ylab = "fonction de densité", col=col[1],lwd=2)


for (i in 2:length(m)){
  y <- pdf(x, alpha[i], lambda[i], m[i],a[i],k[i])
  lines(x, y, col = col[i],lwd=2,lty=i)
}
legend("topright", legend = c(paste("m =",m,", a=",a, ", k=",k, ",\u03BB=", lambda, ", \u03B1=",alpha)), col = c("blue", "red", "green", "black"), 
       lty = c(1,2,3,4), cex = 0.8,lwd=2)


#Loi de Rayleigh

G <- function(x,sigma){
  1-exp(-((x^2)/2*(sigma)^2))
}

g <- function(x, sigma){
  (x/sigma^2)*exp(-((x^2)/2*(sigma)^2))
}

cdf <- function(x, sigma, alpha, lambda, m){
  1-(1+x*(G(x, sigma)+m)/lambda)^(-alpha)
}

pdf <- function(x,sigma, alpha, lambda, m){
  (alpha/lambda)*(G(x,sigma)+m+x*g(x, sigma))*(1+x*(G(x, sigma)+m)/lambda)^(-alpha-1)
}

alpha <- c(0.6,1.5,0.7,1.2)
lambda <- c(0.5,0.2,0.1,0.5)
m <- c( 0.05, 0.025, 0.01, 0.02)
x <- seq(0, 1., length.out = 10000)
sigma = c(4,5,3,2)



plot(x, y <- pdf(x,sigma[1], alpha[1], lambda[1]  , m[1]), type = 'l', ylim = c(0, 1.5), xlab = "x", ylab = "fonction de densité",main = "SHL-Rayleigh",,col= col[1],lwd=2)
legend("topright", legend = c(paste("m =", m,", \u03c3=",sigma, ",\u03BB=", lambda, ",\u03B1=",alpha)), col = c("blue", "red", "green", "black"), lty = c(1,2,3,4),lwd=2, cex = 0.8)

for (i in 2:length(m)){
  y <- pdf(x,sigma[i], alpha[i], lambda[i], m[i])
  lines(x, y, col = col[i],lwd=2,lty=i)
}


#Loi de Burr

G <- function(x,c,k){
  1-(1+x^c)^(-k)
}

g <- function(x, c,k){
  c*k*x^(c-1)/(1+x^c)^(k+1)
}

cdf <- function(x, c,k, alpha, lambda, m){
  1-(1+x*(G(x, c,k)+m)/lambda)^(-alpha)
}

pdf <- function(x,c,k, alpha, lambda, m){
  (alpha/lambda)*(G(x,c,k)+m+x*g(x,c,k))*(1+x*(G(x,c,k)+m)/lambda)^(-alpha-1)
}

alpha <- c(10,1.8,1.3,2)
lambda <- c(1.25,1.25,5,9)
m <- c(0.5, 0.75, 0.5, 1)
x <- seq(0, 1.5, length.out = 10000)
c= c(10.3,10,10.5,9)
k=c(11.5,11,10,11.5)


plot(x, y <- pdf(x,c[i],k[i], alpha[1], lambda[1]  , m[1]), type = 'l', ylim = c(0, 2), xlab = "x",main = "SHL-Burr", ylab = "fonction de densité",col= col[1],lwd=2)
legend("topright", legend = c(paste("m =", m,",c=",c, ",k=",k,",\u03BB=", lambda, ",\u03B1=",alpha)), col = c("blue", "red", "green", "black"), lty = c(1,2,3,4), cex = 0.8,lwd=2)

for (i in 2:length(m)){
  y <- pdf(x,c[i],k[i], alpha[i], lambda[i], m[i])
  lines(x, y, col = col[i],lwd=2,lty=i)
}


