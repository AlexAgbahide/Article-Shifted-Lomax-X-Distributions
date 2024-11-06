

w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


f <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}



# Générer des données
set.seed(123)  # Pour la reproductibilité
n <- 1000  # Nombre d'observations

alpha <- 3.59
lambda <- 9.18
k <- 4.25
m <- 0
a <-1.46
# Faire varier q de 0.60 à 0.95
q <- seq(0.6, 0.95, by = 0.05)
TVAR <-  matrix(NA, nrow = 1, ncol = length(q));

for (i in 1) {
  for (j in 1:length(q)) {
    qlmax_weibull  <- function(q, alpha, lambda, m, a, k) {
      cdf_value <- function(x) {
        cdf(x, alpha, lambda, m, a, k)
      }

      lower <- 0
      upper <- 100

      result <- optimize(function(x) abs(cdf_value(x) - q), interval = c(lower, upper))$minimum

      return(result)
    }
    VAR <- qlmax_weibull(q[j],alpha[i], lambda[i], m[i], a[i], k[i])
    M = function (x){
      x*f(x, alpha[i], lambda[i], m[i], a[i], k[i])
    }
    integrale <- integrate(M, VAR, Inf)
    TVAR[i,j] <- integrale$value/(1-q[j])
  }
}
colors <- rainbow(4)

# Tracer la première ligne avec 'plot()'
plot(q, TVAR[1, ], type = "o", col = colors[1], ylim = range(TVAR), xlab = "q", ylab = "TVaR")
