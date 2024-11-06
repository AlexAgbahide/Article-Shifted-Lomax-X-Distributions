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


VAR <- matrix(NA, nrow = 1, ncol = length(q))

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
    VAR[i,j] <- qlmax_weibull(q[j],alpha[i], lambda[i], m[i], a[i], k[i])
  }
}


# Utiliser une palette de couleurs
colors <- rainbow(4)

# Tracer la première ligne avec 'plot()'
plot(q, VAR[1, ], type = "o", col = colors[1], xlab = "q", ylab = "VAR")

# # Ajouter les autres lignes avec 'lines()'
# for (i in 2:4) {
#   lines(q, VAR[i, ], type = "o", col = colors[i])
# }

#legend("topleft", legend = paste("\u03BB =", lambda, ", \u03B1 =", alpha, ", k =", k, ",m =", m, ",a =",a),
 #      col = colors, lty = 1, pch = 1, cex = 0.8, bty = "n")

# # Tracer un nuage de points pour les valeurs VAR en fonction de q
# plot(q, VAR, type = "p", pch = 16, col = "black", xlab = "q", ylab = "VaR")
#
# # Ajouter une ligne reliant les points
# lines(q, VAR, type = "l", col = "blue")
