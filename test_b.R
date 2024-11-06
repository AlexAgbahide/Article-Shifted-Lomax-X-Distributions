library(readxl)
finance_ins <- read_excel("E:/Users/Alex/OneDrive/Documents/gmm3/finance_ins.xlsx", 
                          col_names = FALSE)

data =sort(finance_ins$...1)


x=data

hist(data, freq = FALSE ,col = "transparent", xlab = "x",ylim = c(0,0.25),main="",breaks = 15)

w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}
##Test de normalité

shapiro.test(data)

# #Test de Kolmogorov-Smirnov  utilisé pour comparer une distribution empirique (tes 
# données) à une distribution théorique (par exemple, SHL-X) ou pour comparer deux 
# distributions empiriques entre elles

ks.test(x, "cdf", 3.59 ,9.18,  0.0, 1.46,4.25)

#Test de Anderson-Darling
library(nortest)
library(ADGofTest)
ad_result <- ad.test(data, cdf, 3.59 ,9.18,  0.0, 1.46,4.25)

# Afficher les résultats
print(ad_result)

# Test du Chi-carré compare les fréquences observées et attendues,
#et produit une p-value indiquant si les deux distributions sont similaires.
bins <- seq(min(data), max(data), length.out = 10)  # Divise les données en 10 groupes

observed <- hist(data, breaks = bins, plot = FALSE)$counts


# Calcul des fréquences attendues selon le modèle SHL-X
expected <- diff(cdf(bins, 3.59 ,9.18,  0.0, 1.46,4.25)) * length(data)

# Test du chi-carré

chisq_test <- chisq.test(observed, p = expected / sum(expected))
print(chisq_test)

library(goftest)
cvm.test(data,"cdf", 3.59 ,9.18,  0.0, 1.46,4.25)


##QQplot

qshl  <- function(q, alpha, lambda, m, a, k) {
  cdf_value <- function(x) {
    cdf(x, alpha, lambda, m, a, k)
  }
  
  lower <- 0
  upper <- 100
  
  result <- optimize(function(x) abs(cdf_value(x) - q), interval = c(lower, upper))$minimum
  
  return(result)
}
params <- c(3.59 ,9.18,  0.0, 1.46,4.25)
n <- length(data)

probabilities <- (1:n) / (n + 1)  # Probabilités pour chaque quantile
theoretical_quantiles <- sapply(probabilities, qshl,3.59 ,9.18,  0.0, 1.46,4.25)

# Calculer les quantiles empiriques des données
empirical_quantiles <- sort(data)

# Créer le QQ-plot
plot(theoretical_quantiles, empirical_quantiles, 
     main = "QQ-plot avec Distribution Théorique SHL-X",
     xlab = "Quantiles Théoriques", 
     ylab = "Quantiles Empiriques",
     pch = 19, col = "blue")
abline(0, 1, col = "red")  # Ajouter la ligne y = x
