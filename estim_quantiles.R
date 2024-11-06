w <- function(x, a, k) {
  1 - exp(-(x / k) ^ a)
}

cdf <- function(x, alpha, lambda, m, a, k) {
  1 - (1 + x * (w(x, a, k) + m) / lambda) ^ -alpha
}

pdf <- function(x, alpha, lambda, m, a, k) {
  (alpha / lambda) * (w(x, a, k) + m + x * (a / k) * (x / k) ^ (a - 1) * exp(-(x / k) ^ a)) * (1 + x * ((w(x, a, k) + m) / lambda)) ^ -(alpha + 1)
}

qlmax_weibull <- function(p, alpha, lambda, m, a, k) {
  cdf_value <- function(x) {
    cdf(x, alpha, lambda, m, a, k)
  }
  
  lower <- 0
  upper <- 100
  
  result <- optimize(function(x) abs(cdf_value(x) - p), interval = c(lower, upper))$minimum
  
  return(result)
}

# Paramètres
m_values <- c(0.25, 1.2)
a <- 1.2
k <- 1.5 
alpha_values <- c(1.5, 2.7, 4, 5.4)
lambda_values <- c(0.5, 1, 1.7, 2.2)
p <- c(0.25, 0.5, 0.75)

# Initialiser une liste pour stocker les résultats
results <- list()

# Boucles pour chaque combinaison de m, alpha et lambda
for (m in m_values) {
  for (alpha in alpha_values) {
    for (lambda in lambda_values) {
      quantiles <- sapply(p, function(prob) qlmax_weibull(prob, alpha, lambda, m, a, k))
      BS <- (quantiles[3] + quantiles[1] - 2 * quantiles[2]) / (quantiles[3] - quantiles[1])
      MK <- (qlmax_weibull(7/8, alpha, lambda, m, a, k) - qlmax_weibull(5/8, alpha, lambda, m, a, k) + qlmax_weibull(3/8, alpha, lambda, m, a, k) - qlmax_weibull(1/8, alpha, lambda, m, a, k)) / (qlmax_weibull(3/4, alpha, lambda, m, a, k) - qlmax_weibull(1/4, alpha, lambda, m, a, k))
      results[[paste(m, alpha, lambda, sep = "_")]] <- c(quantiles, BS, MK)
    }
  }
}

# Créer le tableau des résultats
table_data <- matrix(nrow = 0, ncol = 12)
colnames(table_data) <- c("λ", "α", "Q1", "Q2", "Q3", "BS", "MK", "Q1", "Q2", "Q3", "BS", "MK")

for (lambda in lambda_values) {
  for (alpha in alpha_values) {
    row <- c(lambda, alpha)
    for (m in m_values) {
      values <- results[[paste(m, alpha, lambda, sep = "_")]]
      row <- c(row, values)
    }
    table_data <- rbind(table_data, row)
  }
}

# Afficher le tableau
print("Table 1: Tableau des valeurs pour différentes valeurs de m, α, λ, a = 1.2 et k = 1.5")
print(table_data)
