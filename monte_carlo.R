
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}



qlmax_weibull <- function(p, alpha,lambda, m, a, k) {
  # Function to calculate the CDF value at different quantiles
  cdf_value <- function(x) {
    cdf(x, alpha,lambda, m, a, k)
  }
  
  # Define the search interval based on the probability value
  lower <- 0  # Lower bound for search
  upper <- 100   # Upper bound for search
  
  # Use optimize function to find the quantile value
  result <- optimize(function(x) abs(cdf_value(x) - p), interval = c(lower, upper))$minimum
  
  return(result)
}


rlom_weibull <- function(n, alpha,lambda, m, a, k) {
  # Function to calculate the quantile value
  quantile_value <- function(p) {
    qlmax_weibull(p,  alpha,lambda, m, a, k)
  }
  
  # Generate random uniform values between 0 and 1
  u <- runif(n)
  
  # Calculate quantile values for the random uniform values
  sample <- sapply(u, quantile_value)
  
  
  return(sample)
}


params_fTLCAR_weibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, alpha, lambda, m,a, k)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    
    # Check parameter constraints
    if (alpha <= 0 || lambda <= 0 || m <= 0 || a <= 0 || k <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.5, 0.1, 0.5,0.2)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0.0001, 0.0001, 0.0001, 0.0001,0.0001),
                                    control = list(trace = FALSE),
                                    upper = c(Inf,Inf,Inf,Inf,Inf),
                                    hessian = TRUE))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

calculate_metrics <- function(n, alpha,lambda, m,a,k, num_replications) {
  Meann <- matrix(0, nrow = num_replications, ncol = 5)
  biases <- matrix(0, nrow = num_replications, ncol = 5)
  squared_errors <- matrix(0, nrow = num_replications, ncol = 5)
  variances <- matrix(0, nrow = num_replications, ncol = 5)
  
  for (i in 1:num_replications) {
    # Generate a random sample
    data <- rlom_weibull (n,alpha,lambda, m,a,k)
    
    # Calculate MLE
    mle_params <- params_fTLCAR_weibull(data)
    
    # Calculate bias
    bias <- mle_params - c(alpha,lambda, m,a,k)
    
    # Calculate squared error
    squared_error <- (mle_params - c(alpha,lambda, m,a,k))^2
    
    # Store results
    Meann[i,]=mle_params
    biases[i,] <- bias
    squared_errors[i,] <- squared_error
    
  }
  mean_Meann=colMeans(Meann)
  mean_biases <- colMeans(biases)
  mean_squared_errors <- sqrt(colMeans(squared_errors))
  
  return(data.frame(M_MLSE=mean_Meann, M_Squared_Error = mean_squared_errors ,M_Bias = mean_biases))
}

alpha = 1.5
lambda = 2
m = 0.004
a = 1.8
k=0.8


# Tailles d'échantillon 
sample_sizes <- c(100, 250, 400, 500,1000)

n_simulations=300

# Calculate metrics for each sample size
for (n in sample_sizes) {
  metrics <- calculate_metrics(n,alpha,lambda, m,a,k, n_simulations)
  cat("Sample Size:", n, "\n")
  print(metrics)
  cat("\n")
}


alpha = 0.5
lambda =1
m = 0.005
a = 2
k=0.5

# Calculate metrics for each sample size
for (n in sample_sizes) {
  metrics <- calculate_metrics(n,alpha,lambda, m,a,k, n_simulations)
  cat("Sample Size:", n, "\n")
  print(metrics)
  cat("\n")
}

