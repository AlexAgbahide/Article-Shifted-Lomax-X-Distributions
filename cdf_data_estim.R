
library(readr)



library(readxl)
Portlofio <- read_excel("C:/Users/Alex/Downloads/Portlofio.xlsx")
data = sort(log(Portlofio$Capital))

x = data

hist(data, freq = FALSE,ylim=c(0,0.8), col = "transparent", xlab = "x", main = "",ylab="fdp")


##########################################LOMAX-GWEIBULL#############

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*b(x,a,k)*(beta-log(1-B(x,a,k)))^(-(alpha+1))/(1-B(x,a,k))
}


log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_L1weibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=2,lwd=2,lty=2)



# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


# Estimation des paramètres de la distribution Lomax-Weibull




###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=2,lty=2)


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))






#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.70, 0.8,5.1,0.5,0.391)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=2,lty=2)


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




####Mc donald modified weibull
g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.01, 0.01,0.01,.01,.01,0.01)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5],estimated_params[6]),col=6,lwd=2,lty=2)



# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <-function(x,a,b,c,lambda,alpha){
  term1(x,a,b,c,lambda,alpha)*term2(x,a,b,c,lambda,alpha)*term3(x,a,b,c,lambda,alpha)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(1.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=7,lwd=2,lty=2)


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


###Exponentiated weibull weibull

pdf <-function(x,alpha,lambda,m,a,k){
  ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
     (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=8,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=2,lty=1)


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))





legend("topright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex=0.8)





##########################################LOMAX-GWEIBULL#############

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*b(x,a,k)*(beta-log(1-B(x,a,k)))^(-(alpha+1))/(1-B(x,a,k))
}


log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_L1weibull(data)
estimated_params


plot(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                    estimated_params[3],estimated_params[4]),col=2,lwd=1,lty=2,type="l",ylab ="FDC",xlab="x")



# Estimation des paramètres de la distribution Lomax-Weibull



###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}
cdf <-  function(x, alpha,beta,lambda,gamma){
  1-exp(-alpha*G(x,lambda,gamma)^beta)
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=1,lty=2)


#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

cdf <- function(x, c,d,v,alpha,beta){
  1-((1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c))
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.70, 0.8,5.1,0.5,0.391)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=1,lty=2)



####Mc donald modified weibull

g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}


# Fonction de répartition cumulative (CDF)
cdf <- function(x, alpha, gamma, beta, a, b, c) {
  integrate(function(t) pdf(t, alpha, gamma, beta, a, b, c), lower = 0, upper = x)$value
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.01, 0.01,0.01,.01,.01,0.01)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_mdweibull(data)
estimated_params

# Tracer la CDF avec les paramètres estimés
lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5], estimated_params[6])),
      type = "l", col = 6, lwd = 1, lty = 2)


###beta exponentiated weibull



g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <-function(x,a,b,c,lambda,alpha){
  term1(x,a,b,c,lambda,alpha)*term2(x,a,b,c,lambda,alpha)*term3(x,a,b,c,lambda,alpha)
}


# Define the CDF function
cdf <- function(x, a, b, c, lambda, alpha) {
  result <- tryCatch({
    integrate(function(t) pdf(t, a, b, c, lambda, alpha), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}

log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(1.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}



estimated_params <- params_beweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])), col = 7, lwd = 1, lty = 2)





###Exponentiated weibull weibull


pdf <- function(x,alpha,lambda,m,a,k) {
  # Check for invalid parameter values
  if (any(c(x,alpha,lambda,m,a,k) <= 0)) {
    return(0)  # Return 0 for invalid parameters to avoid non-finite values
  }
  
  # Compute the PDF value
  pdf_value <- ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
                  (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x,alpha,lambda,m,a,k) {
  result <- tryCatch({
    integrate(function(t) pdf(t, alpha,lambda,m,a,k), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])),col=8,lwd=1,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)


##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=1,lty=1)



legend("bottomright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex=0.8)









########################################
library(readxl)
finance_ins <- read_excel("C:/Users/Alex/OneDrive/Documents/gmm3/finance_ins.xlsx", 
                          col_names = FALSE)

data =sort(finance_ins$...1)


x=data

hist(data, freq = FALSE ,col = "transparent", ylab ="fdp", xlab = "x",ylim = c(0,0.25),main="",breaks = 15)
#Weibull 2

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*b(x,a,k)*(beta-log(1-B(x,a,k)))^(-(alpha+1))/(1-B(x,a,k))
}


log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_L1weibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=2,lwd=2,lty=2)



# Estimation des paramètres de la distribution Lomax-Weibull


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.70, 0.8,5.1,0.5,0.391)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



####Mc donald modified weibull
g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.1,.1,.1,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5],estimated_params[6]),col=6,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <-function(x,a,b,c,lambda,alpha){
  term1(x,a,b,c,lambda,alpha)*term2(x,a,b,c,lambda,alpha)*term3(x,a,b,c,lambda,alpha)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(1.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=7,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




###Exponentiated weibull weibull

pdf <-function(x,alpha,lambda,m,a,k){
  ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
     (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=8,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=2,lty=1)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))




legend("topright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex = 0.8)


##########################################LOMAX-GWEIBULL#############

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*b(x,a,k)*(beta-log(1-B(x,a,k)))^(-(alpha+1))/(1-B(x,a,k))
}


log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_L1weibull(data)
estimated_params


plot(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                    estimated_params[3],estimated_params[4]),col=2,lwd=1,lty=2,type="l",ylab ="FDC",xlab="x")



# Estimation des paramètres de la distribution Lomax-Weibull



###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}
cdf <-  function(x, alpha,beta,lambda,gamma){
  1-exp(-alpha*G(x,lambda,gamma)^beta)
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=1,lty=2)


#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

cdf <- function(x, c,d,v,alpha,beta){
  1-((1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c))
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.70, 0.8,5.1,0.5,0.391)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=1,lty=2)



####Mc donald modified weibull
g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}
# Fonction de densité de probabilité (PDF)
pdf <- function(x, alpha, gamma, beta, a, b, c) {
  term1(x, alpha, gamma, beta, a, b, c) * term2(x, alpha, gamma, beta, a, b, c) * term3(x, alpha, gamma, beta, a, b, c)
}

# Fonction de répartition cumulative (CDF)
cdf <- function(x, alpha, gamma, beta, a, b, c) {
  integrate(function(t) pdf(t, alpha, gamma, beta, a, b, c), lower = 0, upper = x)$value
}

log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.1,.1,.1,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params

# Tracer la CDF avec les paramètres estimés
lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5], estimated_params[6])),
      type = "l", col = 6, lwd = 1, lty = 2)


###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <- function(x, a, b, c, lambda, alpha) {
  
  
  # Compute the PDF value
  pdf_value <- term1(x, a, b, c, lambda, alpha) * term2(x, a, b, c, lambda, alpha) * term3(x, a, b, c, lambda, alpha)
  
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x, a, b, c, lambda, alpha) {
  result <- tryCatch({
    integrate(function(t) pdf(t, a, b, c, lambda, alpha), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(1.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])), col = 7, lwd = 1, lty = 2)





###Exponentiated weibull weibull


pdf <- function(x,alpha,lambda,m,a,k) {
  # Check for invalid parameter values
  if (any(c(x,alpha,lambda,m,a,k) <= 0)) {
    return(0)  # Return 0 for invalid parameters to avoid non-finite values
  }
  
  # Compute the PDF value
  pdf_value <- ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
                  (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x,alpha,lambda,m,a,k) {
  result <- tryCatch({
    integrate(function(t) pdf(t, alpha,lambda,m,a,k), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])),col=8,lwd=1,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)


##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=1,lty=1)



legend("bottomright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex=0.8)







########################################
library(readr)
Unemployment_Insurance_Data_July_2008_to_April_2013 <- read_delim("C:/Users/Alex/Downloads/Unemployment_Insurance_Data_-_July_2008_to_April_2013.csv", 
                                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
data =sort(Unemployment_Insurance_Data_July_2008_to_April_2013$`# of First UI Checks issued - Ex. Fed Employees`)


x=data

hist(data, freq = FALSE ,col = "transparent", ylab = "fdp", xlab = "x",main="", ylim = c(0,0.025),breaks = 20)
#Weibull 2

#########LOMAX-GWEIBULL

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*a*((1/k)^a)*(x^(a-1))/(beta+(x/k)^a)^(alpha+1)
}


log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}


estimated_params <- params_L1weibull(data)
estimated_params

lines(data, y <- (pdf(data,estimated_params[1] ,estimated_params[2], estimated_params[3] ,estimated_params[4])),col=2,lwd=2,lty=2)


# Estimation des paramètres de la distribution Lomax-Weibull


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(01, 01,01,01,0.01)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,00,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



####Mc donald modified weibull

g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.01,0.1,.1,.1,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5],estimated_params[6]),col=6,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <-function(x,a,b,c,lambda,alpha){
  term1(x,a,b,c,lambda,alpha)*term2(x,a,b,c,lambda,alpha)*term3(x,a,b,c,lambda,alpha)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.1, .1,.1,.1,.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=7,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))

##Exponentiated weibull weibull

pdf <-function(x,alpha,lambda,m,a,k){
  ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
     (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=8,lwd=2,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))


#######MA LOMAX-WEIBULL

w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
  start_params <- c(0.1, 1, 0.1, 0.1,2)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0.000, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params


lines(data, y <- pdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=2,lty=1)


# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)

# Calcul de l'AIC
n <- length(data)
k <- length(estimated_params)
AIC_value <- -2 * log_likelihood_value + 2 * k

# Calcul du BIC
BIC_value <- -2 * log_likelihood_value + k * log(n)

# Calcul du HQIC
HQIC_value <- -2 * log_likelihood_value + 2 * k * log(log(n))

# Affichage des résultats
print(paste("Log-vraisemblance :", log_likelihood_value))
print(paste("AIC :", AIC_value))
print(paste("BIC :", BIC_value))
print(paste("HQIC :", HQIC_value))



legend("topright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex = 0.8)


##########################################LOMAX-GWEIBULL#############

B <- function(x, a , k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*a*((1/k)^a)*(x^(a-1))/(beta+(x/k)^a)^(alpha+1)
}



log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}



estimated_params <- params_L1weibull(data)
estimated_params


plot(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                    estimated_params[3],estimated_params[4]),col=2,lwd=1,lty=2,type="l",ylab ="FDC",xlab="x",ylim = c(0,1))



# Estimation des paramètres de la distribution Lomax-Weibull



###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}
cdf <-  function(x, alpha,beta,lambda,gamma){
  1-exp(-alpha*G(x,lambda,gamma)^beta)
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=1,lty=2)


#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

cdf <- function(x, c,d,v,alpha,beta){
  1-((1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c))
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(01, 01,01,01,0.01)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=1,lty=2)



####Mc donald modified weibull



g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}

# Fonction de répartition cumulative (CDF)
cdf <- function(x, alpha, gamma, beta, a, b, c) {
  integrate(function(t) pdf(t, alpha, gamma, beta, a, b, c), lower = 0, upper = x)$value
}



log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.01,0.1,.1,.1,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params









# Tracer la CDF avec les paramètres estimés
lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5], estimated_params[6])),
      type = "l", col = 6, lwd = 1, lty = 2)


###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <- function(x, a, b, c, lambda, alpha) {
  
  
  # Compute the PDF value
  pdf_value <- term1(x, a, b, c, lambda, alpha) * term2(x, a, b, c, lambda, alpha) * term3(x, a, b, c, lambda, alpha)
  
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x, a, b, c, lambda, alpha) {
  result <- tryCatch({
    integrate(function(t) pdf(t, a, b, c, lambda, alpha), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.1, .1,.1,.1,.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])), col = 7, lwd = 1, lty = 2)





###Exponentiated weibull weibull


pdf <- function(x,alpha,lambda,m,a,k) {
  # Check for invalid parameter values
  if (any(c(x,alpha,lambda,m,a,k) <= 0)) {
    return(0)  # Return 0 for invalid parameters to avoid non-finite values
  }
  
  # Compute the PDF value
  pdf_value <- ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
                  (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x,alpha,lambda,m,a,k) {
  result <- tryCatch({
    integrate(function(t) pdf(t, alpha,lambda,m,a,k), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])),col=8,lwd=1,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)


##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
  start_params <- c(0.1, 1, 0.1, 0.1,2)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=2,lty=1)



legend("bottomright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex=0.8)

B <-  function(x, a,k){
  1-exp(-(x/k)^(a))
}


b <- function(x, a,k){
  (a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a)))
}

cdf <- function(x,a,k,beta,alpha){
  1-(beta^alpha)*(beta-log(1-B(x,a,k)))^(-alpha)
}

pdf <-function(x,a,k,beta, alpha){
  alpha*(beta^alpha)*a*((1/k)^a)*(x^(a-1))/(beta+(x/k)^a)^(alpha+1)
}



log_likelihood <- function(parameters, data) {
  
  a <- parameters[1]
  k <- parameters[2]
  beta <- parameters[3]
  alpha <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
  
  return(-log_lik)
}



params_L1weibull <- function(data) {
  log_likelihood <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    
    
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,k,beta, alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    a <- parameters[1]
    k <- parameters[2]
    beta <- parameters[3]
    alpha <- parameters[4]
    
    # Check parameter constraints
    if (a <= 0 || k <= 0 || beta <= 0 || alpha <= 0 ) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1, 0.5,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}



estimated_params <- params_L1weibull(data)
estimated_params


plot(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                    estimated_params[3],estimated_params[4]),col=2,lwd=1,lty=2,type="l",ylab ="cdf",xlab="x",ylim = c(0,1))



# Estimation des paramètres de la distribution Lomax-Weibull



###Weibull weibull

G<- function(x,lambda,gamma){
  exp(lambda*x^gamma)-1
}
cdf <-  function(x, alpha,beta,lambda,gamma){
  1-exp(-alpha*G(x,lambda,gamma)^beta)
}

pdf <- function(x, alpha,beta,lambda,gamma){
  alpha*beta*lambda*gamma*(x^(gamma-1))*G(x,lambda,gamma)^(beta-1)*
    exp(-alpha*G(x,lambda,gamma)^beta+lambda*(x)^gamma)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  beta <- parameters[2]
  lambda <- parameters[3]
  gamma <- parameters[4]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
  
  return(-log_lik)
}


params_wweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,beta,lambda,gamma)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    beta <- parameters[2]
    lambda <- parameters[3]
    gamma <- parameters[4]
    
    # Check parameter constraints
    if (alpha <= 0 ||beta <=0 || lambda <= 0||gamma <=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.1,0.10,0.10)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_wweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4]),col=3,lwd=1,lty=2)


#########Odd burr XII Exponentiated weibull

term1 <- function(x, c,d,v,alpha,beta){
  c*d*v*alpha*beta*(x^(alpha-1))*(exp(-(beta*(x^alpha))))*((1-(exp(-(beta*(x^alpha)))))^(2*v-1))
}

term2 <-function(x, c,d,v,alpha,beta){
  (2-(1-(exp(-(beta*(x^alpha)))))^v)*(1-(1-(exp(-(beta*(x^alpha)))))^v)^(-2)
}

term3 <-function(x, c,d,v,alpha,beta){
  (((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d-1)
}

term4 <- function(x, c,d,v,alpha,beta){
  (1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c-1)
}

cdf <- function(x, c,d,v,alpha,beta){
  1-((1+(((1-exp(-(beta*(x^alpha))))^(2*v))/(1-(1-exp(-(beta*(x^alpha))))^v))^(d))^(-c))
}

pdf <- function(x, c,d,v,alpha,beta){
  term1(x, c,d,v,alpha,beta)*term2(x, c,d,v,alpha,beta)*term3(x, c,d,v,alpha,beta)*
    term4(x, c,d,v,alpha,beta)
}


log_likelihood <- function(parameters, data) {
  
  c <- parameters[1]
  d <- parameters[2]
  v <- parameters[3]
  alpha <- parameters[4]
  beta <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,  c,d,v,alpha,beta)))
  
  return(-log_lik)
}


params_oxweibull <- function(data) {
  log_likelihood <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data, c,d,v,alpha,beta)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    c <- parameters[1]
    d <- parameters[2]
    v <- parameters[3]
    alpha <- parameters[4]
    beta <- parameters[5]
    
    # Check parameter constraints
    if (a<= 0 ||beta <=0 || lambda <= 0||d<=0||v<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(01, 01,01,01,0.01)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_oxweibull(data)
estimated_params

lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=4,lwd=1,lty=2)



####Mc donald modified weibull



g<- function(x,alpha,gamma,beta){
  exp(-(alpha*x)-gamma*(x^beta))
}

term1 <- function(x,alpha,gamma,beta,a,b,c){
  (c/beta(a,b))*(alpha+gamma*beta*(x^(beta-1)))*g(x,alpha,gamma,beta)
}

term2 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-g(x,alpha,gamma,beta))^(a*c-1))
}

term3 <-function(x,alpha,gamma,beta,a,b,c){
  ((1-(1-g(x,alpha,gamma,beta))^c)^(b-1))
}

pdf <-function(x,alpha,gamma,beta,a,b,c){
  term1(x,alpha,gamma,beta,a,b,c)*term2(x,alpha,gamma,beta,a,b,c)*term3(x,alpha,gamma,beta,a,b,c)
}

# Fonction de répartition cumulative (CDF)
cdf <- function(x, alpha, gamma, beta, a, b, c) {
  integrate(function(t) pdf(t, alpha, gamma, beta, a, b, c), lower = 0, upper = x)$value
}



log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  gamma <- parameters[2]
  beta <- parameters[3]
  a <- parameters[4]
  b <- parameters[5]
  c <- parameters[6]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
  
  return(-log_lik)
}


params_mdweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,gamma,beta,a,b,c)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[1]
    gamma <- parameters[2]
    beta <- parameters[3]
    a <- parameters[4]
    b <- parameters[5]
    c <- parameters[6]
    
    # Check parameter constraints
    if (alpha<= 0 ||beta <=0 || gamma <= 0||a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(0.1, 0.01,0.1,.1,.1,0.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0,0.000,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_mdweibull(data)
estimated_params









# Tracer la CDF avec les paramètres estimés
lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5], estimated_params[6])),
      type = "l", col = 6, lwd = 1, lty = 2)


###beta exponentiated weibull


g<- function(x,lambda,c){
  exp(-(lambda*x)^c)
}

term1 <- function(x,a,b,c,lambda,alpha){
  ((alpha*c*(lambda^c)/beta(a,b))*(x^(c-1))*g(x,lambda,c))
}

term2 <-function(x,a,b,c,lambda,alpha){
  ((1-g(x,lambda,c))^(alpha*a-1))
}

term3 <-function(x,a,b,c,lambda,alpha){
  ((1-(1-g(x,lambda,c))^alpha)^(b-1))
}

pdf <- function(x, a, b, c, lambda, alpha) {
  
  
  # Compute the PDF value
  pdf_value <- term1(x, a, b, c, lambda, alpha) * term2(x, a, b, c, lambda, alpha) * term3(x, a, b, c, lambda, alpha)
  
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x, a, b, c, lambda, alpha) {
  result <- tryCatch({
    integrate(function(t) pdf(t, a, b, c, lambda, alpha), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[5]
  lambda <- parameters[4]
  a <- parameters[1]
  b <- parameters[2]
  c <- parameters[3]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
  
  return(-log_lik)
}


params_beweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,a,b,c,lambda,alpha)))
    
    return(-log_lik)
  }
  
  # Constraints
  constraint <- function(parameters) {
    alpha <- parameters[5]
    lambda <- parameters[4]
    a <- parameters[1]
    b <- parameters[2]
    c <- parameters[3]
    
    # Check parameter constraints
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||c<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.1, .1,.1,.1,.1)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_beweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])), col = 7, lwd = 1, lty = 2)





###Exponentiated weibull weibull


pdf <- function(x,alpha,lambda,m,a,k) {
  # Check for invalid parameter values
  if (any(c(x,alpha,lambda,m,a,k) <= 0)) {
    return(0)  # Return 0 for invalid parameters to avoid non-finite values
  }
  
  # Compute the PDF value
  pdf_value <- ((alpha*m*a/(k*lambda^m))*((x/k)^(a*m-1))*exp(-(lambda^(-m))*(x/k)^(a*m))*
                  (1-exp(-(lambda^(-m))*((x/k)^a)^m))^(alpha-1))
  return(pdf_value)
}

# Define the CDF function
cdf <- function(x,alpha,lambda,m,a,k) {
  result <- tryCatch({
    integrate(function(t) pdf(t, alpha,lambda,m,a,k), lower = 0, upper = x)$value
  }, error = function(e) {
    warning("Integration failed: ", e$message)
    return(NA)
  })
  
  return(result)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
  
  return(-log_lik)
}


params_ewweibull <- function(data) {
  log_likelihood <- function(parameters) {
    alpha <- parameters[1]
    lambda <- parameters[2]
    m <- parameters[3]
    a <- parameters[4]
    k <- parameters[5]
    # Calculate the log-likelihood value for the data
    log_lik <- sum(log(pdf(data,alpha,lambda,m,a,k)))
    
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
    if (alpha<= 0 ||lambda <=0 || a<=0||b<=0||m<=0) {
      stop("Invalid parameter values")  # Throw an error for infeasible parameter values
    }
    
    return(NULL)
  }
  
  # Estimate parameters using constrained optimization
  start_params <- c(.01,0.01,0.03,0.04,.005)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0,0,0,0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_ewweibull(data)
estimated_params



lines(data, sapply(data, function(x) cdf(x, estimated_params[1], estimated_params[2],
                                         estimated_params[3], estimated_params[4], estimated_params[5])),col=8,lwd=1,lty=2)

# Calcul du log-vraisemblance
log_likelihood_value <- -log_likelihood(estimated_params, data)


##########################MA LOMAX-WEIBULL############################
w <- function(x, a , k){
  1-exp(-(x/k)^(a))
}

cdf <- function(x, alpha,lambda, m, a, k){
  1-(1+x*(w(x,a, k)+m)/lambda)^(-alpha)
}


pdf <- function(x, alpha,lambda, m,a,k){
  (alpha/lambda)*(w(x,a,k)+m+x*(a/k)*((x/k)^(a-1))*(exp(-(x/k)^(a))))*(1+x*((w(x,a, k)+m)/lambda))^(-alpha-1)
}


log_likelihood <- function(parameters, data) {
  
  alpha <- parameters[1]
  lambda <- parameters[2]
  m <- parameters[3]
  a <- parameters[4]
  k <- parameters[5]
  # Calculate the log-likelihood value for the data
  log_lik <- sum(log(pdf(data,alpha,lambda, m,a,k)))
  
  return(-log_lik)
}


params_Lweibull <- function(data) {
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
  start_params <- c(0.1, 1, 0.1, 0.1,2)
  
  # Suppress warnings during parameter estimation
  result <- suppressWarnings(nlminb(start_params,
                                    log_likelihood,
                                    lower = c(0, 0, 0, 0,0),
                                    control = list(trace = FALSE)))
  
  # Extract the estimated parameters
  estimated_params <- result$par
  
  return(estimated_params)
}

estimated_params <- params_Lweibull(data)
estimated_params



lines(data, y <- cdf(data,estimated_params[1] ,estimated_params[2],
                     estimated_params[3],estimated_params[4],estimated_params[5]),col=1,lwd=2,lty=1)



legend("bottomright", legend = c("SHL-W", "LW", "WW", " OBXIIEW","McMW","BEW","EWW"),
       col = c(1,2,3,4,6,7,8), lty = c(1, 2, 2,2,2,2,2), bg = "white",cex=0.8)

