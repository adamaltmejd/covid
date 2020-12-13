#'
#' computes the probability that a gamma random variable 
#' is between 0,t_1,t_2,...,
#' 
#' @param  t     - (T x 1) time points
#' @param  theta - (2 x 1) shape of gamma, mean of gamma
Prob_gamma <- function(t,theta){
  
  PT <- pgamma(cumsum(c(t,Inf)), shape = theta[1], scale = theta[2]/theta[1])
  return(diff(c(0,PT)))
}
#'
#' the Jacobian of Prob_gamma with respect to t
#' 
#' @param  t     - (T x 1) time points
#' @param  theta - (2 x 1) shape of gamma, mean of gamma
dProb_gamma <- function(t, theta){
  print("ERRROR has not adjusted for adding inf")
  fT <- dgamma(cumsum(c(t,Inf)), shape = theta[1], scale = theta[2]/theta[1])
  dfT <- diff(fT)
  J <- matrix(0, nrow=length(t), ncol=length(t))
  diag(J) <- fT
  for(j in 1:(length(t)-1))
    J[(j+1):(length(t)),j] <- dfT[j:(length(t)-1)]
  
  return(J)
}

#' computes the log likelihood of observing new_cases given t, N, Prob
#' @param  tdiff      - (m x 1) time difference at location 
#' @param  new_cases - (m x T) new cases each time point 
#' @param  N         - (1 x 1) number of dead
#' @param  Prob      - (function) probabiliy \int_{t_0}^t_1 f_{\theta}(x) dx   
density_t <- function(tdiff, n_obs, N, Prob){
  
  P <- Prob(tdiff)
  return(sum(dbinom(c(n_obs,N-sum(n_obs)), N, P, log=T)))
  
}

#'
#' @param  tdiff      - (m x 1) time difference at location
#' @param  mu_theta   - (m x 1) mean of lognormal
#' @parma  sigma_theta - (m x 1) std of log normal
prior_t <- function(tdiff, mu_theta, sigma_theta){
  return(sum(dnorm(tdiff, mean = mu_theta, sd=sigma_theta, log=T)))
}

#' prior distribution of N
#' 
#' @param N       - (T x 1)
#' @param theta_N - (list)
prior_N <- function(N, theta_N){
  
  
} 

#'
#' fitting maxmimum likelhood for inverse gamma distribuiton
#' @param x - (n x 1) data
ml_inversegamma <- function(x){
  n <- length(x)
  mu <- mean(x)
  v = var(x)
  alpha <- mu^2/v + 2
  C = - log(sum(1/x)) - mean(log(x))
  alpha_0 <- Inf
  while(abs(alpha_0-alpha)>1e-6*alpha){
    alpha_0 <- alpha
    f <- function(x){ (digamma(x) - log(n*alpha_0) - C)^2}
    alpha  <- optim(alpha, f,method=c("Brent"), lower=1e-6, upper=100*(n))$par
  }
  return(c(alpha,n*alpha/sum(1/x)))
}