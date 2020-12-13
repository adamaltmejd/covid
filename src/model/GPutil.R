library(Matrix)
library(invgamma)
#'
#' Measument error prior
#' Q = (tau Q_0 + tau_E^-1 I)^{-1}
#'  @param x - (2x1) tau tau_E
#'  @param Ltheta    - (n x 1) KL base elements
#'  @param Q_E       - (n x 1) eigenvalues of Q_0
tau_sigma_prior <- function(x, L_theta, Q_E){
  tau <- x[1]
  tau_E <- x[2]
  if(min(tau,tau_E) <0)
    return(list(loglik=-Inf))
  D <- sqrt(1/(1/(tau*Q_E) + 1/tau_E))
  lik <- -1/2 * sum((D*L_theta)^2 )
  lik <- lik + sum(log(D[abs(D)>10e-14]))
  lik <- lik +  dinvgamma(tau, shape= 0.01, rate = 0.01, log = T)
  lik <- lik +  dinvgamma(tau_E, shape= 0.01, rate = 0.01, log = T)
  return(list(loglik = lik))
}

matern.covariance <- function(h, kappa, nu, sigma)
{
  if (nu == 1/2) {
    C = sigma^2*exp(-kappa*abs(h))
  } else {
    C = (sigma^2 / (2^(nu - 1) * gamma(nu))) * ((kappa*abs(h))^nu) * besselK(kappa*abs(h), nu)
  }
  C[h == 0] = sigma^2
  return(as.matrix(C))
}

lik.spatial <- function(Y, Sigma, R = NULL ){

  if(is.null(R))
    R = chol(Sigma)
  v = solve(t(R),Y)
  log.like = -sum(log(diag(R))) - 0.5*t(v) %*% v
  return(log.like)
}


#'
#' General Gaussian prior
#'
#'  @param theta - (nx1) the processes
#'  @param L     - (nxn) cholesky preicison factor
prior_GP <- function(theta, L){
  lik <- -1/2 * sum((L%*%theta)^2)
  Q = t(L)%*%L
  grad <-  -(Q%*%theta)
  Hessian <- - Q
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}

#'
#' General Gaussian prior with a an extra prior at step 1 of
#'
#'  @param theta -    (nx1) the processes
#'  @param L        - (nxn) cholesky preicison factor
#'  @param mu_sigma - (2x1) mu sigma^2, tau
prior_GP_p <- function(theta, L, mu_sigma_tau){
  Q = t(L)%*%L
  theta[1] = theta[1] - mu_sigma_tau[1]
  Q[1,1] <- Q[1,1] + 1/mu_sigma_tau[2]^2
  Q      <- mu_sigma_tau[3]*Q
  lik <- -1/2 * (t(theta)%*%Q%*%theta)

  grad <-  -(Q%*%theta)
  Hessian <- - Q
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}


#'
#' General Gaussian prior
#'
#'  @param theta - (nx1) the processes
#'  @param L     - (nxn) cholesky covariance factor
prior_GP2 <- function(theta, Sigma, mu ){
  theta_Sigma <- solve(Sigma,theta - mu )
  lik <- -1/2 * t(theta - mu)%*%theta_Sigma
  grad = theta_Sigma
  return(list(loglik = lik, grad = grad))
}

prior_GP3 <- function(theta, a1, a2, mu ,alpha=0.001, beta = 0.001){

  theta <- theta-mu
  n <- length(theta)
  L <- toeplitz(c(-a2,-a1,1, rep(0,n-3)))
  L[lower.tri(L,diag=F)] <- 0
  L <- L[-c(n,n-1),]
  L<-as(L, "sparseMatrix")
  lik <- -tau/2 * sum((L%*%theta)^2)
  Q = t(L)%*%L
  grad <- -tau * (Q%*%theta)
  ##
  # sample tau
  ##
  sigma   <- 1/rgamma(1, shape= (n-2)/2+1 + alpha, rate = -lik/tau + beta)
  tau_sample <- 1/sigma
  return(list(loglik = lik, grad = grad,  tau=tau_sample))

}


#'
#' first order random walk model (intrisinct)
#'  d theta /dt = N(0,\tau)
#'  @param theta - (nx1)
#'  @param tau   - precision
#'  @param alpha - prior parameter
#'  @param beta  - prior parameter
prior_s1d <-function(theta, tau, alpha = 0.01, beta = 0.01){

  n <- length(theta)
  L <- toeplitz(c(-1,1, rep(0,n-2)))
  L[lower.tri(L,diag=F)] <- 0
  L <- L[-n,]
  L<-as(L, "sparseMatrix")
  lik <- -tau/2 * sum((L%*%theta)^2)
  Q = t(L)%*%L
  grad <- -tau * (Q%*%theta)
  Hessian <- - tau *Q
  ##
  # sample tau
  ##
  sigma   <- 1/rgamma(1, shape= (n-1)/2+1 + alpha, rate = -lik/tau + beta)
  tau_sample <- 1/sigma
  return(list(loglik = lik, grad = grad, Hessian = Hessian, tau=tau_sample))
}

#'
#' second order random walk model (intrisinct)
#'  d theta /dt = N(0,\tau)
#'  @param theta - (nx1)
#'  @param tau   - precision
#'  @param alpha - prior parameter
#'  @param beta  - prior parameter
prior_s2d <-function(theta, tau, alpha = 0.01, beta = 0.01){

  n <- length(theta)
  L <- toeplitz(c(-1,2,-1, rep(0,n-3)))
  L[lower.tri(L,diag=F)] <- 0
  L <- L[-c(n,n-1),]
  L<-as(L, "sparseMatrix")
  lik <- -tau/2 * sum((L%*%theta)^2)
  Q = t(L)%*%L
  grad <- -tau * (Q%*%theta)
  Hessian <- - tau *Q
  ##
  # sample tau
  ##
  sigma   <- 1/rgamma(1, shape= (n-1)/2+1 + alpha, rate = -lik/tau + beta)
  tau_sample <- 1/sigma
  return(list(loglik = lik, grad = grad, Hessian = Hessian, tau=tau_sample))
}


#'
#' Poisson log likelihood
#' N     - (n x 1) Poisson observation
#' theta - (n x 1) log intens
#'
lik_poisson<- function(N, theta){
  lambda <- exp(theta)
  lik <- sum(N * theta -  lambda)
  grad <- N  - lambda
  Hessian <- - SparseM::diag(lambda)
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}
#'
#' Poisson log likelihood
#' N     - (n x 1) Poisson observation
#' theta - (m x 1) log intens
#' A     - (n x m) link matrix
#'
lik_poisson_noise<- function(N, theta,A){
  llambda <- A%*%theta
  lambda <- exp(llambda)
  lik <- sum(N * (llambda) -  lambda)
  grad <- t(A)%*%(N  - lambda)
  Hessian <- - t(A)%*%SparseM::diag(lambda)%*%A
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}
#'
#' Poisson log likelihood
#' N     - (n x 1) Poisson observation
#' theta - (m x 1) log intens
#' A     - (n x m) link matrix
#'
lik_poisson_noise<- function(N, theta,A){
  llambda <- as.vector(A%*%theta)
  lambda <- as.vector(exp(llambda))
  lik <- sum(N * (llambda) -  lambda)
  grad <- t(A)%*%(N  - lambda)
  Hessian <- - t(A)%*%SparseM::diag(lambda)%*%A
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}

#'
#' second order random walk model (intrisinct)
#'  d theta /dt = N(0,\tau)
#'  @param theta - (nx1)
#'  @param tau   - precision
prior_noise <-function(theta){

  n <- length(theta)
  lik <- - 0.5 * sum(theta[2:n]^2)
  grad <- - c(0,theta[2:n])
  Hessian <- - SparseM::diag(n)
  Hessian[1,1] <- 0
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}



#'
#' Poisson log likelihood
#' N     - (n x 1) Poisson observation
#' theta - (n x 1) log intens
#'
lik_poisson2<- function(N, theta){
  lambda <- apply(as.matrix(theta),1,function(x){max(x,0.1)})
  lik <- sum(N * log(lambda) -  lambda)
  grad <- (theta>0.1) * (N/lambda - 1)
  Hessian <- - SparseM::diag((theta>0.1)/lambda^2)
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}


dnegbin <- function(x, mu, phi, log.p=T){
  lik <- lgamma(x+phi)-lgamma(phi) - lgamma(x+1)
  lik <- lik + x*log(mu)
  lik <- lik + phi * log(phi)
  lik <- lik - (phi+x) * log(phi + mu)
  if(log.p)
    return(lik)
  return(exp(lik))
}


#'
#' Negative Biniomial log likelihood (phi)
#' N     - (n x 1) Poisson observation
#' theta - (m x 1) log intens
#' phi   - (1 x 1) over disperstion
#'
lik_negbin_phi <- function(N, theta, phi){
  mu <- exp(theta)
  lik <- dnegbin(N, mu, phi)
  grad <- digamma(N + phi) -digamma(phi)
  grad <- grad  + log(phi) + 1
  grad <- grad  - log(phi + mu)
  grad <- grad  - (phi+N)/(phi + mu)
  return(list(loglik = sum(lik), grad = sum(grad)))
}

#'
#' Negative Biniomial log likelihood (theta)
#' N     - (n x 1) Poisson observation
#' theta - (n x 1) log intens
#' phi   - (1 x 1) over disperstion
#'
lik_negbin_theta <- function(N, theta, phi){
  mu <- exp(theta)
  lik <- dnegbin(N, mu, phi)
  grad <- mu * (N/mu - (phi+N)/(phi + mu))
  return(list(loglik = sum(lik), grad = grad))
}

#'
#' Negative Biniomial log likelihood (theta)
#' N     - (n x 1) Poisson observation
#' theta - (m x 1) log intens
#' A     - (n x 1) linking obs to theta
#' phi   - (1 x 1) over disperstion
#'
lik_negbin_theta_A <- function(N, theta, phi, A){
  mu <- as.vector(exp(A%*%theta))
  lik <- dnegbin(N, mu, phi)
  grad <- as.vector(t(A)%*%(mu * (N/mu - (phi+N)/(phi + mu))))
  return(list(loglik = sum(lik), grad = grad))
}

##
#' testing lik_negbin_theta
#' N     - (n x 1) Poisson observation
#' theta - (m x 1) log intens
#' phi   - (1 x 1) over disperstion
##
test_negbin <- function(N, theta, phi){

  base <- lik_negbin_theta(N, theta, phi)
  base2 <- lik_negbin_phi(N, theta, phi)
  lik <- base$loglik
  grad_theta <- base$grad
  grad_phi <- base2$grad
  eps <- 1e-6
  grad_num_theta <- rep(0,length(theta))
  grad_num_phi   <- (lik_negbin_theta(N, theta, phi+eps)$loglik - lik)/eps
  for(i in 1:length(theta)){
    theta_temp <- theta
    theta_temp[i] <- theta_temp[i] + eps
    grad_num_theta[i] <- (lik_negbin_theta(N, theta_temp, phi)$loglik - lik)/eps
  }
  print(base$loglik - base2$loglik)
  print(grad_num_theta-grad_theta)
  print(grad_num_phi - grad_phi)
}
##
#' sampling from a discrete phi
#'
##
sample.phi<- function(phi, N, theta, alpha = NULL, samples=10){

  if(is.null(alpha))
    alpha <- 10
  acc <- 0
  mu = exp(theta)
  lik_0 <-sum(dnegbin(N, mu, phi))  - 0.01 * phi
  for(j in 1:samples){
    phi_star <- sample((phi-alpha):(phi+alpha), 1)
    if(phi_star<=0)
      next
    lik_star <- sum(dnegbin(N, mu, phi_star)) - 0.01 * phi_star
    if(log(runif(1)) < lik_star - lik_0){
      lik_i = lik_star
      phi <- phi_star
      acc <- acc + 1
    }
  }
 return(list(phi=phi,acc=acc))
}

##
#' sampling from a discrete phi with beta prior
#'
#' @param phi     - previous value of parameter
#' @param N       - (n x 1 ) observations of neg bin
#' @param prior   - (2 x 1) mean, sd for Beta distribution prior
#' @param theta   - (n x 1) log of parameter value
#' @param alpha   - (int) sample size
#' @param samples - (int) how many sample to within each iteration
##
sample.phi.prior<- function(phi, N, prior, theta, alpha = NULL, samples=10){

  if(is.null(alpha))
    alpha <- 10
  acc <- 0
  mu = exp(theta)
  lik_0 <-sum(dnegbin(N, mu, phi))  + dnorm(log(phi), prior[1], prior[2], log = T)
  for(j in 1:samples){
    phi_star <- sample((phi-alpha):(phi+alpha), 1)
    if(phi_star<=0)
      next
    lik_star <- sum(dnegbin(N, mu, phi_star)) + dnorm(log(phi_star), prior[1], prior[2], log = T)
    if(log(runif(1)) < lik_star - lik_0){
      lik_i = lik_star
      phi <- phi_star
      acc <- acc + 1
    }
  }
  return(list(phi=phi,acc=acc))
}
