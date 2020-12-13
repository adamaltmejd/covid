MHiter <- function(MH_obj, calcLik = F,...)
{
  #' one iteration of MH-TE d and stochastic gradient approximation
  #' @param MH is a list containg
  #'           $Lik       -> takes theta computes log-likelihood
  #'           $theta     -> (dx1) vector of parameter
  #'           $Sigma     -> RH Sigma
  #'           $sigma     -> multiplicative coeffients
  #'           $lik_old   -> old likelihood value  d
  #'           $grad_old  -> the previous gradient
  #'           $thetaM    -> the estimated mean value
  #'           $n_SA      -> iteration number of for mean and covariance
  #'           $n_SA2     -> iteration number of option 2
  #'           $c_p       -> constant to take power of between takes value between (0,1]
  #'           $Dacc_prob -> desired accpetence probabilility
  #' @param  ... arguments to the likelihood and gradient
  if(calcLik){
    res <- MH_obj$Lik(MH_obj$theta,...)
    MH_obj$grad_old <- res$grad
    MH_obj$lik_old  <- res$loglik
    MH_obj$res <- res
  }

  MH_obj <- MH_RW(MH_obj, ...)
  MH_obj <- MH_SA(MH_obj, option = 1)
  MH_obj <- MH_SA(MH_obj, option = 2)

  return(MH_obj)
}

MALAiter <- function(MH_obj, calcLik = F,...)
{
  #' one iteration of MALA and stochastic gradient approximation
  #' @param MH is a list containg
  #'           $Lik       -> takes theta computes log-likelihood
  #'           $theta     -> (dx1) vector of parameter
  #'           $Sigma     -> RH Sigma
  #'           $sigma     -> multiplicative coeffients
  #'           $lik_old   -> old likelihood value  d
  #'           $grad_old  -> the previous gradient
  #'           $thetaM    -> the estimated mean value
  #'           $n_SA      -> iteration number of for mean and covariance
  #'           $n_SA2     -> iteration number of option 2
  #'           $c_p       -> constant to take power of between takes value between (0,1]
  #'           $Dacc_prob -> desired accpetence probabilility
  #' @param  ... arguments to the likelihood and gradient
  if(calcLik){
    res <- MH_obj$Lik(MH_obj$theta,...)
    MH_obj$grad_old <- res$grad
    MH_obj$lik_old  <- as.vector(res$loglik)
    MH_obj$res <- res
  }

    MH_obj <- MH_MALA(MH_obj, ...)
    MH_obj <- MH_SA(MH_obj, option = 1)
    MH_obj <- MH_SA(MH_obj, option = 2)

    return(MH_obj)
}

MALAHiter <- function(MH_obj, calcLik = F,...)
{
  #' one iteration of MALA using Hessian and stochastic gradient approximation
  #' @param MH is a list containg
  #'           $Lik       -> takes theta computes log-likelihood
  #'           $theta     -> (dx1) vector of parameter
  #'           $Sigma     -> RH Sigma
  #'           $sigma     -> multiplicative coeffients
  #'           $lik_old   -> old likelihood value  d
  #'           $grad_old  -> the previous gradient
  #'           $thetaM    -> the estimated mean value
  #'           $n_SA      -> iteration number of for mean and covariance
  #'           $n_SA2     -> iteration number of option 2
  #'           $c_p       -> constant to take power of between takes value between (0,1]
  #'           $Dacc_prob -> desired accpetence probabilility
  #' @param  ... arguments to the likelihood and gradient
  if(calcLik){
    res <- MH_obj$Lik(MH_obj$theta,...)
    MH_obj$grad_old     <- res$grad
    MH_obj$lik_old      <- res$loglik
    MH_obj$Hessian_old  <- res$Hessian
    R = chol(-MH_obj$Hessian_old)
    MH_obj$iHessian_old <- chol2inv(R)
    MH_obj$L_old        <- t(R)
  }

  MH_obj <- MH_MALA_Hessian(MH_obj, ...)
  MH_obj <- MH_SA(MH_obj, option = 2)

  return(MH_obj)
}
LCNiter <- function(MH_obj, calcLik = F,...)
{
  #' Precond crank nicholson iteration using the Hessian
  #' model is
  #' theta ~ N(0, C)
  #' Y     ~ exp(-\Phi(theta))
  #' @param MH is a list containg
  #'           $Lik       -> takes theta computes log-likelihood
  #'           $theta     -> (dx1) vector of parameter
  #'           $Sigma     -> RH Sigma
  #'           $sigma     -> multiplicative coeffients
  #'           $lik_old   -> old likelihood value  d
  #'           $grad_old  -> the previous gradient
  #'           $thetaM    -> the estimated mean value
  #'           $n_SA      -> iteration number of for mean and covariance
  #'           $n_SA2     -> iteration number of option 2
  #'           $c_p       -> constant to take power of between takes value between (0,1]
  #'           $Dacc_prob -> desired accpetence probabilility
  #' @param  ... arguments to the likelihood and gradient
  if(calcLik){
    res <- MH_obj$Lik(MH_obj$theta,...)
    MH_obj$grad_old     <- res$grad
    MH_obj$lik_old      <- res$loglik
    MH_obj$Hessian_old  <- res$Hessian
    R = chol(-MH_obj$Hessian_old)
    MH_obj$iHessian_old <- chol2inv(R)
    MH_obj$R_old        <- R
  }

  MH_obj <- MH_LCN_Hessian(MH_obj, ...)
  MH_obj <- MH_SA(MH_obj, option = 3)

  return(MH_obj)
}
MH_LCN_Hessian <- function(MH_obj,...)
{
  #'Precond crank nicholson iteration using the Hessian
  #' model is
  #' theta ~ N(0, C)
  #' Y     ~ exp(-\Phi(theta))
  #' we don't simplification
  #' likelihood here repesent both prior and likelihood.
  #' @param MH is a list containg
  #'           $Phi      -> takes theta computes log-likelihood
  #'           $theta    -> (dx1) vector of parameter
  #'           $beta     -> multiplicative coeffients in adaptive mcmc
  #'           $lik_old  -> old likelihood value  d
  #'           $grad_old -> the previous gradient
  #'           $grad_old -> the previous gradient
  #' @param  ... arguments to the likelihood and gradient

  MH_obj$accept = 0
  MH_obj$count <- MH_obj$count + 1

  MH_obj$beta <- MH_obj$sigma
  sqrtOneBeta2 = 1-sqrt(1-MH_obj$beta^2)

  mu_old     <-  (1-sqrtOneBeta2)*MH_obj$theta + sqrtOneBeta2 * ( MH_obj$iHessian_old%*%MH_obj$grad_old)
  #theta_star <- as.vector(mu_old + MH_obj$beta * SparseM::backsolve(MH_obj$R_old,
  #                                                                  rnorm(length(MH_obj$theta)),
  #                                                                  twice=F))
  iL <- chol(MH_obj$iHessian_old)
  theta_star <- as.vector(mu_old + MH_obj$beta * iL%*%rnorm(length(MH_obj$theta)))


  res <- MH_obj$Lik(theta_star,...)
  lik_star <- res$loglik
  if(lik_star == -Inf) { return(MH_obj) }

  R = chol(-res$Hessian)
  iHessian <- chol2inv(R)
  mu_star <- (1-sqrtOneBeta2)*theta_star +  sqrtOneBeta2 *(iHessian%*%res$grad)

  q_x      <- - (t(MH_obj$theta - mu_star)%*%(-res$Hessian        )%*%(MH_obj$theta - mu_star)) / (2* MH_obj$beta^2)
  q_xstar  <- - (t(theta_star   - mu_old )%*% (-MH_obj$Hessian_old)%*%(theta_star   - mu_old )) / (2* MH_obj$beta^2)


  if( log(runif(1)) < lik_star - MH_obj$lik_old + as.vector(q_x) - as.vector(q_xstar))
  {
    MH_obj$Hessian_old  <- res$Hessian
    MH_obj$iHessian_old <- iHessian
    MH_obj$theta   = theta_star
    MH_obj$lik_old = lik_star
    MH_obj$R_old   = R
    MH_obj$accept  = 1
    MH_obj$grad_old <- res$grad

    return(MH_obj)
  }
  return(MH_obj)
}

MH_MALA_Hessian <- function(MH_obj,...)
{
  #'MALA iteration using the Hessian
  #' @param MH is a list containg
  #'           $Lik      -> takes theta computes log-likelihood
  #'           $theta    -> (dx1) vector of parameter
  #'           $Sigma    -> RH Sigma
  #'           $sigma    -> multiplicative coeffients
  #'           $lik_old  -> old likelihood value  d
  #'          $grad_old -> the previous gradient
  #' @param  ... arguments to the likelihood and gradient

  MH_obj$accept = 0
  MH_obj$count <- MH_obj$count + 1


  mu_old <-  MH_obj$theta +  MH_obj$sigma^2/2 * ( MH_obj$iHessian_old%*%MH_obj$grad_old)
  #theta_star <- as.vector(mu_old + MH_obj$sigma * forwardsolve(MH_obj$L_old, rnorm(length(MH_obj$theta))))
  iL <- chol(MH_obj$iHessian_old)
  theta_star <- as.vector(mu_old + MH_obj$sigma * iL%*%rnorm(length(MH_obj$theta)))

  res <- MH_obj$Lik(theta_star,...)
  lik_star <- res$loglik
  if(lik_star == -Inf) { return(MH_obj) }

  R = chol(-res$Hessian)
  iHessian <- chol2inv(R)
  iHessian <- solve(- res$Hessian)
  mu_star <- theta_star +  MH_obj$sigma^2/2 *(iHessian%*%res$grad)
  Lstar <- t(chol(-res$Hessian))
  q_x      <- - (t(MH_obj$theta - mu_star)%*%(-res$Hessian        )%*%(MH_obj$theta - mu_star)) / (2* MH_obj$sigma^2)
  q_xstar  <- - (t(theta_star   - mu_old )%*% (-MH_obj$Hessian_old)%*%(theta_star   - mu_old )) / (2* MH_obj$sigma^2)


  if( log(runif(1)) < lik_star - MH_obj$lik_old + as.vector(q_x) - as.vector(q_xstar))
  {
    MH_obj$Hessian_old  <- res$Hessian
    MH_obj$iHessian_old <- iHessian
    MH_obj$theta   = theta_star
    MH_obj$lik_old = lik_star
    MH_obj$L_old   = t(R)
    MH_obj$accept  = 1
    MH_obj$grad_old <- res$grad

    return(MH_obj)
  }
  return(MH_obj)
}

MH_MALA <- function(MH_obj,...)
{
  #'MALA object
  #' @param MH is a list containg
  #'           $Lik      -> takes theta computes log-likelihood
  #'           $theta    -> (dx1) vector of parameter
  #'           $Sigma    -> RH Sigma
  #'           $sigma    -> multiplicative coeffients
  #'           $lik_old  -> old likelihood value  d
  #'          $grad_old -> the previous gradient
  #' @param  ... arguments to the likelihood and gradient

  MH_obj$accept = 0
  MH_obj$count <- MH_obj$count + 1

  Le <-exists('L', where=MH_obj)
  #if(exists('Sigma', where=MH_obj) ==TRUE && Le == FALSE)
  #  MH_obj$L <- t(chol(MH_obj$Sigma))

  if(Le==FALSE)
  {
    mu_old <- as.vector(MH_obj$theta + MH_obj$grad_old * MH_obj$sigma^2/2)
    theta_star <- as.vector(mu_old  + MH_obj$sigma * rnorm(length(MH_obj$theta)))

  }else{
    mu_old <-  as.vector(MH_obj$theta +  MH_obj$sigma^2/2 *( MH_obj$Sigma%*%MH_obj$grad_old))
    theta_star <- as.vector(mu_old + MH_obj$sigma * (MH_obj$L%*%rnorm(length(MH_obj$theta))))
  }
  res <- MH_obj$Lik(theta_star,...)
  lik_star <- as.vector(res$loglik)
  if(lik_star == -Inf) { return(MH_obj) }



  if(Le==FALSE) {
    mu_star <- theta_star + MH_obj$sigma^2/2 * res$grad
    q_x      <- - sum( (MH_obj$theta - mu_star)^2 )/(2* MH_obj$sigma^2)
    q_xstar  <- - sum( (theta_star   - mu_old)^2  )/(2* MH_obj$sigma^2)
  }else{
    mu_star <- theta_star +  MH_obj$sigma^2/2 *( MH_obj$Sigma%*%res$grad)
    q_x      <- - sum(solve(MH_obj$L, MH_obj$theta - mu_star)^2) /(2* MH_obj$sigma^2)
    q_xstar  <- - sum(solve(MH_obj$L, theta_star   - mu_old)^2)  /(2* MH_obj$sigma^2)
  }

  if( log(runif(1)) < lik_star - MH_obj$lik_old + q_x - q_xstar)
  {
    MH_obj$theta   = theta_star
    MH_obj$lik_old = lik_star
    MH_obj$accept  = 1
    MH_obj$grad_old <- res$grad
    MH_obj$res <- res

  }
  return(MH_obj)
}



MH_SA <- function(MH_obj, option = 1)
{
  # updating the covariance matrix using standard Stochastic approximation
  # needs:
  # if option 1
  # MH_obj$theta  <- vector of intrest
  # MH_obj$thetaM <- the estimated mean value
  # MH_obj$Sigma  <- the estimated covariance
  # MH_obj$n_SA   <- iteration number of option 2
  # if option 2
  # MH_obj$n_SA2  <- iteration number of option 2
  # MH_obj$c_p    <-  constant to take power of between takes value between (0,1]
  #MH_obj$Dacc_prob <- desired accpetence probabilility

  if(option == 1)
  {
    if(MH_obj$count %% 2 == 0){
    w0 <- (MH_obj$n_SA -1)

    w1 <- MH_obj$n_SA
    if(length(MH_obj$thetaM) == length(MH_obj$theta)){
      MH_obj$thetaM <- w0/w1 * MH_obj$thetaM + 1/w1 * MH_obj$theta
      MH_obj$Sigma  <- w0/w1 * MH_obj$Sigma + 1/w1 * (MH_obj$thetaM- MH_obj$theta)%*%t(MH_obj$thetaM- MH_obj$theta)
    }else{
      MH_obj$thetaM = MH_obj$theta
      MH_obj$Sigma  <- (MH_obj$thetaM- MH_obj$theta)%*%t(MH_obj$thetaM- MH_obj$theta)
    }
    MH_obj$n_SA  <- MH_obj$n_SA + 1

    if(MH_obj$n_SA  == 20 * nrow(MH_obj$Sigma)){
      #MH_obj$L <- t(chol(MH_obj$Sigma + 0.01*min(diag(MH_obj$Sigma)) * diag(dim(MH_obj$Sigma)[1])))
      MH_obj$L <- t(chol(MH_obj$Sigma))
      MH_obj$sigma <- MH_obj$sigma/sqrt(max(diag(MH_obj$Sigma)))

    }
    if(MH_obj$count %% 50 == 0){
      if(MH_obj$n_SA > 20 * nrow(MH_obj$Sigma)){
        #MH_obj$L <- t(chol(MH_obj$Sigma + 0.01*min(diag(MH_obj$Sigma)) * diag(dim(MH_obj$Sigma)[1])))
        MH_obj$L <- t(chol(MH_obj$Sigma ))
      }
    }
  }}
  if( option == 2)
  {
      if(MH_obj$count %% 50  == 0){
        acc          <-  MH_obj$accept_count / 50
        w0 <- (1/MH_obj$n_SA2)^MH_obj$c_p
        MH_obj$sigma <- MH_obj$sigma * exp(w0 *  (acc - MH_obj$Dacc_prob))
        MH_obj$n_SA2 <- MH_obj$n_SA2 + 1
        MH_obj$accept_count <- 0
      }else{
        MH_obj$accept_count <- MH_obj$accept_count + MH_obj$accept
      }
  }
  if( option == 3){
    if(MH_obj$count %% 50  == 0){
      acc          <-  MH_obj$accept_count / 50
      w0 <- (1/MH_obj$n_SA2)^MH_obj$c_p
      MH_obj$sigma <- MH_obj$sigma * exp(w0 *  (acc - MH_obj$Dacc_prob))
      MH_obj$n_SA2 <- MH_obj$n_SA2 + 1
      MH_obj$accept_count <- 0
      MH_obj$sigma <- min(MH_obj$sigma, 1-1e-6)
    }else{
      MH_obj$accept_count <- MH_obj$accept_count + MH_obj$accept
    }
  }


  return(MH_obj)
}
###
# sets up a basic object for optimization
#
# for running the object one needs to set:
#
#   theta <- parameters for Lik
#   Lik   <- loglikelhiood object
#
#
##
MH_setup <- function(Dacc_prob = 0.5)
{
  MH_obj <- list()
  MH_obj$count <- 0
  MH_obj$accept_count <- 0
  MH_obj$count_prev <- 0
  MH_obj$c_p <- 0.5
  MH_obj$n_SA  <- 20
  MH_obj$n_SA2 <- 20
  MH_obj$sigma <- 0.001
  MH_obj$Sigma  <- 0.
  MH_obj$thetaM <- 0.
  MH_obj$Dacc_prob <- Dacc_prob
  return(MH_obj)
}


MH_MALA_diag <- function(MH_obj,...)
{
  #MALA
  # MH is a list containg
  # $Lik      -> takes theta computes log-likelihood
  # $theta    -> (dx1) vector of parameter
  # $Sigma    -> RH Sigma
  # $sigma    -> multiplicative coeffients
  # $lik_old  -> old likelihood value  d
  # $grad_old -> the previous gradient
  # ... arguments to the likelihood

  MH_obj$accept = 0
  MH_obj$count <- MH_obj$count + 1
  D2 <- diag(MH_obj$Sigma)
  D <- sqrt(D2)


  mu_old <-  MH_obj$theta +  MH_obj$sigma^2/2 *( D2*MH_obj$grad_old)
  theta_star <- mu_old + MH_obj$sigma * (D*rnorm(length(MH_obj$theta)))
  res <- MH_obj$Lik(theta_star,...)
  lik_star <- res$loglik
  if(lik_star == -Inf) { return(MH_obj) }




  mu_star <- theta_star +  MH_obj$sigma^2/2 *( D2*res$grad)
  q_x      <- - sum(((MH_obj$theta - mu_star)/D)^2) /(2* MH_obj$sigma^2)
  q_xstar  <- - sum(((theta_star   - mu_old)/D)^2)  /(2* MH_obj$sigma^2)


  if( log(runif(1)) < lik_star - MH_obj$lik_old + q_x - q_xstar)
  {
    MH_obj$theta   = theta_star
    MH_obj$lik_old = lik_star
    MH_obj$accept  = 1
    MH_obj$grad_old <- res$grad
    MH_obj$accept_count <- MH_obj$accept_count  +1

    return(MH_obj)
  }
  return(MH_obj)
}

MH_RW <- function(MH_obj,...)
{
  #regular MH random walk
  # MH is a list containg
  # $Lik     -> takes theta computes log-likelihood
  # $theta   -> (dx1) vector of parameter
  # $Sigma   -> RH Sigma
  # $sigma   -> multiplicative coeffients
  # $lik_old -> old likelihood value  d
  # ... arguments to the likelihood


  MH_obj$count <- MH_obj$count + 1

  Le <- "L" %in% names(MH_obj)  

  if(Le==FALSE)
  {
    theta_star <- MH_obj$theta + MH_obj$sigma * rnorm(length(MH_obj$theta))
  }else{
    theta_star <- MH_obj$theta + MH_obj$sigma * MH_obj$L%*%rnorm(length(MH_obj$theta))
  }
  lik_star <- MH_obj$Lik(theta_star,...)$loglik

  if( log(runif(1)) < lik_star - MH_obj$lik_old)
  {
    MH_obj$theta   = theta_star
    MH_obj$lik_old = lik_star
    MH_obj$accept  = 1
    MH_obj$accept_count <- MH_obj$accept_count  +1

    return(MH_obj)
  }
  MH_obj$accept = 0
  return(MH_obj)
}


