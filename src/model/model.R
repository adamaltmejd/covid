library(tidyr)
source(file.path("src", "model", "util.R"))
source(file.path("src", "model", "MH.R"))
source(file.path("src", "model", "GPutil.R"))
source(file.path("src", "model", "MLbeta.R"))
library(numDeriv)
library(invgamma)
library(mvtnorm)


#'
#' model for estimating number of deaths
#' Each death is given an effort and
#' each days one works a random effort on each death once work>effort
#' the death is reported
#' No prior on number of deaths is given
#'
#' @param data -  (TxT ) of new_cases
#' @param model_parameters -
#'                          [sim]     number of MCMC samples
#'                          [burnin]  discaredd number of MCMC samples
#'                          [N.days.fixed] - the first N.days.fixed number of death is the reported cases
#'                          [quantile]     - lower uper confidence intervall for deaths
#' @param prior_list       - list of priors for the parameters
#'                         - [mu_beta]    mean     fixed effect parameters
#'                         - [Sigma_beta] variance fixed effect parameters
#'                         - [a_sigma]    variance parameter for the first seven days
#'                         - [b_sigma]    variance parameter after seven days
#' @param covariates       - covariates for latent processes
model <- function(new_cases, model_parameters, prior_list, covariates,beta_covarites=c( 0.8886356, 0.9882732) ,startvalue_list=NULL){

  N_0 <- dim(new_cases)[1]
  dates <- colnames(new_cases)
  mu_beta <- prior_list$mu_beta
  Q_beta <- solve(prior_list$Sigma_beta)
  #sigma prior
  a_sigma <- prior_list$a_sigma
  b_sigma <- prior_list$b_sigma

  theta_mu    <- prior_list$theta_mu
  theta_Sigma <- prior_list$theta_Sigma




  sim <- model_parameters$sim
  # start values
  sigma_time <- c(1,1)
  beta_time  <- mu_beta


  #known or fixed covariates
  #beta_fixed <- c(1)

  ###
  #
  # distribution of each death time point
  #
  ##
  Prob <- function(t){Prob_gamma(t, c(1,5))}



  ##
  # build models for time point distribution,
  # and sampling N
  ###
  X_time <- build_X_time(new_cases)
  new_cases[X_time$X_diag==1] <- NA # remove zero first observation

  dates_keep <- rowSums(is.na(new_cases))<dim(new_cases)[2]
  new_cases <- new_cases[dates_keep,]



  if(is.null(startvalue_list))
    N <-apply(new_cases,1,sum, na.rm=T)
  X_full <- c()
  X_fixed <- c()
  X_sigma <- c()
  timePoints_MH <- list()
  n.obs.full <- c()
  max_t <- 0 #largest number of ts
  for(i in 1:dim(new_cases)[1]){
    cases_i <- new_cases[i,]
    index_  <- is.na(cases_i)==F
    max_t <- max(c(max_t, sum(index_)))

    timePoints_MH[[i]] <- MH_setup(Dacc_prob=0.3)
    timePoints_MH[[i]]$sigma <- 0.05
    timePoints_MH[[i]]$theta <- rep(1, sum(index_))
    timePoints_MH[[i]]$n.obs <- cases_i[index_]
    timePoints_MH[[i]]$index_non_na <-      index_
    timePoints_MH[[i]]$Lik <- function(x,
                                       n.obs,
                                       N,
                                       mu_t,
                                       sigma_t,
                                       Prob) {
      if(min(x)<0)
        return(list(loglik =-Inf))
      log_lik   <- density_t(x, n.obs, N, Prob)
      lik_prior  <- prior_t(x,mu_t, sigma_t)
      return(list(loglik = log_lik + lik_prior))
    }
    n.obs.full <- c(n.obs.full,timePoints_MH[[i]]$n.obs)


    #setting upp covariates for priro
    timePoints_MH[[i]]$X <- cbind(X_time$X_first[i, index_]
                                  ,(X_time$X_t<=4)[i, index_] - X_time$X_first[i, index_],
                                  (X_time$X_t>4)[i, index_],
                                  X_time$X_na[i,index_])
    timePoints_MH[[i]]$X_fixed <- t(X_time$X_first[i, index_,drop=F])
    X_full <- rbind(X_full  , timePoints_MH[[i]]$X)
    #X_fixed <- rbind(X_fixed, timePoints_MH[[i]]$X_fixed)


    timePoints_MH[[i]]$X_sigma <- 1*cbind((X_time$X_t<=4)[i, index_],
                                          (X_time$X_t>4)[i, index_])
    X_sigma <- rbind(X_sigma,
                     timePoints_MH[[i]]$X_sigma)
    if(i > dim(new_cases)[1]-14){
      N[i] <- max(ceiling(mean(cases_i[index_][1:min(2,sum(index_))])/0.3), N[i])
      }
  }
  # for sampling adaptive N sampling
  alpha.MCMC <- rep(10, dim(new_cases)[1])
  acc_N      <- rep(0, dim(new_cases)[1])


  XXt <- t(X_full)%*%X_full
  Q_post     <- XXt +  Q_beta
  Sigma_post <- solve(Q_post)
  L_post <- t(chol(Sigma_post))


  ####
  #
  # GP prior
  #
  ####
  covariates <- covariates[dates_keep]
  A   <- matrix(0, nrow= dim(new_cases)[1], ncol = 2)
  A[,1] <- 1
  A[,2] <- covariates



  ####
  # GP prior done
  ####

  N_vec <- matrix(0,nrow=sim, ncol=length(N))
  colnames(N_vec) <- rownames(new_cases)
  Beta_vec         <- matrix(0, nrow=sim,   ncol=length(mu_beta))
  sigma_vec        <- matrix(0, nrow=sim,   ncol=2)
  ProbMatrix_vec   <- matrix(0, nrow=sim,   ncol= N_0*N_0)
  t_vec            <- matrix(NA, nrow=sim, ncol= dim(new_cases)[1] * max_t)


  lambda <- exp(A%*%beta_covarites)
  for(iter in 1:sim){

    # simulating time pointsm N and hyperparameters
    report_effort <- c()
    ProbMatrix_new <- matrix(NA, N_0, N_0)
    for(i in 1:dim(new_cases)[1]){
      timePoints_MH[[i]] <- MHiter(timePoints_MH[[i]],calcLik = T,
                                   timePoints_MH[[i]]$n.obs,
                                   N = N[i],
                                   timePoints_MH[[i]]$X%*%beta_time,# + timePoints_MH[[i]]$X_fixed%*%beta_fixed,
                                   timePoints_MH[[i]]$X_sigma%*%sigma_time,
                                   Prob)
      report_effort <- c(report_effort, timePoints_MH[[i]]$theta)
      t_vec[iter, max_t*(i-1)+ (1:sum(timePoints_MH[[i]]$index_non_na))] <- (timePoints_MH[[i]]$theta - timePoints_MH[[i]]$X%*%beta_time)
      ProbMatrix_new[i, timePoints_MH[[i]]$index_non_na] <- Prob(timePoints_MH[[i]]$theta)[1:(length(timePoints_MH[[i]]$theta))]
      if(i > model_parameters$N.days.fixed){
        # sampling N

        Nstar <- sample((N[i]-alpha.MCMC[i]):(N[i]+alpha.MCMC[i] ), 1)
        if(Nstar >= sum(timePoints_MH[[i]]$n.obs)){
          lik      <- density_t((timePoints_MH[[i]]$theta), timePoints_MH[[i]]$n.obs, N[i], Prob)   + dpois(N[i], lambda[i],log=T)
          lik_star <-  density_t((timePoints_MH[[i]]$theta), timePoints_MH[[i]]$n.obs, Nstar, Prob) + dpois(Nstar, lambda[i],log=T)
          if(log(runif(1)) < lik_star-lik ){
            N[i] <- Nstar
            acc_N[i] <- acc_N[i] + 1
          }
        }
      }
    }





    ##
    # sampling latent parameters
    #
    ###
    if(iter >100){
      Q_obs      <- diag(1./as.vector(X_sigma%*%sigma_time)^2)
      Q_post     <- t(X_full)%*%Q_obs%*%(X_full) +  Q_beta
      Sigma_post <- solve(Q_post)

      L_post <- t(chol(Sigma_post))
      #report_effort <-  report_effort -  X_fixed%*%beta_fixed
      mu_hat <- Sigma_post%*%(t(X_full)%*%(Q_obs%*%report_effort) + Q_beta%*%mu_beta)

      beta_time     <- mu_hat +   ( L_post%*%rnorm(length(beta_time)))

      a_hat  <- a_sigma + colSums(X_sigma)/2
      report_effort = report_effort - X_full%*%beta_time
      b_hat  <- b_sigma + 0.5  *t(X_sigma)%*%(report_effort)^2
      sigma_time    <- sqrt(rinvgamma(2, shape= a_hat, rate=b_hat))


    }



    if(iter%%50==0 &  iter < model_parameters$burnin){
      alpha.MCMC[acc_N/50 > 0.3] <- alpha.MCMC[acc_N/50 > 0.3] +1
      alpha.MCMC[acc_N/50 < 0.3] <- alpha.MCMC[acc_N/50 < 0.3] -1
      alpha.MCMC[alpha.MCMC<1] <- 1
      acc_N             <-acc_N* 0
    }

    Beta_vec[iter, ] <- beta_time #iXXt%*%t(X_full)%*%log_t
    sigma_vec[iter,] <- sigma_time
    ProbMatrix_vec[iter,] <- ProbMatrix_new
    N_vec[iter,] <- N

  }
  Beta_vec        <- Beta_vec[model_parameters$burnin:model_parameters$sim, ]
  sigma_vec       <- sigma_vec[model_parameters$burnin:model_parameters$sim,]
  N_vec           <- N_vec[model_parameters$burnin:model_parameters$sim,]
  ProbMatrix_vec  <- ProbMatrix_vec[model_parameters$burnin:model_parameters$sim,]
  t_vec           <- t_vec[model_parameters$burnin:model_parameters$sim,]
  post_sigma_1 <- ml_inversegamma(sigma_vec[,1]^2)
  post_sigma_2 <- ml_inversegamma(sigma_vec[,2]^2)


  posterior_list <- list(mu_beta      = colMeans(Beta_vec),
                         Sigma_beta   = cov(Beta_vec),
                         a_sigma      = c(post_sigma_1[1],post_sigma_2[1]),
                         b_sigma      = c(post_sigma_1[2],post_sigma_2[2]))


  result <- list()
  CI <- t(apply(N_vec,2 , quantile, prob=model_parameters$quantile))
  result$Npost <- data.frame(dates = rownames(new_cases),
                             mean  = colMeans(N_vec),
                             median  =apply(N_vec,2 , quantile, prob=0.5),
                             lCI   = CI[,1],
                             uCI   = CI[,2])

  result$posteriror_list <- posterior_list
  result$posteriror_sample <- list(Beta        = Beta_vec,
                                   sigma      = sigma_vec,
                                   N          = N_vec,
                                   ProbMatrix = ProbMatrix_vec,
                                   t          = t_vec,
                                   max_t      = max_t)
  result$X_full <- X_full
  return(result)
}





