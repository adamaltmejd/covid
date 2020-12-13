
source(file.path("src", "model", "util.R"))
##
#'
#'@param    lag    - (int)   lag days  (lag = 0 is only first non-holiday, lag = 1 two non holidays, etc...)
#'@param    npar   - (int)   number of days with unique values to estimate parameters on
#'@param    theta0 - (p x 1) inital guess of parameters
#'@param    latest_days_not_report - (int) dont use the latest day due to bias in the estimate of N
##
ML_betaBin <- function(result,
                       lag,
                       npar = 2,
                       theta0 = NULL,
                       latest_days_not_report = 10){

    report <- splitlag(result$detected,
                       as.Date(result$dates_report) ,
                       lag,
                       result$dates_not_reported)
    report_lag <- report$Reported_T
    deaths_est_T <- apply(report$Reported_T, 1, max, na.rm=T)

    data_T <- newDeaths(deaths_est_T,
                        report_lag)
    N <- dim(data_T$death.remain)[2]
    X <- setup_data_lag(N, npar, result$dates_report[1:N],npar)
    IndexX <- matrix(FALSE, N, N)
    for(i in 1:N){
        if( sum(data_T$death.remain[i,] == 0 & is.na(data_T$death.remain[i,] )==F) >0){
            IndexX[i,i:(which.max(data_T$death.remain[i,]==0)-2)] =T
        }
    }
    IndexX[,result$dates_not_reported]=F
    IndexX[(N-latest_days_not_report):N,]=F
    X <- X[IndexX[upper.tri(data_T$death.remain,diag = T)],]
    index <- upper.tri(data_T$death.remain,diag = T) & IndexX
    y = data_T$report.new[index ]
    n = data_T$death.remain[index ]
    index = is.na(y)==F & is.na(n)==F & n>0
    X <- as.matrix(X[index,])
    y <- y[index]

    n <- n[index]
    p <- dim(X)[2]
    if(is.null(theta0))
        theta0 <- rep(0,2*p)
    #res <- optim(theta0, function(x){-log_bb(x, y, n, X)  + sum(exp(x[(p+1):(2*p)]))})
    res <- optim(theta0, function(x){-log_bb(x, y, n, X)})
    for(i in 1:300){
        #res <- optim(res$par,function(x){-log_bb(x, y, n, X)  + sum(exp(x[(p+1):(2*p)]))})
        res <- optim(res$par,function(x){-log_bb(x, y, n, X)  })
        if(res$convergence==0)
            break
    }
    if(res$convergence>0){
        print('warning non convergence')
    }else{
        print(paste('log lik = ',-res$value))
    }
    return(list(alpha_X = res$par[1:p], beta_X = res$par[(p+1):(2*p)]))
}


##
#'
#'    lag    - (int)   lag days  (lag = 0 is only first non-holiday, lag = 1 two non holidays)
#'    theta0 - (p x 1) inital guess of parameters
##
ML_betaBin_post <- function(result,
                       lag,
                       maxusage.day,
                       npars,
                       theta0 = NULL,
                       latest_days_not_report = 10){

    N <- dim(result$detected)[2]
    report <- splitlag(result$detected,
                       as.Date(result$dates_report)
                       ,lag
                       ,result$dates_not_reported)
    report_lag <- report$Reported_O
    deaths_est_T <- apply(report$Reported_O[1:N,], 1, max, na.rm=T)
    report_lag <- report$Reported_O[1:N,1:N]
    data_O <- newDeaths(deaths_est_T,
                        report_lag)

    X <- setup_data_postlag2(N, lag, npars, result$dates_report[1:N])
    IndexX <- matrix(FALSE, N, N)
    for(i in 1:N){
        if( N-i >= maxusage.day){
            IndexX[i,] =T
        }
    }
    IndexX[,result$dates_not_reported]=F
    IndexX[(N-latest_days_not_report):N,]=F
    X <- X[IndexX[upper.tri(data_O$death.remain,diag = T)],]

    index <- upper.tri(data_O$death.remain,diag = T) & IndexX
    y = data_O$report.new[index ]
    n = data_O$death.remain[index ]
    index =  n>0 &is.na(y)==F & is.na(n)==F
    X <- as.matrix(X[index,])
    y <- y[index]
    n <- n[index]
    p <- dim(X)[2]
    if(is.null(theta0))
        theta0 <- rep(0,2*p)
    #res <- optim(theta0, function(x){-log_bb(x, y, n, X)  + sum(exp(x[(p+1):(2*p)]))})
    res <- optim(theta0, function(x){-log_bb(x, y, n, X)  })
    for(i in 1:30){
        #res <- optim(res$par,function(x){-log_bb(x, y, n, X)  + sum(exp(x[(p+1):(2*p)]))})
        res <- optim(res$par,function(x){-log_bb(x, y, n, X)  })
        if(res$convergence==0)
            break
    }
    if(res$convergence>0){
        print('warning non convergence')
    }else{
        print(paste('log lik = ',-res$value))
    }
    return(list(alpha_X = res$par[1:p], beta_X = res$par[(p+1):(2*p)]))
}

log_bb<- function(theta, y, n,X){
    p <- length(theta)/2
    mu <- 1/(1+exp(-X%*%theta[1:p]))
    M <- exp(X%*%theta[(p+1):(2*p)])
    alpha <- mu*M
    beta  <- M-alpha
    if(min(1/(alpha+beta)) <10^-3)
        return(-Inf)
    lik <- sum(dBB(x =y,
                   size = n,
                   alpha = alpha,
                   beta = beta,
                   log.p=T))
    return(lik)
}


##
#' running the data for obsevations after the lag
#' result       - data
#' alpha_beta   - parameter for beta_binomial distribuiton
#' npars         - number of parameters used for alpha_beta model
#' true.day     - treat all days prior to true.day as fixed
#' maxusage.day - only use data up to maxusage.day to estimate data
#'                death reported after maxusage.day are ignored
##
BetaGP_lag_post_fast <- function(
                                    result,
                                    alpha_Beta,
                                    lag,
                                    npars,
                                    theta_prior,
                                    true.day = 0,
                                    maxusage.day = 30,
                                    MCMC_sim  = 1000,
                                    burnin_p = 0.5,
                                    deaths_sim = 30){
    N_T <- length(result$dates)
    deaths_est_T <- apply(result$detected, 1, max, na.rm=T)
    report <- splitlag(result$detected,
                       as.Date(result$dates_report),
                       lag,
                       result$dates_not_reported)

    X_T <- setup_data_postlag2(N_T, lag, npars, result$dates_report)
    deaths_est <- apply(report$Reported_O, 1, function(x) { if(all(is.na(x))){return(0)};
        max(x,na.rm=T)})
    X_mu <- X_T
    X_M  <- X_T
    p  <- dim(X_mu)[2]
    p1 <- dim(X_M)[2]


    ##
    # seting up MCMC GP
    ##

    phi.samples <- 10
    phi <- 100
    alpha.phi <- 10
    ###
    # A -matrix
    ###
    pool <- 1
    A_m  <- ceiling(N_T/pool)
    A_j <- rep(1:A_m,each=pool)[(A_m*pool - N_T + 1):(A_m*pool)]
    A   <- sparseMatrix(i=1:N_T,j=A_j,  dims=c(N_T, A_m))
    Start_theta <- as.vector(log((t(A)%*%deaths_est)/colSums(A)+1))
    tau <- 0.1
    MH_obj_GP <- MH_setup()
    MH_obj_GP$sigma <- 0.01
    MH_obj_GP$theta <- Start_theta
    n_theta <- length(Start_theta)
    L <- toeplitz(c(-1,1, rep(0,n_theta-2)))
    L[lower.tri(L,diag=F)] <- 0
    L <- L[-n_theta,]
    L<-as(L, "sparseMatrix")

    Gp.prior <- matrix(NA, nrow=MCMC_sim, ncol=1)

    MH_obj_GP$Lik <- function(x, N, L, phi, A, theta_prior) {
        prior_list <- prior_GP_p(x, L, theta_prior)
        lik_list   <- lik_negbin_theta_A(N, x, phi, A)
        return(list(loglik = lik_list$loglik + prior_list$loglik,
                    grad = as.vector(lik_list$grad  + prior_list$grad)))
    }


    Alpha <- matrix(NA, ncol=N_T, nrow=N_T)
    Beta <- matrix(NA, ncol=N_T, nrow=N_T)
    P <- matrix(NA, ncol=N_T, nrow=N_T)
    theta_GP <-  matrix(NA, nrow=MCMC_sim, ncol = length(MH_obj_GP$theta))
    GPprior <- rep(0, MCMC_sim)
    Death_est <- matrix(NA, nrow=MCMC_sim, ncol=N_T)
    alpha.MCMC <- rep(4, N_T)

    phis <- rep(0, MCMC_sim)
    p_vec <- rep(0, MCMC_sim)
    pi_vec <- rep(0, MCMC_sim)


    burnin <- ceiling(burnin_p*MCMC_sim)


    data_ <-newDeaths(deaths_est,
                      report$Reported_O,
                      maxusage.day)


    beta_mu    <- alpha_Beta$alpha_X
    beta_M     <- alpha_Beta$beta_X
    mu <- 1/(1+exp(-X_mu%*%beta_mu))
    M  <- exp(X_M%*%beta_M)
    Alpha[upper.tri(data_$report.new,diag=T)] <- M * mu
    Beta[upper.tri(data_$report.new,diag=T)]  <- M * (1-mu)


    for(i in 1:(MCMC_sim+burnin-1)){

        if(i%%100==0){
            cat('*')
        }
        if(i%%1000==0){
            cat('+')
        }
        if(i%%10000==0){
            cat(' ',i/10000,' ')
        }




        #replace
        res <- sample.deathsBB_negbin(deaths_sim,
                                  deaths_est,
                                  Alpha,
                                  Beta,
                                  report$Reported_O,
                                  alpha.MCMC,
                                  true.day = true.day,
                                  theta = as.vector(exp(A%*%MH_obj_GP$theta)),
                                  phi  =phi)
        deaths_est <- res$deaths
        data_ <-newDeaths(deaths_est,
                          report$Reported_O,
                          maxusage.day)
        ##
        # build tau sampler
        ##

        L_theta          <- L%*%MH_obj_GP$theta
        #sigma2 <- rGIG(p = -length(L_theta)/2 +2,0.01,sum(L_theta^2))
        tau   <- 100 #1/sigma2 #rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        L_in <- sqrt(tau) * L

        MH_obj_GP <- MALAiter(MH_obj_GP, TRUE,
                              N = deaths_est,
                              L = L_in,
                              phi = phi,
                              A = A,
                              theta_prior  =theta_prior)
        phi_res <- sample.phi(phi,
                              deaths_est,
                              as.vector(A%*%MH_obj_GP$theta),
                              alpha = alpha.phi)
        phi <- phi_res$phi

        if(i < burnin){
            alpha.phi <- max(1,alpha.phi + (2*(phi_res$acc/phi.samples > 0.3)  - 1))
            alpha.MCMC[res$acc/deaths_sim > 0.3] <- alpha.MCMC[res$acc/deaths_sim > 0.3] +1
            alpha.MCMC[res$acc/deaths_sim < 0.3] <- alpha.MCMC[res$acc/deaths_sim < 0.3] -1
            alpha.MCMC[alpha.MCMC<1] <- 1
        }else{
            Death_est[i-burnin + 1,] <-res$deaths
            theta_GP[i-burnin + 1,] <-  as.vector(MH_obj_GP$theta)
            Gp.prior[i-burnin + 1]  <- tau
            phis[i-burnin + 1]  <- phi
        }
    }
    res_save <- list(Death_est = Death_est,
                     theta_GP = theta_GP,
                     true.day = true.day,
                     maxusage.day = maxusage.day,
                     GP.prior      = Gp.prior,
                     phis = phis,
                     lag = lag,
                     j = j,
                     A=  A,
                     npars=  npars)
    return(res_save)
}


##
#' building a fast version of BetaGP where the intial value
#' using previous information as:
#' theta ~ N(\mu_0,\sigma_0^2)
#' of the GP has x ~ N(\mu_0,\sigma_0^2) representing the
#' @param lag         - (int)  lag days  (lag = 0 is only first non-holiday, lag = 1 two non holidays, etc..)
#' @param alpha_beta  - (param)
#' @param npar        - (int) how many lag used to build BB
#' @param theta_prior - (2x1) mean variance
#' @param MCMC_sim    - (int) how many samples to use
#' @param burnin_p    - ([0,1]) propotion to use as burnin
#' @param deaths_sim  -  (int) internal loop for sample N
##
BetaGP_lag_fast <- function(result,
                            alpha_beta,
                            lag,
                            npar,
                            theta_prior,
                            MCMC_sim  = 1000,
                            burnin_p = 0.5,
                            deaths_sim = 30){

    N_T <- length(result$dates)
    #deaths_est_T <- apply(result$detected, 1, max, na.rm=T)
    report <- splitlag(result$detected,
                       as.Date(result$dates_report),
                       lag,
                       result$dates_not_reported)
    X_T <- setup_data_lag(N_T,
                          npar,
                          result$dates_report,
                          npar)


    deaths_est <- apply(report$Reported_T, 1, max, na.rm=T)
    deaths_est[deaths_est==-Inf] = 0
    ##
    # seting up MCMC GP
    ##

    phi.samples <- 10
    phi <- 100
    alpha.phi <- 10
    tau <- 0.1

    ###
    # A -matrix
    ###
    pool <- 1
    A_m  <- ceiling(N_T/pool)
    A_i <- N_T/pool
    A_j <- rep(1:A_m,each=pool)[1:N_T]
    A   <- sparseMatrix(i=1:N_T,j=A_j,  dims=c(N_T, A_m))
    Start_theta <- as.vector(log((t(A)%*%deaths_est)/colSums(A)+1))
    MH_obj_GP <- MH_setup()
    MH_obj_GP$sigma <- 0.1

    MH_obj_GP$theta <- Start_theta
    n_theta <- length(Start_theta)
    L <- toeplitz(c(-1,1, rep(0,n_theta-2)))
    L[lower.tri(L,diag=F)] <- 0
    L <- L[-n_theta,]
    L<-as(L, "sparseMatrix")
    MH_obj_GP$Lik <- function(x, N, L, phi, A, theta_prior) {
        prior_list <- prior_GP_p(x, L, theta_prior)
        lik_list   <- lik_negbin_theta_A(N, x, phi, A)
        return(list(loglik = lik_list$loglik + prior_list$loglik,
                    grad = as.vector(lik_list$grad  + prior_list$grad)))
    }
    Alpha <- matrix(NA, ncol=N_T, nrow=N_T)
    Beta <- matrix(NA, ncol=N_T, nrow=N_T)
    P <- matrix(NA, ncol=N_T, nrow=N_T)
    theta_GP <-  matrix(NA, nrow=MCMC_sim, ncol = length(MH_obj_GP$theta))
    GPprior <- rep(0, MCMC_sim)
    Death_est <- matrix(NA, nrow=MCMC_sim, ncol=N_T)
    alpha.MCMC <- rep(4, N_T)
    phis <- rep(0, MCMC_sim)


    burnin <- ceiling(burnin_p*MCMC_sim)


    data_ <-newDeaths(deaths_est,
                      report$Reported_T,
                      Inf)
    mu <-  1/(1+exp(-X_T%*%alpha_beta$alpha_X))
    M  <- exp(X_T%*%alpha_beta$beta_X)
    Alpha[upper.tri(data_$report.new,diag=T)] <- M*mu
    Beta[upper.tri(data_$report.new,diag=T)]  <- M*(1-mu)
    MU <- Alpha
    MM <- Beta
    MU[upper.tri(data_$report.new,diag=T)]  <- mu
    MM[upper.tri(data_$report.new,diag=T)]  <- M


    for(i in 1:(MCMC_sim+burnin-1)){
        if(i%%100==0){
            cat('*')
        }
        if(i%%1000==0){
            cat('+')
        }
        if(i%%10000==0){
            cat(' ',i/10000,' ')
        }


        #prior_N <- function(N, i){1}
        prior_N <- function(N, i){ dnegbin(N, as.vector(exp(A%*%MH_obj_GP$theta))[i],phi) }
        res <- sample.deathsBB.lag(deaths_sim,
                                   deaths_est,
                                   Alpha,
                                   Beta,
                                   report$Reported_T,
                                   alpha.MCMC,
                                   lag = lag,
                                   dates = as.Date(result$dates_report),
                                   use.prior=T,
                                   prior=prior_N)
        deaths_est <- res$deaths
        data_ <-newDeaths(deaths_est,
                          report$Reported_T,
                          Inf)

        ##
        # build tau sampler
        ##

        L_theta          <- L%*%MH_obj_GP$theta

        sigma2 <- 1#rGIG(p = -length(L_theta)/2 +1,0.001,sum(L_theta^2))
        tau   <- 100 #rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        #tau   <- rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        L_in <- sqrt(tau) * L

        MH_obj_GP <- MALAiter(MH_obj_GP, TRUE,
                              N = deaths_est,
                              L = L_in,
                              phi = phi,
                              A = A,
                              theta_prior = theta_prior)
        phi_res <- sample.phi(phi,
                              deaths_est,
                              as.vector(A%*%MH_obj_GP$theta),
                              alpha = alpha.phi)
        phi <- phi_res$phi

        if(i < burnin){
            alpha.phi <- max(1,alpha.phi + (2*(phi_res$acc/phi.samples > 0.3)  - 1))
            alpha.MCMC[res$acc/deaths_sim > 0.3] <- alpha.MCMC[res$acc/deaths_sim > 0.3] +1
            alpha.MCMC[res$acc/deaths_sim < 0.3] <- alpha.MCMC[res$acc/deaths_sim < 0.3] -1
            alpha.MCMC[alpha.MCMC<1] <- 1
        }else{
            Death_est[i-burnin + 1,] <-res$deaths
            theta_GP[i-burnin + 1,] <-  as.vector(MH_obj_GP$theta)
            GPprior[i-burnin + 1]  <- tau
            phis[i-burnin + 1] <- phi
        }
    }
    res_save <- list(Death_est = Death_est,
                     theta = c(alpha_beta$alpha_X,alpha_beta$beta_X),
                     theta_GP = theta_GP,
                     lag       = lag,
                     GPprior      = GPprior,
                     phis = phis,
                     date  = result$dates_report[N_T],
                     A = A,
                     npar=npar)
    return(res_save)

}



##
#' running the benchmark for day j
#'
#' j - day j
#' result    - (output) of data
#' true.day  - (int) days not estimated
#'  lag    - (int)   lag days  (lag = 0 is only first non-holiday, lag = 1 two non holidays)
#' MCMC_sim     - (int) how many simulations
#' burnin_p     - ([0,1]) how many burnins to run in percantage of MCMC_sim
#' prior        - (1 ) [1] 0: rw1, 1: rw2,
##
benchmark_BetaGP_lag_j <-function(j,
                                  result,
                                  lag,
                                  MCMC_sim,
                                  burnin_p,
                                  deaths_sim,
                                  prior = c(0),
                                  npar=2){
    require(Matrix)
    N_T <- length(result$dates)
    deaths_est_T <- apply(result$detected, 1, max, na.rm=T)
    result_j <- result
    result_j$detected     <- result_j$detected[1:j,1:j]
    result_j$dates        <- result_j$dates[1:j]
    result_j$dates_report <-  result_j$dates_report[1:j]
    report_j <- splitlag(result_j$detected,as.Date(result_j$dates_report), lag)
    param<- ML_betaBin(result_j,
                       lag,
                       npar)
    N_j <- j
    X_T <- setup_data_lag(N_T, npar, result$dates_report, npar)
    X_j <- setup_data_lag(N_j, npar, result_j$dates_report, npar)


    deaths_est <- apply(report_j$Reported_T, 1, max, na.rm=T)
    ##
    # seting up MCMC GP
    ##

    phi.samples <- 10
    phi <- 100
    alpha.phi <- 10

    ###
    # A -matrix
    ###
    pool <- 1
    A_m  <- ceiling(N_j/pool)
    A_i <- N_j/pool
    A_j <- rep(1:A_m,each=pool)[1:N_j]
    A   <- sparseMatrix(i=1:N_j,j=A_j,  dims=c(N_j, A_m))
    Start_theta <- as.vector(log((t(A)%*%deaths_est)/colSums(A)+1))
    tau <- 0.1
    MH_obj_GP <- MH_setup()
    MH_obj_GP$sigma <- 0.1

    MH_obj_GP$theta <- Start_theta
    if(prior == 0){
        n_theta <- length(Start_theta)
        L <- toeplitz(c(-1,1, rep(0,n_theta-2)))
        L[lower.tri(L,diag=F)] <- 0
        L <- L[-n_theta,]
        L<-as(L, "sparseMatrix")
    }else{
        n_theta <- length(Start_theta)
        L <- toeplitz(c(-1,2,-1, rep(0,n_theta-3)))
        L[lower.tri(L,diag=F)] <- 0
        L <- L[-c(n_theta,n_theta-1),]
        L<-as(L, "sparseMatrix")
    }
    MH_obj_GP$Lik <- function(x, N, L, phi, A) {
        prior_list <- prior_GP(x, L)
        lik_list   <- lik_negbin_theta_A(N, x, phi, A)
        return(list(loglik = lik_list$loglik + prior_list$loglik,
                    grad = as.vector(lik_list$grad  + prior_list$grad)))
    }


    Alpha <- matrix(NA, ncol=N_j, nrow=N_j)
    Beta <- matrix(NA, ncol=N_j, nrow=N_j)
    P <- matrix(NA, ncol=N_j, nrow=N_j)
    theta_GP <-  matrix(NA, nrow=MCMC_sim, ncol = length(MH_obj_GP$theta))
    GPprior <- rep(0, MCMC_sim)
    Death_est <- matrix(NA, nrow=MCMC_sim, ncol=N_j)
    alpha.MCMC <- rep(4, N_j)
    pred_set <-array(NA, dim = c(j,N_T-j,MCMC_sim))
    phis <- rep(0, MCMC_sim)


    burnin <- ceiling(burnin_p*MCMC_sim)


    data_ <-newDeaths(deaths_est,
                      report_j$Reported_T,
                      Inf)
    mu <-  1/(1+exp(-X_j%*%param$alpha_X))
    M  <- exp(X_j%*%param$beta_X)
    Alpha[upper.tri(data_$report.new,diag=T)] <- M*mu
    Beta[upper.tri(data_$report.new,diag=T)]  <- M*(1-mu)
    MU <- Alpha
    MM <- Beta
    MU[upper.tri(data_$report.new,diag=T)]  <- mu
    MM[upper.tri(data_$report.new,diag=T)]  <- M
    for(i in 1:(MCMC_sim+burnin-1)){

        if(i%%100==0){
            cat('*')
        }
        if(i%%1000==0){
            cat('+')
        }
        if(i%%10000==0){
            cat(' ',i/10000,' ')
        }


        #prior_N <- function(N, i){1}
        prior_N <- function(N, i){ dnegbin(N, as.vector(exp(A%*%MH_obj_GP$theta))[i],phi) }
        res <- sample.deathsBB.lag(deaths_sim,
                               deaths_est,
                               Alpha,
                               Beta,
                               report_j$Reported_T,
                               alpha.MCMC,
                               lag = lag,
                               dates = as.Date(result_j$dates_report),
                               use.prior=T,
                               prior=prior_N)
        deaths_est <- res$deaths
        data_ <-newDeaths(deaths_est,
                          report_j$Reported_T,
                          Inf)

        ##
        # build tau sampler
        ##

        L_theta          <- L%*%MH_obj_GP$theta

        sigma2 <- rGIG(p = -length(L_theta)/2 +1,0.001,sum(L_theta^2))
        tau   <- 1/sigma2 #rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        #tau   <- rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        L_in <- sqrt(tau) * L

        MH_obj_GP <- MALAiter(MH_obj_GP, TRUE,
                              N = deaths_est,
                              L = L_in,
                              phi = phi,
                              A = A)
        phi_res <- sample.phi(phi,
                              deaths_est,
                              as.vector(A%*%MH_obj_GP$theta),
                              alpha = alpha.phi)
        phi <- phi_res$phi

        if(i < burnin){
            alpha.phi <- max(1,alpha.phi + (2*(phi_res$acc/phi.samples > 0.3)  - 1))
            alpha.MCMC[res$acc/deaths_sim > 0.3] <- alpha.MCMC[res$acc/deaths_sim > 0.3] +1
            alpha.MCMC[res$acc/deaths_sim < 0.3] <- alpha.MCMC[res$acc/deaths_sim < 0.3] -1
            alpha.MCMC[alpha.MCMC<1] <- 1
        }else{
            Death_est[i-burnin + 1,] <-res$deaths
            theta_GP[i-burnin + 1,] <-  as.vector(MH_obj_GP$theta)
            GPprior[i-burnin + 1]  <- tau
            phis[i-burnin + 1] <- phi
        }
    }
    res_save <- list(Death_est = Death_est,
                     theta = c(param$alpha_X,param$beta_X),
                     theta_GP = theta_GP,
                     lag       = lag,
                     GPprior      = GPprior,
                     phis = phis,
                     date  = result$dates_report[j],
                     j = j,
                     A = A,
                     npar=npar)
    return(res_save)

}


##
#' running the benchmark for day j
#'
#' j - day j
#' result -
#' true.day     - (int) days not estimated
#'  lag         - (int)   lag days  (lag = 0 is only first non-holiday, lag = 1 two non holidays)
#' maxusage.day - (int) how many reports days to use for each death
#' MCMC_sim     - (int) how many simulations
#' burnin_p     - ([0,1]) how many burnins to run in percantage of MCMC_sim
#' prior        - (2 x 1) [1] 0: rw1, 1: rw2
##
benchmark_BetaGP_post_lag_j <- function(j,
                                        result,
                                        lag,
                                        true.day,
                                        maxusage.day,
                                        MCMC_sim,
                                        burnin_p,
                                        deaths_sim,
                                        prior = 0,
                                        npars = 2){
    require(Matrix)
    N_T <- length(result$dates)
    deaths_est_T <- apply(result$detected, 1, max, na.rm=T)
    result_j <- result
    result_j$detected     <- result_j$detected[1:j,1:j]
    result_j$dates        <- result_j$dates[1:j]
    result_j$dates_report <-  result_j$dates[1:j]
    report_j <- splitlag(result_j$detected,as.Date(result_j$dates_report), lag)
    N_j <- j

    X_j <- setup_data_postlag2(j, lag, npars, result_j$dates_report)
    X_M <- setup_data_postlag2(j, lag, npars, result_j$dates_report)
    deaths_est <- apply(report_j$Reported_O, 1, function(x) { if(all(is.na(x))){return(0)};
                                                            max(x,na.rm=T)})
    X_mu <- X_j
    #X_mixed <- setup_data_mixed_effect(N, 2, result$dates_report[1:N])

    p  <- dim(X_mu)[2]
    #X_M  <- cbind(rep(1,dim(X_mu)[1]),rowSums(X_mu[,4:5])>0)
    p1 <- dim(X_M)[2]


    prob_null = 0.02
    pi_null   = 0.1
    z_null    = NA
    ##
    # seting up MCMC
    ##
    MH_obj <- MH_setup()
    MH_obj$sigma <- 0.1
    MH_obj$theta <- rep(0,p + p1 )

    MH_obj$Lik <- loglikProbBB_bi
    sigma_mixed <- 1

    ##
    # seting up MCMC GP
    ##

    phi.samples <- 10
    phi <- 100
    alpha.phi <- 10
    ###
    # A -matrix
    ###
    pool <- 1
    A_m  <- ceiling(N_j/pool)
    A_j <- rep(1:A_m,each=pool)[(A_m*pool - N_j + 1):(A_m*pool)]
    A   <- sparseMatrix(i=1:N_j,j=A_j,  dims=c(N_j, A_m))
    Start_theta <- as.vector(log((t(A)%*%deaths_est)/colSums(A)+1))
    tau <- 0.1
    MH_obj_GP <- MH_setup()
    MH_obj_GP$sigma <- 0.01
    MH_obj_GP$theta <- Start_theta
    if(prior[1] == 0){
        n_theta <- length(Start_theta)
        L <- toeplitz(c(-1,1, rep(0,n_theta-2)))
        L[lower.tri(L,diag=F)] <- 0
        L <- L[-n_theta,]
        L<-as(L, "sparseMatrix")
    }else{
        n_theta <- length(Start_theta)
        L <- toeplitz(c(-1,2,-1, rep(0,n_theta-3)))
        L[lower.tri(L,diag=F)] <- 0
        L <- L[-c(n_theta,n_theta-1),]
        L<-as(L, "sparseMatrix")
    }
    Gp.prior <- matrix(NA, nrow=MCMC_sim, ncol=1)

    MH_obj_GP$Lik <- function(x, N, L, phi, A) {
        prior_list <- prior_GP(x, L)
        lik_list   <- lik_negbin_theta_A(N, x, phi, A)
        return(list(loglik = lik_list$loglik + prior_list$loglik,
                    grad = as.vector(lik_list$grad  + prior_list$grad)))
    }


    Alpha <- matrix(NA, ncol=N_j, nrow=N_j)
    Beta <- matrix(NA, ncol=N_j, nrow=N_j)
    P <- matrix(NA, ncol=N_j, nrow=N_j)
    theta_GP <-  matrix(NA, nrow=MCMC_sim, ncol = length(MH_obj_GP$theta))
    Thetas <- matrix(NA, nrow=MCMC_sim, ncol = length(MH_obj$theta))
    GPprior <- rep(0, MCMC_sim)
    Death_est <- matrix(NA, nrow=MCMC_sim, ncol=N_j)
    alpha.MCMC <- rep(4, j)
    pred_set <-array(NA, dim = c(j,N_T-j,MCMC_sim))
    phis <- rep(0, MCMC_sim)
    p_vec <- rep(0, MCMC_sim)
    pi_vec <- rep(0, MCMC_sim)


    burnin <- ceiling(burnin_p*MCMC_sim)


    data_ <-newDeaths(deaths_est,
                      report_j$Reported_O,
                      maxusage.day)

    for(i in 1:(MCMC_sim+burnin-1)){

        if(i%%100==0){
            cat('*')
        }
        if(i%%1000==0){
            cat('+')
        }
        if(i%%10000==0){
            cat(' ',i/10000,' ')
        }


        MH_obj <- MALAiter(MH_obj, TRUE,
                           death.remain = data_$death.remain,
                           report.new   = data_$report.new,
                           X_mu         = X_mu,
                           X_M          = X_M,
                           p_null       = prob_null,
                           pi_null      = pi_null,
                           z            = z_null)

        pi_null <- MH_obj$res$pi_null
        z_null  <- MH_obj$res$z
        p_null  <- MH_obj$res$p_null

        beta_mu    <- MH_obj$theta[1:p]
        beta_M     <- MH_obj$theta[p + (1:p1)]
        mu <- 1/(1+exp(-X_mu%*%beta_mu))
        M  <- exp(X_M%*%beta_M)
        Alpha[upper.tri(data_$report.new,diag=T)] <- M * mu
        Beta[upper.tri(data_$report.new,diag=T)]  <- M * (1-mu)



        prior_N <- function(N, i){ dnegbin(N, as.vector(exp(A%*%MH_obj_GP$theta))[i],phi) }
        res <- sample.deathsBB_bi(deaths_sim,
                               deaths_est,
                               Alpha,
                               Beta,
                               report_j$Reported_O,
                               alpha.MCMC,
                               true.day = true.day,
                               p_null = p_null,
                               pi_null = pi_null,
                               use.prior=T,
                               prior=prior_N)
        deaths_est <- res$deaths
        data_ <-newDeaths(deaths_est,
                          report_j$Reported_O,
                          maxusage.day)
        ##
        # build tau sampler
        ##

        L_theta          <- L%*%MH_obj_GP$theta
        sigma2 <- rGIG(p = -length(L_theta)/2 +2,0.01,sum(L_theta^2))
        tau   <- 1/sigma2 #rgamma(1, shape=length(L_theta)/2 + 0.001, scale = sum(L_theta^2)/2 + 0.001)
        L_in <- sqrt(tau) * L

        MH_obj_GP <- MALAiter(MH_obj_GP, TRUE,
                              N = deaths_est,
                              L = L_in,
                              phi = phi,
                              A = A)
        phi_res <- sample.phi(phi,
                              deaths_est,
                              as.vector(A%*%MH_obj_GP$theta),
                              alpha = alpha.phi)
        phi <- phi_res$phi

        if(i < burnin){
            alpha.phi <- max(1,alpha.phi + (2*(phi_res$acc/phi.samples > 0.3)  - 1))
            alpha.MCMC[res$acc/deaths_sim > 0.3] <- alpha.MCMC[res$acc/deaths_sim > 0.3] +1
            alpha.MCMC[res$acc/deaths_sim < 0.3] <- alpha.MCMC[res$acc/deaths_sim < 0.3] -1
            alpha.MCMC[alpha.MCMC<1] <- 1
        }else{
            p_vec[i-burnin + 1] <- p_null
            pi_vec[i-burnin + 1] <- pi_null
            Thetas[i-burnin + 1,] <- MH_obj$theta
            Death_est[i-burnin + 1,] <-res$deaths
            theta_GP[i-burnin + 1,] <-  as.vector(MH_obj_GP$theta)
            Gp.prior[i-burnin + 1]  <- tau
            phis[i-burnin + 1]  <- phi
        }
    }
    res_save <- list(Thetas = Thetas,
                     Death_est = Death_est,
                     theta_GP = theta_GP,
                     true.day = true.day,
                     maxusage.day = maxusage.day,
                     GP.prior      = Gp.prior,
                     phis = phis,
                     p_vec = p_vec,
                     pi_vec = pi_vec,
                     date  = result$dates_report[j],
                     lag = lag,
                     j = j,
                     A=  A,
                     npars=  npars)
    return(res_save)
}

