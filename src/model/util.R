require(Matrix)
holidays.Sweden <- as.Date(c("2020-04-10","2020-04-13","2020-05-01","2020-05-21","2020-06-19","2020-06-20",
                             "2020-12-23","2020-12-24","2020-12-25","2020-12-31","2021-01-01","2021-01-06"))

##
# sample reported deaths after rep.day (if Inf) predicit day
# assuming Multionimal distribution
#
#  samples  - (int) how many samples should be done for each N (cheap)
#  deaths   - (N x 1) true number of deaths each day
#  P        - (N x N) matrix of probabilites (only upper triangular part relevant)
#  Reported - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
#  alpha    - (N x 1) stepsizes
#  true.dat  - (int) how many days after recording should one sample
##
sample.deaths <- function(samples, deaths, P, Reported, alpha, true.day = 0,
                          use.prior=FALSE,
                          prior=NULL){

  N <- length(deaths)
  acc <- rep(0,N)
  for(i in 1:N){
    if(i > true.day){
      P_i         = P[i,i:N]
      Reported_i  = Reported[i,i:N]
      index = is.na(Reported_i)==F
      P_i         = P_i[index]
      Reported_i  = Reported_i[index]
      lik_i <- loglikDeathsGivenProb(deaths[i], P_i, Reported_i)
      if(use.prior==T)
        lik_i <- lik_i + prior(deaths[i], i)
      for(j in 1:samples){
        death_star <- sample((deaths[i]-alpha[i]):(deaths[i]+alpha[i]), 1)
        lik_star <- loglikDeathsGivenProb(death_star,  P_i, Reported_i)
        if(use.prior==T)
          lik_star <- lik_star + prior(death_star, i)
        if(is.nan(lik_star))
          next

        if(log(runif(1)) < lik_star-lik_i){
          lik_i = lik_star
          deaths[i] <- death_star
          acc[i] = acc[i] + 1
        }
      }
    }
  }
  return(list(deaths=deaths, acc = acc))
}


##
# sample reported deaths after rep.day (if Inf) predicit day using Beta binomial dist
#
#  samples  - (int) how many samples should be done for each N (cheap)
#  deaths   - (N x 1) true number of deaths each day
#  alpha     - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  beta      - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  Reported - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
#  alpha.MCMC    - (N x 1) stepsizes
#  true.dat  - (int) how many days after recording should one sample
#  prior     - (function) should  return log of prior density (use N and i)
##
sample.deathsBB <- function(samples, deaths, alpha, beta, Reported, alpha.MCMC, true.day = 0 ,
                            use.prior=FALSE,
                            prior=NULL){

  N <- length(deaths)
  acc <- rep(0,N)
  for(i in 1:N){
    if(i > true.day){
      alpha_i     = alpha[i,i:N]
      beta_i      = beta[i,i:N]
      Reported_i  = Reported[i,i:N]
      index = is.na(Reported_i)==F
      alpha_i  = alpha_i[index]
      beta_i  = beta_i[index]
      Reported_i  = Reported_i[index]
      if(length(Reported_i)>0){
        lik_i <- loglikDeathsGivenProbBB(deaths[i],alpha_i , beta_i, Reported_i)
      }else{
        lik_i <- 0
      }
      if(use.prior==T)
        lik_i <- lik_i + prior(deaths[i], i)
      for(j in 1:samples){
        death_star <- sample((deaths[i]-alpha.MCMC[i]):(deaths[i]+alpha.MCMC[i]), 1)
        if(length(Reported_i)>0){
          lik_star <- loglikDeathsGivenProbBB(death_star, alpha_i , beta_i, Reported_i)
        }else{
          lik_star <- 0
        }
        if(use.prior==T)
          lik_star <- lik_star + prior(death_star, i)
        if(is.nan(lik_star))
          next
        if(log(runif(1)) < lik_star-lik_i){
          lik_i = lik_star
          deaths[i] <- death_star
          acc[i] = acc[i] + 1
        }
      }
    }
  }
  return(list(deaths=deaths, acc = acc))
}

##
# sample reported deaths after rep.day (if Inf) predicit day using Beta binomial dist
#
#  samples  - (int) how many samples should be done for each N (cheap)
#  deaths   - (N x 1) true number of deaths each day
#  alpha     - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  beta      - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  Reported - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
#  alpha.MCMC    - (N x 1) stepsizes
#  true.dat  - (int) how many days after recording should one sample
#  prior     - (function) should  return log of prior density (use N and i)
##
sample.deathsBB_bi <- function(samples, deaths, alpha, beta, Reported, alpha.MCMC, true.day = 0 ,
                               p_null,
                               pi_null,
                            use.prior=FALSE,
                            prior=NULL){

  N <- length(deaths)
  acc <- rep(0,N)
  for(i in 1:N){
    if(i > true.day){
      alpha_i     = alpha[i,i:N]
      beta_i      = beta[i,i:N]
      Reported_i  = Reported[i,i:N]
      index = is.na(Reported_i)==F
      alpha_i  = alpha_i[index]
      beta_i  = beta_i[index]
      Reported_i  = Reported_i[index]
      if(length(Reported_i)>0){
        lik_i <- loglikDeathsGivenProbBB_bi(deaths[i],alpha_i , beta_i, pi_null,p_null, Reported_i)
      }else{
        lik_i <- 0
      }
      if(use.prior==T)
        lik_i <- lik_i + prior(deaths[i], i)
      for(j in 1:samples){
        death_star <- sample((deaths[i]-alpha.MCMC[i]):(deaths[i]+alpha.MCMC[i]), 1)
        if(length(Reported_i)>0){
          lik_star <- loglikDeathsGivenProbBB_bi(death_star, alpha_i , beta_i, pi_null,p_null, Reported_i)
        }else{
          lik_star <- 0
        }
        if(use.prior==T)
          lik_star <- lik_star + prior(death_star, i)
        if(is.nan(lik_star))
          next
        if(log(runif(1)) < lik_star-lik_i){
          lik_i = lik_star
          deaths[i] <- death_star
          acc[i] = acc[i] + 1
        }
      }
    }
  }
  return(list(deaths=deaths, acc = acc))
}


##
# sample reported deaths after rep.day (if Inf) predicit day using Beta binomial dist
#
#  samples  - (int) how many samples should be done for each N (cheap)
#  deaths   - (N x 1) true number of deaths each day
#  alpha     - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  beta      - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  Reported - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
#  alpha.MCMC    - (N x 1) stepsizes
#  true.dat  - (int) how many days after recording should one sample
#  prior     - (function) should  return log of prior density (use N and i)
##
sample.deathsBB_negbin <- function(samples, deaths, alpha, beta, Reported, alpha.MCMC, true.day = 0 ,
                                   theta,
                                   phi){

  N <- length(deaths)
  acc <- rep(0,N)
  for(i in 1:N){
    if(i > true.day){
      alpha_i     = alpha[i,i:N]
      beta_i      = beta[i,i:N]
      Reported_i  = Reported[i,i:N]
      index = is.na(Reported_i)==F
      alpha_i  = alpha_i[index]
      beta_i  = beta_i[index]
      Reported_i  = Reported_i[index]
      if(length(Reported_i)>0){
        lik_i <- loglikDeathsGivenProbBB(deaths[i],alpha_i , beta_i, Reported_i)
      }else{
        lik_i <- 0
      }
      lik_i <- lik_i + dnegbin(deaths[i], theta[i],phi)
      for(j in 1:samples){
        death_star <- sample((deaths[i]-alpha.MCMC[i]):(deaths[i]+alpha.MCMC[i]), 1)
        if(length(Reported_i)>0){
          lik_star <- loglikDeathsGivenProbBB(death_star, alpha_i , beta_i, Reported_i)
        }else{
          lik_star <- 0
        }
        lik_star <- lik_star + dnegbin(death_star, theta[i],phi)
        if(is.nan(lik_star))
          next
        if(log(runif(1)) < lik_star-lik_i){
          lik_i = lik_star
          deaths[i] <- death_star
          acc[i] = acc[i] + 1
        }
      }
    }
  }
  return(list(deaths=deaths, acc = acc))
}


##
# sample reported deaths after rep.day (if Inf) predicit day using Beta binomial dist
#
#  samples  - (int) how many samples should be done for each N (cheap)
#  deaths   - (N x 1) true number of deaths each day
#  alpha     - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  beta      - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  Reported - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
#  alpha.MCMC    - (N x 1) stepsizes
#  lag  - (int) how many days after recording should one sample
#  prior     - (function) should  return log of prior density (use N and i)
##
sample.deathsBB.lag <- function(samples, deaths, alpha, beta, Reported, alpha.MCMC, lag ,
                                dates,
                            use.prior=FALSE,
                            prior=NULL){

  N <- length(deaths)
  acc <- rep(0,N)
  holidays <- weekdays(dates)%in%c("Sunday","Saturday") | c(dates)%in%c(holidays.Sweden)
  for(i in 1:N){
    k <- which.max(cumsum(holidays[i:N]==F)==(lag+1))-1
    if(k==0){
      alpha_i     = alpha[i,i:N]
      beta_i      = beta[i,i:N]
      Reported_i  = Reported[i,i:N]
      index = is.na(Reported_i)==F
      alpha_i  = alpha_i[index]
      beta_i  = beta_i[index]
      Reported_i  = Reported_i[index]
      if(length(Reported_i)>0){
        lik_i <- loglikDeathsGivenProbBB(deaths[i],alpha_i , beta_i, Reported_i)
      }else{
        lik_i <- 0
      }
      if(use.prior==T)
        lik_i <- lik_i + prior(deaths[i], i)
      for(j in 1:samples){
        death_star <- sample((deaths[i]-alpha.MCMC[i]):(deaths[i]+alpha.MCMC[i]), 1)
        if(length(Reported_i)>0){
          lik_star <- loglikDeathsGivenProbBB(death_star, alpha_i , beta_i, Reported_i)
        }else{
          lik_star <- 0
        }
        if(use.prior==T)
          lik_star <- lik_star + prior(death_star, i)
        if(is.nan(lik_star))
          next
        if(log(runif(1)) < lik_star-lik_i){
          lik_i = lik_star
          deaths[i] <- death_star
          acc[i] = acc[i] + 1
        }
      }
    }
  }
  return(list(deaths=deaths, acc = acc))
}

##
# fill report using binimoal beta
#
#
##
fill.ReportBB <- function(deaths, Alpha, Beta, Reported, maxusage.day){
  N <- length(deaths)
  N_2 <- dim(Alpha)[2]
  for(i in 1:N){
      Alpha_i         = Alpha[i,i:N_2]
      Beta_i          = Beta[i,i:N_2]
      Reported_i  = Reported[i,i:N_2]
      index = is.na(Reported_i)==T
      for(j in min(which(index)):length(Reported_i)){
        p <- rbeta(1, Alpha_i[j], Beta_i[j])

        if(i> maxusage.day){
          if(j==1){
            Reported_i[j] <- rbinom(1, size=deaths[i] , prob = p)
          }else if(deaths[i] - Reported_i[j-1]>0){
            Reported_i[j] <- rbinom(1, size=deaths[i] - Reported_i[j-1], prob = p) + Reported_i[j-1]
          }else{Reported_i[j] <- Reported_i[j-1]}
        }else{
        Reported_i[j] <- Reported_i[j-1]
        }
      }
    Reported[i,i:N_2] = Reported_i
  }
  return(Reported)
}


##
# fill report using binimoal beta up to lag
#
# deaths    - (N x 1) number of estimated deaths
# Alpha     - (N x m) parameter for Beta Bin
# Beta      - (N x m) parameter for Beta Bin
# dates     - (m x 1) dates reported
# Reported  - (N x m) reported so far and na for non reported
# lag       - (int)   lag = 0 is only first non-holiday, lag = 1 two non holidays
##
fill.ReportBB.lag <- function(deaths, Alpha, Beta, Reported, dates, lag){
  N <- length(deaths)
  m <- dim(Alpha)[2]
  holidays <- weekdays(dates)%in%c("Sunday","Saturday") | c(dates)%in%c(holidays.Sweden)
  for(i in 1:N){
    k <- which.max(cumsum(holidays[i:m]==F)==(lag+1))-1
    if(k==0)
      k <- m - i
    Alpha_i         = Alpha[i,i:(i+k)]
    Beta_i          = Beta[i,i:(i+k)]
    Reported_i  = Reported[i,i:(i+k)]
    index = is.na(Reported_i)==T
    if(sum(index)==0)
      next

    for(j in min(which(index)):length(Reported_i)){
      p <- rbeta(1, Alpha_i[j], Beta_i[j])
      if(deaths[i] - Reported_i[j-1]>0){
          if(j==(k+1)){
            Reported_i[j] <- deaths[i]
          }else{
            Reported_i[j] <- rbinom(1, size=deaths[i] - Reported_i[j-1], prob = p) + Reported_i[j-1]
          }
        }else{Reported_i[j] <- Reported_i[j-1]}

    }
    Reported[i,i:(i+k)] = Reported_i
  }
  return(Reported)
}

##
# fill report using binimoal beta post lag
#
# deaths       - (N x 1) number of estimated deaths
# Alpha        - (N x m) parameter for Beta Bin
# Beta         -  (N x m) parameter for Beta Bin
# dates        - (m x 1) dates reported
# Reported     - (N x m) reported so far and na for non reported
# lag          - (int)   lag = 0 is only first non-holiday, lag = 1 two non holidays
##
fill.ReportBB.postlag <- function(deaths, Alpha, Beta, Reported, dates, lag){
  N <- length(deaths)
  m <- dim(Alpha)[2]
  holidays <- weekdays(dates)%in%c("Sunday","Saturday") | c(dates)%in%c(holidays.Sweden)
  for(i in 1:N){
    k <- which.max(cumsum(holidays[i:m]==F)==(lag+1))-1
    if(k==0)
      k <- m - i
    if(k+i >= m)
      next

    death_i <- deaths[i] +  Reported[i,(i+k)]
    # use i instead of i+1 to get the latest size
    Alpha_i         = Alpha[i,(i+k):m]
    Beta_i          = Beta[i,(i+k):m]
    Reported_i  = Reported[i,(i+k):m]
    index = is.na(Reported_i)==T
    if(sum(index)==0)
      next

    for(j in min(which(index)):length(Reported_i)){
      p <- rbeta(1, Alpha_i[j], Beta_i[j])
      rep_prev <- Reported_i[j-1]
      if( death_i - rep_prev>0 ){
        Reported_i[j] <- rbinom(1, size=death_i - rep_prev, prob = p) + rep_prev
      }else{Reported_i[j] <- rep_prev}

    }
    Reported[i,(i+k):m] = Reported_i
  }
  return(Reported)
}

##
# fill report using binimoal beta post lag
#
# deaths       - (N x 1) number of estimated deaths
# Alpha        - (N x m) parameter for Beta Bin
# Beta         -  (N x m) parameter for Beta Bin
# dates        - (m x 1) dates reported
# Reported     - (N x m) reported so far and na for non reported
# lag          - (int)   lag = 0 is only first non-holiday, lag = 1 two non holidays
##
fill.ReportBB.postlag_bi <- function(deaths,
                                     Alpha,
                                     Beta,
                                     pi_null,
                                     p_null,
                                     Reported,
                                     dates,
                                     lag){
  N <- length(deaths)
  m <- dim(Alpha)[2]
  holidays <- weekdays(dates)%in%c("Sunday","Saturday") | c(dates)%in%c(holidays.Sweden)
  for(i in 1:N){
    k <- which.max(cumsum(holidays[i:m]==F)==(lag+1))-1
    if(k==0)
      k <- m - i
    if(k+i >= m)
      next

    death_i <- deaths[i] +  Reported[i,(i+k)]
    # use i instead of i+1 to get the latest size
    Alpha_i         = Alpha[i,(i+k):m]
    Beta_i          = Beta[i,(i+k):m]
    Reported_i  = Reported[i,(i+k):m]
    index = is.na(Reported_i)==T
    if(sum(index)==0)
      next

    for(j in min(which(index)):length(Reported_i)){
      if(runif(1) >pi_null){
        p <- rbeta(1, Alpha_i[j], Beta_i[j])
      }else{
        p <- p_null
      }
      rep_prev <- Reported_i[j-1]
      if( death_i - rep_prev>0 ){
        Reported_i[j] <- rbinom(1, size=death_i - rep_prev, prob = p) + rep_prev
      }else{Reported_i[j] <- rep_prev}

    }
    Reported[i,(i+k):m] = Reported_i
  }
  return(Reported)
}



##
# fill report
#
#
##
fill.Report <- function(deaths, P, Reported, maxusage.day){
  N <- length(deaths)
  N_2 <- dim(P)[2]
  for(i in 1:N){
      P_i         = P[i,i:N_2]
      Reported_i  = Reported[i,i:N_2]
      index = is.na(Reported_i)==T
      for(j in min(which(index)):length(Reported_i)){
        Reported_i[j] <- rbinom(1, size=deaths[i] - Reported_i[j-1], prob =  P_i[j]) + Reported_i[j-1]
        if(i> maxusage.day){
          Reported_i[j] <- rbinom(1, size=deaths[i] - Reported_i[j-1], prob = P_i[j]) + Reported_i[j-1]
        }else{
          Reported_i[j] <- Reported_i[j-1]
        }
      }
      Reported[i,i:N_2] = Reported_i
  }
  return(Reported)
}
###
#
# posterior sampling of deaths given Prob vec
#
# P           - (N x N) probability matrix over probability of detecting reminder
# Reported    - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
# Predict.day - (int) which day to of reporting to predict
# sim         - (2 x 1) MCMC samples inner loop and outer loop
# alpha       - (N x 1) stepsizes
#
###
death.givenProb <- function(P, Reported ,Predict.day = Inf,sim=c(2000,10), alpha = NULL){

  N <- dim(P)[1]
  if(is.null(alpha))
    alpha <- rep(4, N)

  deaths <- matrix(NA, nrow=sim[1], ncol = N)
  deaths_est <- apply(Reported, 1, max, na.rm=T)

  burnin = ceiling(0.3*sim[1])
  for(i in 1:(sim[1] + burnin -1)){

    res <- sample.deaths(sim[2],deaths_est, P, Reported, alpha,rep.day=Predict.day)
    deaths_est <- res$deaths
    if(i < burnin){
      alpha[res$acc/sim[2] > 0.3] <- alpha[res$acc/sim[2] > 0.3] +1
      alpha[res$acc/sim[2] < 0.3] <- alpha[res$acc/sim[2] < 0.3] -1
      alpha[alpha<1] <- 1
    }else{
      deaths[i - burnin +1,] = deaths_est
    }
  }
  return(deaths)
}




##
# log liklihood of obseving report given death and prob
#  deaths  - (int) true number of deaths
#  p       - (n x 1) probability of report
#  report  - (n x 1) reported deaths culmative each date
##
loglikDeathsGivenProb <- function(death, p, report){

  if(death < max(report,na.rm=T))
    return(-Inf)
  n <- length(p)
  if(is.na(report[1])){
    #we dont have data from day one

    ndeaths <- diff(report[is.na(report)==F])
    report_adj <- report[1:(n-1)]
    report_adj <- report_adj[is.na(report_adj)==F]
  }else{
    ndeaths <- c(report[1],diff(report))
    if(n>1){
      report_adj <- c(0, report[1:(n-1)])
    }else{
      report_adj <- 0
    }
  }

  ndeaths[ndeaths <0 ] = 0

  return(sum(dbinom(ndeaths,  death - report_adj,  prob = p[is.na(p)==F], log=T )))
}

##
# build holiday covariates vector for holiday
#
##
#  holidays - (N x 1) true if day is holiday false else
##
buildXholiday <- function(N,holidays){
  ##
  # base matrix
  ##
  index_base <- t(matrix(rep(holidays,N),ncol = N))

  index_base <- index_base[upper.tri(index_base,diag=T)]
  index_base <- 1* index_base
  #sparse matrix
  i_base <- 1:length(index_base)
  i_base <- i_base[index_base==1]
  return(sparseMatrix(j=rep(1,length(i_base)),i=i_base,  dims=c(length(index_base), 1)))
}

buildXall <- function(N){
  ##
  # base matrix
  ##
  index_base <- t(matrix(1:N^2,ncol = N, nrow=N))

  index_base <- index_base[upper.tri(index_base,diag=T)]
  #sparse matrix
  i_base <- 1:length(index_base)
  j_base <- 1:length(index_base)
  return(sparseMatrix(j=j_base,i=j_base))
}

##
# build day effect matrix
#  nDayEffects - number of speical days effect (1- first day, 2- first + second day)
#  N - number of days
#  nDayEffects - how many day covariate effect to create
##
buildXdayeffect <- function(N, nDayEffects = 1){
  nDayEffects <- min(N,nDayEffects)
  index_days <- toeplitz(c( (1:nDayEffects), rep(0,N -nDayEffects)))
  index_days <- index_days[upper.tri(index_days,diag=T)]
  j_ <-  index_days[index_days>0]
  i_base <- 1:length(index_days)
  i_ <-  i_base[index_days>0]
  return(sparseMatrix(i=i_,j=j_ , dims=c(length(index_days), nDayEffects)  ) )
}
##
# build day mixed effect matrix
#  nDayEffects - number of speical days effect (1- first day, 2- first + second day)
#  N - number of days
#  nDayEffects - how many day covariate effect to create
##
buildXmixeddayeffect <- function(N, nDayEffects = 1){

  index = rep(0,N)
  index[nDayEffects] = 1
  index_days <- toeplitz(index)
  index_days <- index_days[upper.tri(index_days,diag=T)]
  j_ <-  1:sum(index_days[index_days>0])
  i_base <- 1:length(index_days)
  i_ <-  i_base[index_days>0]
  return(sparseMatrix(i=i_,j=j_ , dims=c(length(index_days), sum(index_days[index_days>0])))  )
}
##
# building an X matrix such that each day has a fixed effect
#
##
buildXday <- function(N){
  ##
  # base matrix
  ##
  index_base <- t(matrix(rep(1:N,N),ncol = N))
  index_base <- index_base[upper.tri(index_base,diag=T)]
  #sparse matrix
  j_base <- 1:length(index_base)
  #adding mean effect
  j_ <- c(index_base, rep(N+1,length(index_base)))
  i_ <- c(j_base, 1:length(index_base))
  #day effects
  if(nDayEffects > 0){
    index_days <- toeplitz(c( N+1 + (1:nDayEffects), rep(0,N -nDayEffects)))
    index_days <- index_days[upper.tri(index_days,diag=T)]
    j_ <- c(j_, index_days[index_days>0])
    i_ <- c(i_, j_base[index_days>0])
  }
  return(sparseMatrix(i=i_,j=j_))
}
##
#' split data before and after lag
#'
#'
#' Reported -  (N x N) number of reported deaths
#' dates    -  (N x 1) dates of reporting
#' lag      -  (int)   how long after to report (lag = 0 is only first non-holiday, lag = 1 two non holidays,
#' dates_not_reported - (N x 1) dayes with no reporting in
#'
##
splitlag <- function(Reported_T, dates, lag,
                     dates_not_reported){

  holidays <- weekdays(dates)%in%c("Sunday","Saturday") | c(dates)%in%c(holidays.Sweden)
  N <- dim(Reported_T)[1]
  N2 <- dim(Reported_T)[2]
  Reported_O = Reported_T
  day_completed <- rep(1,N)
  for(i in 1:N){
    # which day is lag +1 number of days before holiday
    k <- which.max(cumsum(holidays[i:N2]==F)==(lag+1))-1
    if(k==0)
      k <- N2-i
    Reported_O[i, i:min(i+k, N2)] <- NA
    if(k+i<N2){
      Reported_T_temp =  Reported_T[i,  (i+k)]
      if(is.na(Reported_T_temp)) # has been no reporting those days
        Reported_T_temp = 0
      Reported_O[i,  (i+k+1):N2] <- Reported_O[i,  (i+k+1):N2] -Reported_T_temp
      Reported_T[i, (i+k+1):N2]  <- NA
    }else{
      day_completed[i] <- 0
    }
  }
  return(list(Reported_O = Reported_O,
              Reported_T = Reported_T,
              day_completed = day_completed,
              dates_not_reported = dates_not_reported) )
}

##
#' removes observations on weekend, monday, and put diagonal to zero
#' putting the data into the style of current reporting (2020-11)
#'  @param reports      - (N x N) reported cumlative deaths
#'  @param report_dates - (N x 1) dates the data was reported
#'
report_clean <- function(reports, report_dates){
  diag(reports) <- 0
  holidays <- weekdays(report_dates)%in%c("Monday","Sunday","Saturday") | c(report_dates)%in%c(holidays.Sweden)
  reports[,holidays] <- NA
  return(reports)
}

##
#' cereate new_cases matrix
#'  @param reports - (N x N) reported cumlative deaths
##
newCases <- function(reports){
  newreport <- reports
  #reports_temp[is.na(reports_temp)]=0
  ##
  # ugly fix
  ##

  reports_temp <- reports

  for(i in 1:dim(reports_temp)[1]){
    reports_temp[i,1:(i-1)]=0
    if(is.na(reports_temp[i,i]))
      reports_temp[i,i] <- 0
    for(j in i:dim(reports_temp)[2]){
      if(is.na(reports_temp[i,j]))
        reports_temp[i,j]= reports_temp[i,j-1]
    }

  }
  reports_temp <- cbind(0,reports_temp)
  newreport <- t(diff(t(reports_temp)))
  newreport[is.na(reports)]=NA
  #newreport[upper.tri(newreport)] <- diff.report[upper.tri(diff.report,T)]
  newreport[newreport<0 & is.na(newreport)==F]=0 #fake
  return(newreport)
}
##
# transforms data,
# we remove data if negative..
#
#  deaths  - (N x 1) how many has died (thruth)
#  reports - (N x N) reported cumlative deaths
#  maxusage.day - (int) only use data up to maxusage.days i.e.
#                       reports[i,i + maxusage.day - 1]
#
##
newDeaths <-function(deaths, reports,maxusage.day = -1){
  newreport <- reports
  #reports_temp[is.na(reports_temp)]=0
  ##
  # ugly fix
  ##

  reports_temp <- reports

  for(i in 1:dim(reports_temp)[1]){
    reports_temp[i,1:(i-1)]=0
    if(is.na(reports_temp[i,i]))
      reports_temp[i,i] <- 0
    for(j in i:dim(reports_temp)[2]){
      if(is.na(reports_temp[i,j]))
        reports_temp[i,j]= reports_temp[i,j-1]
    }

  }
  reports_temp <- cbind(0,reports_temp)
  newreport <- t(diff(t(reports_temp)))
  newreport[is.na(reports)]=NA
  #newreport[upper.tri(newreport)] <- diff.report[upper.tri(diff.report,T)]
  newreport[newreport<0 & is.na(newreport)==F]=0 #fake
  death.rem <- diag(deaths)

  cumsum_report <- t(apply(newreport, 1, function(x){x[is.na(x)==F]=cumsum(x[is.na(x)==F]); return(x)}))
  dr<-deaths - cumsum_report
  N <- length(deaths)
  dr <- dr[1:(N-1),1:(N-1)]
  death.rem[upper.tri(newreport)] <- dr[upper.tri(dr,T)]
  death.rem[lower.tri(death.rem)] <- NA
  diag(death.rem)[is.na(diag(reports))] <- NA

  ##
  # ugly fix 2
  ##
  for(k in 1:dim(death.rem)[1]){
    index <- is.na(death.rem[k,]) & is.na(newreport[k,])==F
    if(sum(index)>0){
      death.rem[k, index] <- deaths[k]
    }
  }
  if(maxusage.day>0){
    for(i in 1:length(death.rem)){
      if(i + maxusage.day - 1 < N ){
        death.rem[i,(i+maxusage.day):N] = NA
        newreport[i,(i+maxusage.day):N] = NA
      }
    }
  }
  #removing small bugs in reporting
  newreport[death.rem <0 & is.na(death.rem)==F] = 0
  death.rem[death.rem <0& is.na(death.rem)==F] = 0
  index <- (death.rem< newreport) &  is.na(death.rem) ==F & is.na(newreport)==F
  newreport[index] =death.rem[index]
  return(list(death.remain = death.rem, report.new = newreport))
}


##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   Bin(deaths[upper.tri], logit(X\beta))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
#'               $hessian - Hessian of the likelihood
##
loglikProb <- function(beta, death.remain, report.new, X){

  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X <- X[index,]
  p = as.vector(1/(1+exp(-X%*%beta)))
  y <- y[index]
  n <- n[index]
  lik <- sum(dbinom(y,size = n, prob = as.vector(p), log = T))

  grad <- as.vector(t(X)%*%as.vector(y-n*p))

  Hessian <- -t(X)%*%diag(as.vector(n*p*(1-p)))%*%X
  Hessian <- diag(diag(Hessian))

  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}
loglikPrior <- function(beta, n, sigma, res){
  n <- length(sigma)
  res$grad[1:n] <-  res$grad[1:n]-beta[1:n]/(sigma^2)
  diag(res$Hessian) <- diag(res$Hessian) - c(1/sigma^2, rep(0,length(beta)-n))
  res$loglik <- res$loglik -sum(beta[1:n]^2/(2*sigma^2))
  return(res)
}

loglik <- function(beta,death.remain, report.new, X, sigma){
  res <- loglikProb(beta, death.remain, report.new, X)
  res <- loglikPrior(beta, n, sigma, res)
  return(res)
}


#####
##
# beta binomial part
##
#####

##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB_mixedeffect_bi <- function(beta, death.remain, report.new, X_mu, X_M, X_mixed, sigma_mixed){

  p <- dim(X_mu)[2]
  p1 <- dim(X_M)[2]
  p2 <- dim(X_mixed)[2]
  beta_mu    <- beta[1:p]
  beta_M     <- beta[p + (1:p1)]
  beta_mixed <- beta[p + p1 + (1:p2)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X_mu    <- X_mu[index,]
  X_mixed <- X_mixed[index,]
  X_M     <- X_M[index,]
  mu      <- 1/(1+exp(-X_mu%*%beta_mu - X_mixed%*%beta_mixed))
  M       <- exp(X_M%*%beta_M)

  alpha <- M * mu
  beta  <- M * (1-mu)
  y <- y[index]
  n <- n[index]

  if(max(beta)>120)
    return(list(loglik = -Inf, grad = 0))


  lik <- sum(dBB(y, size = n, alpha = alpha, beta = beta, log.p=T))
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0))

  ##
  #
  # mixed effect
  ##
  lik <- lik - (0.5/sigma_mixed^2) * sum(beta_mixed^2)
  ##
  # prior
  ##
  #lik <- lik - sum(exp(beta_M))
  grad_lik <- grad_dBB(y, size = n, alpha = alpha, beta = beta)

  grad_mu <- (M*mu * (1-mu) * grad_lik$grad_alpha)
  grad_mu <- (grad_mu - (M * mu * (1-mu) * grad_lik$grad_beta))
  grad_mixed <- t(X_mixed)%*%grad_mu - (1/sigma_mixed^2) * beta_mixed
  grad_mu    <- t(X_mu)%*%grad_mu
  grad_M <- (M*mu * grad_lik$grad_alpha)
  grad_M <- grad_M + (M * (1-mu) * grad_lik$grad_beta)
  grad_M <- t(X_M)%*%grad_M #- exp(beta_M)
  grad =  c(as.vector(grad_mu),as.vector(grad_M),as.vector(grad_mixed))
  if(sum(is.na(grad))>0)
    return(list(loglik = -Inf, grad = 0))
  return(list(loglik = lik, grad = grad))
}


##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB_bi <- function(beta, death.remain, report.new, X_mu, X_M, pi_null, p_null, z){

  p <- dim(X_mu)[2]
  p1 <- dim(X_M)[2]
  beta_mu    <- beta[1:p]
  beta_M     <- beta[p + (1:p1)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X_mu    <- X_mu[index,]
  X_M     <- X_M[index,]
  mu      <- 1/(1+exp(-X_mu%*%beta_mu ))
  M       <- exp(X_M%*%beta_M)

  alpha <- M * mu
  beta  <- M * (1-mu)
  y <- y[index]
  n <- n[index]

  if(is.na(z[1]))
    z = rep(0,length(y))
  index_z   <- z==0
  y_lik     <- y[index_z]
  n_lik     <- n[index_z]
  alpha_lik <- alpha[index_z]
  beta_lik  <- beta[index_z]
  mu_lik     <- mu[index_z]
  M_lik     <- M[index_z]
  if(max(beta)>100)
    return(list(loglik = -Inf, grad = 0))


  lik_obs <- dBB(y, size = n, alpha = alpha, beta = beta, log.p=T)
  lik <- sum(lik_obs)
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0))

  ##
  # prior
  ##
  lik <- lik
  grad_lik <- grad_dBB(y_lik, size = n_lik, alpha = alpha_lik, beta = beta_lik)

  grad_mu <- (M_lik*mu_lik * (1-mu_lik) * grad_lik$grad_alpha)
  grad_mu <- (grad_mu - (M_lik * mu_lik * (1-mu_lik) * grad_lik$grad_beta))
  grad_mu    <- t(X_mu[index_z==T,])%*%grad_mu
  grad_M <- (M_lik*mu_lik * grad_lik$grad_alpha)
  grad_M <- grad_M + (M_lik * (1-mu_lik) * grad_lik$grad_beta)
  grad_M <- t(X_M[index_z==T,])%*%grad_M
  grad =  c(as.vector(grad_mu),as.vector(grad_M))
  if(sum(is.na(grad))>0)
    return(list(loglik = -Inf, grad = 0))


  ##
  # sample mixture
  ##
  lik_null <- dbinom(y, n, p_null, log=T)
  p_null   <- log(pi_null) + lik_null
  p_full  <-  log(1-pi_null)   + lik_obs
  p_max   <- apply(cbind(p_null,p_full),1, max)
  p0 <- exp(p_null - p_max)
  p0 <- p0/(p0 + exp(p_full-p_max))
  z <- as.vector(1*  (runif(length(p0)) <= p0))
  pi <- rbeta(1, 5+ sum(z==1), 5 + sum(z==0))
  p  <- rbeta(1, 1 + sum(y[z==1]), 20+ sum(n[z==1])-sum(y[z==1]))

  return(list(loglik = lik, grad = grad, p_null= p, pi_null = pi, z = z))
}


##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB3 <- function(beta, death.remain, report.new, X_mu, X_M){

  p <- dim(X_mu)[2]
  p1 <- dim(X_M)[2]
  beta_mu    <- beta[1:p]
  beta_M     <- beta[p + (1:p1)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X_mu    <- X_mu[index,]
  X_M     <- X_M[index,]
  mu      <- 1/(1+exp(-X_mu%*%beta_mu ))
  M       <- exp(X_M%*%beta_M)

  alpha <- M * mu
  beta  <- M * (1-mu)
  y <- y[index]
  n <- n[index]

  lik <- sum(dBB(y, size = n, alpha = alpha, beta = beta, log.p=T))
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0))

  ##
  # prior
  ##
  lik <- lik - sum(exp(beta_M))
  grad_lik <- grad_dBB(y, size = n, alpha = alpha, beta = beta)

  grad_mu <- (M*mu * (1-mu) * grad_lik$grad_alpha)
  grad_mu <- (grad_mu - (M * mu * (1-mu) * grad_lik$grad_beta))
  grad_mu    <- t(X_mu)%*%grad_mu
  grad_M <- (M*mu * grad_lik$grad_alpha)
  grad_M <- grad_M + (M * (1-mu) * grad_lik$grad_beta)
  grad_M <- t(X_M)%*%grad_M - exp(beta_M)
  grad =  c(as.vector(grad_mu),as.vector(grad_M))
  if(sum(is.na(grad))>0)
    return(list(loglik = -Inf, grad = 0))
  return(list(loglik = lik, grad = grad))
}


##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB_param2 <- function(beta, death.remain, report.new, X, calcH=F){

  p <- dim(X)[2]
  beta_1 <- beta[1:p]
  beta_2 <- beta[(p+1):(2*p)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X <- X[index,]
  mu <- 1/(1+exp(-X%*%beta_1))
  M  <- exp(X%*%beta_2)

  alpha <- M * mu
  beta  <- M * (1-mu)
  y <- y[index]
  n <- n[index]

  if(max(beta)>120)
    return(list(loglik = -Inf, grad = 0))



  lik <- sum(dBB(y, size = n, alpha = alpha, beta = beta, log.p=T))
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0))

  lik <- lik #- 2*sum(exp(beta_2))
  grad_lik <- grad_dBB(y, size = n, alpha = alpha, beta = beta)

  grad_mu <- (M*mu * (1-mu) * grad_lik$grad_alpha)
  grad_mu <- t(X)%*%(grad_mu - (M * mu * (1-mu) * grad_lik$grad_beta))
  grad_M <- (M*mu * grad_lik$grad_alpha)
  grad_M <- grad_M + (M * (1-mu) * grad_lik$grad_beta)
  grad_M <- t(X)%*%grad_M #- 2*exp(beta_2)
  grad =  c(as.vector(grad_mu),as.vector(grad_M))
  if(sum(is.na(grad))>0)
    return(list(loglik = -Inf, grad = 0))
  return(list(loglik = lik, grad = grad))
}




##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB_mixedeffect <- function(beta, death.remain, report.new, X_mu, X_M, X_mixed, sigma_mixed){

  p <- dim(X_mu)[2]
  p1 <- dim(X_M)[2]
  p2 <- dim(X_mixed)[2]
  beta_mu    <- beta[1:p]
  beta_M     <- beta[p + (1:p1)]
  beta_mixed <- beta[p + p1 + (1:p2)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X_mu    <- X_mu[index,]
  X_mixed <- X_mixed[index,]
  X_M     <- X_M[index,]
  mu      <- 1/(1+exp(-X_mu%*%beta_mu - X_mixed%*%beta_mixed))
  M       <- exp(X_M%*%beta_M)

  alpha <- M * mu
  beta  <- M * (1-mu)
  y <- y[index]
  n <- n[index]

  if(max(beta)>120)
    return(list(loglik = -Inf, grad = 0))


  lik <- sum(dBB(y, size = n, alpha = alpha, beta = beta, log.p=T))
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0))

  ##
  #
  # mixed effect
  ##
  lik <- lik - (0.5/sigma_mixed^2) * sum(beta_mixed^2)
  ##
  # prior
  ##
  #lik <- lik - sum(exp(beta_M))
  grad_lik <- grad_dBB(y, size = n, alpha = alpha, beta = beta)

  grad_mu <- (M*mu * (1-mu) * grad_lik$grad_alpha)
  grad_mu <- (grad_mu - (M * mu * (1-mu) * grad_lik$grad_beta))
  grad_mixed <- t(X_mixed)%*%grad_mu - (1/sigma_mixed^2) * beta_mixed
  grad_mu    <- t(X_mu)%*%grad_mu
  grad_M <- (M*mu * grad_lik$grad_alpha)
  grad_M <- grad_M + (M * (1-mu) * grad_lik$grad_beta)
  grad_M <- t(X_M)%*%grad_M #- exp(beta_M)
  grad =  c(as.vector(grad_mu),as.vector(grad_M),as.vector(grad_mixed))
  if(sum(is.na(grad))>0)
    return(list(loglik = -Inf, grad = 0))
  return(list(loglik = lik, grad = grad))
}


##
# likelihood observations given deaths and probabilites
# model is:
# newreport[upper.tri] \sim   BB(deaths[upper.tri], exp(X\beta_1), exp(X\beta_2))
#
#' @return obj - list
#'               $loglik  - logliklihood
#'               $grad    - gradient
##
loglikProbBB <- function(beta, death.remain, report.new, X, calcH=F){

  p <- dim(X)[2]
  beta_1 <- beta[1:p]
  beta_2 <- beta[(p+1):(2*p)]
  N <- dim(report.new)[1]

  index <- upper.tri(death.remain,diag = T)
  y = report.new[index]
  n = death.remain[index]
  index = is.na(y)==F & is.na(n)==F
  X <- X[index,]
  alpha <- exp(X%*%beta_1)
  beta  <- exp(X%*%beta_2) #unfortunate name
  if(min(1/(alpha+beta)) <10^-3)
    return(list(loglik = -Inf, grad = 0, Hessian = 0))
  y <- y[index]
  n <- n[index]

  lik <- sum(dBB(y, size = n, alpha = alpha, beta = beta, log.p=T))
  if(is.na(lik))
    return(list(loglik = -Inf, grad = 0, Hessian = 0))

  grad_lik <- grad_dBB(y, size = n, alpha = alpha, beta = beta)
  grad_alpha <- as.vector(t(X)%*%(alpha*grad_lik$grad_alpha))
  grad_beta  <- as.vector(t(X)%*%(beta*grad_lik$grad_beta))
  if(calcH){
    H_ <- Hessian_dBB(y, size = n, alpha = alpha, beta = beta)
    H_12 <-  t(X)%*%diag(as.vector(alpha*beta*H_$grad_alpha_beta))%*%X
    H_11 <-  t(X)%*%diag(as.vector(alpha^2*H_$grad_alpha_alpha))%*%X
    H_22 <-  t(X)%*%diag(as.vector(beta^2*H_$grad_beta_beta))%*%X
    Hessian <- rbind(cbind(H_11, H_12) ,
                     cbind(H_12, H_22) )
    Hessian  <- diag(diag(Hessian) )
    diag(Hessian) <- diag(Hessian)
  }else{
    Hessian =NULL
  }
  grad =  c(grad_alpha,grad_beta)
    if(sum(is.na(grad))>0)
      return(list(loglik = -Inf, grad = 0, Hessian = 0))
  return(list(loglik = lik, grad = grad, Hessian = Hessian))
}
##
# PRobability of beta binomial
##
dBB<- function(x, size, alpha, beta, log.p = F){
  const = lgamma(size + 1) - lgamma(x + 1) -
    lgamma(size - x + 1)
  ld <- const +
    lgamma(x + alpha) + lgamma(size - x + beta) -
    lgamma(size + alpha + beta) +
    lgamma(alpha + beta) -
    lgamma(alpha) - lgamma(beta)
  if(log.p==F){
    return(exp(ld))
  }
  return(ld)
}
##
# gradient of log of probability beta binomial
# return list,
# [[1]] grad alpha
# [[2]] grad beta
##
grad_dBB<- function(x, size, alpha, beta){
  grad_alpha <- digamma(x + alpha) -
    digamma(size + alpha + beta) +
    digamma(alpha + beta) -
    digamma(alpha)
  grad_beta <- digamma(size - x + beta) -
    digamma(size + alpha + beta) +
    digamma(alpha + beta) -
    digamma(beta)
  return(list(grad_alpha = grad_alpha,
              grad_beta  = grad_beta ))
}
##
# Hessian of log of probability beta binomial
# return list,
# [[1]] grad alpha
# [[2]] grad beta
##
Hessian_dBB<- function(x, size, alpha, beta){
  grad_alpha_alpha <- trigamma(x + alpha) -
    trigamma(size + alpha + beta) +
    trigamma(alpha + beta) -
    trigamma(alpha)
  grad_beta_beta <- trigamma(size - x + beta) -
    trigamma(size + alpha + beta) +
    trigamma(alpha + beta) -
    trigamma(beta)

  grad_alpha_beta <- -trigamma(size + alpha + beta) +
    trigamma(alpha + beta)
  return(list(grad_alpha_alpha = grad_alpha_alpha,
              grad_beta_beta  = grad_beta_beta,
              grad_alpha_beta = grad_alpha_beta))
}

##
# log liklihood of obseving report given death and prob
# density is Beta binomial
#  deaths  - (int) true number of deaths
#  alpha       - (n x 1) bb parameter 1
#  beta        - (n x 1) bb parameter 2
#  report  - (n x 1) reported deaths culmative each date
##
loglikDeathsGivenProbBB <- function(death, alpha, beta, report){

  if(death < max(report,na.rm=T))
    return(-Inf)
  n <- length(alpha)
  if(is.na(report[1])){
    #we dont have data from day one

    ndeaths <- diff(report[is.na(report)==F])
    report_adj <- report[1:(n-1)]
    report_adj <- report_adj[is.na(report_adj)==F]
  }else{
    ndeaths <- c(report[1],diff(report))
    if(n>1){
      report_adj <- c(0, report[1:(n-1)])
    }else{
      report_adj <- 0
    }
  }

  ndeaths[ndeaths <0 ] = 0

  return(sum(dBB(x =ndeaths,
                 size = death - report_adj,
                 alpha = alpha[is.na(alpha)==F],
                 beta = beta[is.na(alpha)==F],
                 log.p=T)))
}


##
# log liklihood of obseving report given death and prob
# density is Beta binomial
#  deaths  - (int) true number of deaths
#  alpha       - (n x 1) bb parameter 1
#  beta        - (n x 1) bb parameter 2
#  report  - (n x 1) reported deaths culmative each date
##
loglikDeathsGivenProbBB_bi <- function(death, alpha, beta, pi_null,p_null,report){

  if(death < max(report,na.rm=T))
    return(-Inf)
  n <- length(alpha)
  if(is.na(report[1])){
    #we dont have data from day one

    ndeaths <- diff(report[is.na(report)==F])
    report_adj <- report[1:(n-1)]
    report_adj <- report_adj[is.na(report_adj)==F]
  }else{
    ndeaths <- c(report[1],diff(report))
    if(n>1){
      report_adj <- c(0, report[1:(n-1)])
    }else{
      report_adj <- 0
    }
  }

  ndeaths[ndeaths <0 ] = 0
  p <- rep(0, length(ndeaths))

  log1 <- dBB(x =ndeaths,
              size = death - report_adj,
              alpha = alpha[is.na(alpha)==F],
              beta = beta[is.na(alpha)==F],
              log.p=T)
  #p_ <- rep(0,length(log1))
  #p_[1:min(2,length(ndeaths))] =log1[1:min(2,length(ndeaths))]
  #if(length(ndeaths)>2){
  #  p1 <- log(1-pi_null) + log1[3:length(ndeaths)]
  #  p2 <- log(pi_null) + dbinom(ndeaths[3:length(ndeaths)], death - report_adj[3:length(ndeaths)],prob=p_null,log = T)
  #  p_[3:length(ndeaths)] <- log( exp(p1) + exp(p2))
  #}
  p1 <- log(1-pi_null) + log1
  p2 <- log(pi_null) +  dbinom(ndeaths, death - report_adj,prob=p_null,log = T)
  p_ <- log( exp(p1) + exp(p2))
  return(sum(p_))
}

###
#
# posterior sampling of deaths given Prob vec
#
#  alpha     - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
#  beta      - (N x N) matrix of beta binom parameter (only upper triangular part relevant)
# Reported    - (N x N) matrix of reported deaths cumlative (only upper triangular relevant)
# Predict.day - (int) which day to of reporting to predict
# sim         - (2 x 1) MCMC samples inner loop and outer loop
# alpha       - (N x 1) stepsizes
# prior       - (function) prior for N (needs use N and i)
#
###
death.givenParamBB <- function(alpha, beta, Reported,Predict.day = Inf,sim=c(2000,10), alpha.MCMC = NULL,
                               use.prior=F,
                               prior=NULL){

  N <- dim(alpha)[1]
  if(is.null(alpha.MCMC))
    alpha.MCMC <- rep(4, N)

  deaths <- matrix(NA, nrow=sim[1], ncol = N)
  deaths_est <- apply(Reported, 1, max, na.rm=T)

  burnin = ceiling(0.3*sim[1])
  for(i in 1:(sim[1] + burnin -1)){
    res <- sample.deathsBB(sim[2],
                           deaths_est,
                           alpha,
                           beta,
                           Reported,
                           alpha.MCMC,
                           rep.day=Predict.day,
                           use.prior=use.prior,
                           prior= prior)
    deaths_est <- res$deaths
    if(i < burnin){
      alpha.MCMC[res$acc/sim[2] > 0.3] <- alpha.MCMC[res$acc/sim[2] > 0.3] +1
      alpha.MCMC[res$acc/sim[2] < 0.3] <- alpha.MCMC[res$acc/sim[2] < 0.3] -1
      alpha.MCMC[alpha.MCMC<1] <- 1
    }else{
      deaths[i - burnin +1,] = deaths_est
    }
  }
  return(deaths)
}

##
# buidling the X matrix
#
#
# unique.prob - (int) how many of days shall we have unique probabilites
##
setup_data <- function(N, Predict.day, dates_report, unique.prob=NULL){

  X <- buildXdayeffect(N,Predict.day)
  #holidays and weekends

  result$dates_report <- as.Date(dates_report)

  if(is.null(unique.prob)==F)
    X <- cbind(X[,1:unique.prob,drop=F],rowSums(X[,1:unique.prob,drop=F])==0)

  holidays <- weekdays(dates_report)%in%c("Sunday","Saturday") | c(dates_report)%in%c(holidays.Sweden)
  holiday.yesterday <- weekdays(dates_report - 1)%in%c("Sunday","Saturday") |
    (result$dates_report-1)%in%c(holidays.Sweden)
  Xhol <- buildXholiday(N,holidays - (holiday.yesterday*holidays==T))
  Xhol.sunday <- buildXholiday(N,holiday.yesterday*holidays==T)
  Xhol.yesterday <-buildXholiday(N,holiday.yesterday - (holiday.yesterday*holidays==T) )

  X_dim <- dim(X)[2]
  X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday, (Xhol+Xhol.sunday)*X[,1])
  colnames(X) <- c(paste('lag_',0:(X_dim-1),sep=''),'holiday','sunday','holiday_yesterday','lag0_hol')
  return(X)
}

##
# buidling the X matrix
#
#
# unique.prob - (int) how many of days shall we have unique probabilites
##
setup_data_lag <- function(N, Predict.day, dates_report, unique.prob=NULL){

  X <- buildXdayeffect(N,Predict.day)
  #holidays and weekends

  result$dates_report <- as.Date(dates_report)

  if(is.null(unique.prob)==F)
    X <- cbind(X[,1:unique.prob,drop=F],rowSums(X[,1:unique.prob,drop=F])==0)

  holidays <- weekdays(dates_report)%in%c("Sunday","Saturday") | c(dates_report)%in%c(holidays.Sweden)
  holiday.yesterday <- weekdays(dates_report - 1)%in%c("Sunday","Saturday") |
    (result$dates_report-1)%in%c(holidays.Sweden)
  Xhol <- buildXholiday(N,holidays - (holiday.yesterday*holidays==T))
  Xhol.sunday <- buildXholiday(N,holiday.yesterday*holidays==T)
  Xhol.yesterday <-buildXholiday(N,holiday.yesterday - (holiday.yesterday*holidays==T) )
  X.Wednesday <-buildXholiday(N, weekdays(dates_report)%in%c('Wednesday') )
  #X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday, Xhol*X[,1],X.Wednesday-X.Wednesday*Xhol)
  X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday, Xhol*X[,1])

  return(X)
}
##
# buidling the X matrix
#
#
# unique.prob - (int) how many of days shall we have unique probabilites
##
setup_data_postlag <- function(N, lag, Predict.day, dates_report, unique.prob=NULL){
  if(Predict.day==0){
    X <- matrix(1, nrow=N*(N+1)/2,  ncol=1+lag+2)
    unique.prob= NULL
  }else{
    X <- buildXdayeffect(N,Predict.day+lag+2)
  }
  if(lag>0)
    X <- X[,-(1:(lag+2)),drop=F]
  #holidays and weekends

  result$dates_report <- as.Date(dates_report)

  if(is.null(unique.prob)==F)
    X <- cbind(X[,1:unique.prob,drop=F],rowSums(X[,1:unique.prob,drop=F])==0)

  holidays <- weekdays(dates_report)%in%c("Sunday","Saturday") | c(dates_report)%in%c(holidays.Sweden)
  holiday.yesterday <- weekdays(dates_report - 1)%in%c("Sunday","Saturday") |
    (result$dates_report-1)%in%c(holidays.Sweden)
  Xhol <- buildXholiday(N,holidays - (holiday.yesterday*holidays==T))
  Xhol.sunday <- buildXholiday(N,holiday.yesterday*holidays==T)
  Xhol.yesterday <-buildXholiday(N,holiday.yesterday - (holiday.yesterday*holidays==T) )
  #X.Wednesday <-buildXholiday(N, weekdays(dates_report)%in%c('Wednesday') )
  #X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday, Xhol*X[,1],X.Wednesday-X.Wednesday*Xhol)
  X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday)

  return(X)
}

##
# buidling the X matrix
#
#
# unique.prob - (int) how many of days shall we have unique probabilites
##
setup_data_postlag2 <- function(N, lag, params, dates_report){
  if(params==0){
    X <- matrix(1, nrow=N*(N+1)/2,  ncol=1)
    unique.prob= 0
  }else{
    X_ <- matrix(0, nrow=N, ncol = N)
    holidays <- weekdays(dates_report)%in%c("Sunday","Saturday") | c(dates_report)%in%c(holidays.Sweden)
    for(i in 1:N){
      k <- which.max(cumsum(holidays[i:N]==F)==(lag+1))-1
      for(p in 1:params){
        if((i+k +p) <= N)
          X_[i,(i+k +p)] <- p
      }
    }
    X_ <- X_[upper.tri(X_,diag=T)]
    j_ <-  X_[X_>0]
    i_base <- 1:length(X_)
    i_ <-  i_base[X_>0]
    X <- sparseMatrix(i=i_,j=j_ , dims=c(length(X_), max(j_))  )
    unique.prob = max(j_)
  }



  #holidays and weekends

  result$dates_report <- as.Date(dates_report)

  if(unique.prob)
    X <- cbind(X[,1:unique.prob,drop=F],rowSums(X[,1:unique.prob,drop=F])==0)

  holidays <- weekdays(dates_report)%in%c("Sunday","Saturday") | c(dates_report)%in%c(holidays.Sweden)
  holiday.yesterday <- weekdays(dates_report - 1)%in%c("Sunday","Saturday") |
    (result$dates_report-1)%in%c(holidays.Sweden)
  Xhol <- buildXholiday(N,holidays - (holiday.yesterday*holidays==T))
  Xhol.sunday <- buildXholiday(N,holiday.yesterday*holidays==T)
  Xhol.yesterday <-buildXholiday(N,holiday.yesterday - (holiday.yesterday*holidays==T) )
  X <- cbind(X, Xhol, Xhol.sunday, Xhol.yesterday)

  return(X)
}

setup_data_mixture_cov <- function(N, lag, dates_report){

    X_ <- matrix(0, nrow=N, ncol = N)
    holidays <- weekdays(dates_report)%in%c("Sunday")
    for(i in 1:N){
      k <- which.max(cumsum(holidays[i:N]==F)==(lag+1))-1
      if(i+k < N)
        X_[i,(i+k+1):N] = (i+k+1):N
    }
    X_[,holidays==T]=0
  return(X_)
}
setup_all_days <- function(N){

  X_ <- matrix(0, nrow=N, ncol = N)
  for(i in 1:N){
      X_[i,i:N] = i:N
  }
  X <- X_[upper.tri(X_,diag=T)]
  return(X)
}


setup_data_mixed_effect <- function(N, lag, dates_report){
  X_ <- setup_data_mixture_cov(N, lag, dates_report)
  X_ <- X_[upper.tri(X_,diag=T)]
  X_ <- as.integer(factor(X_))-1
  X <- matrix(0, nrow=length(X_), ncol = max(X_))
  for(i in 1:max(X_)){
    X[,i] <- X_==i
  }
  return(X)
}
