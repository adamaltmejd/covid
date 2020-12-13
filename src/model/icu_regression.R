####
# model for building icu_smoothed covariates for each date
#
#
####
library(gamm4)
library(readxl)



#'
#'
#' Regression between deaths and icu,
#'
#'@param list for deaths output of buildData.R
#'@param list for icu    output of buildData_icu.R
#'@param int  days of lag to use to fit coeffients
icu_covariates <- function(deaths, icu){

    ##
    #read files
    #
    # save files on line 77
    icu_cov <- NULL
    icu_cov_name <- file.path("data", "model", "icu_covariates.rds")
    if(file.exists(icu_cov_name)==T)
        icu_cov  <- readRDS(icu_cov_name)

    icu_pre <-read_excel(path = file.path("data", "FHM", "Folkhalsomyndigheten_Covid19_2020-12-09.xlsx"),
                         sheet = 3,
                         col_types = c("date", "numeric"),
                         progress = FALSE)
    ###
    ###

    lag        <- 7   #days to use

    Reported = icu$detected
    N <- dim(Reported)[1]
    icu_est <- rep(0,N-lag)
    for(j in 1:(N -lag)){
        icu_est[j] <- max(icu$detected[j,1:(j+lag)],na.rm=T)
    }


    icu_pre <- data.frame(date = as.Date(icu_pre$Datum_vårdstart),
                          icu  = icu_pre$Antal_intensivvårdade)
    icu_est <- c(icu_pre$icu[1:51], icu_est)
    dates <- c(icu_pre$date[1:51] ,as.Date(icu$dates[1:(N-lag)]))



    if(is.null(icu_cov)){
        data.list <- list(date=1:51,
                          Y = icu_est[1:51])
        model.fit <- gam(Y~s(date), data=data.list, family = poisson)
        icu_cov = data.frame(date=dates[1:51],
                            theta=log(model.fit$fitted.values)[1:51])
    }
    for(i in 52:length(icu_est)){
        if(dates[i]%in% icu_cov$date ==T)
            next

        cat('running covariate estimate for day = ', as.character(dates[i]),'\n')
        data.list <- list(date=1:i,
                          Y = icu_est[1:i])

        model.fit <- gam(Y~s(date), data=data.list, family = poisson)

        icu_cov <- rbind(icu_cov,
                         c(as.character(dates[i]),
                           log(model.fit$fitted.values)[i]))

    }
    icu_cov$theta <- as.numeric(icu_cov$theta)
    saveRDS(icu_cov, file.path("data", "model", "icu_covariates.rds"))
    theta <- icu_cov$theta
    icu_lag <- c(rep(0,lag), theta)[1:length(theta)]


    # fit coefficients
    incomplete_deaths <- 30
    N_d <- length(deaths$dates)
    Cov_analysis <- data.frame(y     = apply(deaths$detected[1:(N_d-incomplete_deaths),],1,max,na.rm=T),
                               dates = deaths$dates[1:(N_d-incomplete_deaths)])
    icu_cov_glm <- icu_lag[dates%in%Cov_analysis$dates]

    glm.fit <- glm(y~x,poisson, data= data.frame(y=Cov_analysis$y,x=icu_cov_glm))






    return(list(theta_cov = data.frame(dates = dates,
                                       theta = icu_lag),
                coeff     = glm.fit$coefficients))
}



