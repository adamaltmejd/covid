###
#
# fit Gaussian processes for smoothing
#

likelihood_death <- function(x, D, y){
    kappa       <- exp(x[1])
    nu          <- exp(x[2])
    sigma       <- exp(x[3])
    sigma_noise <- exp(x[4])
    Sigma <- matern.covariance(D, kappa, nu, sigma)
    diag(Sigma)<- diag(Sigma) + sigma_noise^2
    R <- chol(Sigma, pivot=T)
    if(attr(R,"rank" )< length(y))
        return(-Inf)
    v <- solve(t(R),y[attr(R,"pivot")])
    return( -sum(log(diag(R))) - 0.5*t(v)%*%v)
}

gp_smooth <- function(death_prediction, model_death_dt, model.file ="model_smooth.rds"){

    model_dt_smooth <- NULL

    if( file.exists(file.path("data", "model", model.file))){
        model_dt_smooth<- readRDS(file.path("data", "model", model.file))
    }

    N <- length(model_death_dt$dates)
    death_reported_so_far <- data.frame(
                                        T_deaths = apply(model_death_dt$detected,1,max, na.rm=T)[1:(N-25)],
                                        dates  =  model_death_dt$dates[1:(N-25)])
    dates <- sort(unique(death_prediction$prediction_date))

    kappa <- 1
    nu    <- 1
    sigma <- 1
    sigma_noise <- 1
    for(date_ in as.character(dates)){

        if(is.null(model_dt_smooth)==F){
            if(sum(model_dt_smooth$prediction_date == date_) >0)
                next
        }
        cat('prediction smooth day ',as.character(date_),'\n',sep="")
        model_dt_date <- death_prediction[death_prediction$prediction_date==date_,]

        index_d <- death_reported_so_far$date < model_dt_date$prediction_date[1]-25
        if(sum(index_d) < 20)
            next
        time_d  = death_reported_so_far$date[index_d] - min(death_reported_so_far$date[index_d])
        y_d     =  sqrt(death_reported_so_far$T_deaths[index_d] )
        D_d     = as.matrix(dist(time_d))
        res     <- optim(log(c(kappa,nu,sigma,sigma_noise)), function(x){-likelihood_death(x, D_d, y_d)})
        kappa       <- exp(res$par[1])
        nu          <- exp(res$par[2])
        sigma       <- exp(res$par[3])
        sigma_noise <- exp(res$par[4])
        y       <- sqrt(model_dt_date$total)
        sigma_Y <-(sqrt(model_dt_date$total_uCI+1)-sqrt(model_dt_date$total_lCI+1))/(2*qnorm(0.975))


        D <- as.matrix(dist(model_dt_date$date-min(model_dt_date$date)))
        Sigma <- matern.covariance(D, kappa, nu, sigma)
        Sigma_obs <- Sigma
        diag(Sigma_obs)<- diag(Sigma_obs) + sigma_noise^2
        Sigma_Y <- Sigma_obs
        diag(Sigma_Y) <- diag(Sigma_Y) + sigma_Y^2
        mu <- Sigma%*%solve(Sigma_Y, y)
        mu_obs <- Sigma_obs%*%solve(Sigma_Y, y)

        Sigma_cond <- Sigma - Sigma%*%solve(Sigma_Y,Sigma)
        Sigma_obs_cond <- Sigma_obs - Sigma_obs%*%solve(Sigma_Y,Sigma_obs)

        model_dt_date[,'total']     <- round((mu_obs)^2)
        model_dt_date[,'total_uCI'] <- ceiling((mu_obs+ 2*sqrt(diag(Sigma_obs_cond)))^2)
        lw <- floor(apply(mu_obs- 2*sqrt(diag(Sigma_obs_cond)),1,function(x){max(0,x)})^2)
        lw <- apply(cbind(model_dt_date$sure_deaths,lw),1,max, na.rm=T)
        model_dt_date[,'total_lCI'] <- lw
        model_dt_date[,'predicted_deaths'] <- model_dt_date[,'total']  - model_dt_date[,'obs']

        date_obs <- model_dt_date$prediction_date[1]
        date_min <- min(model_dt_date$date)
        total_date <- 0:(date_obs-date_min) + as.Date(date_min)
        new_dates <- setdiff(as.character(total_date),as.character(model_dt_date$date))
        if(length(new_dates)>0){

            time  = c(as.Date(new_dates),model_dt_date$date)- min(model_dt_date$date)
            D     = as.matrix(dist(time))
            Sigma_mod <- matern.covariance(D, kappa, nu, sigma)
            Sigma_mod_obs <- Sigma_mod
            diag(Sigma_mod_obs)<- diag(Sigma_mod_obs) + sigma_noise^2
            index <- (length(new_dates) + 1):length(time)
            mu_obs <-Sigma_mod_obs[,index]%*%solve(Sigma_Y, y)

            Sigma_mod_obs_cond <- Sigma_mod_obs - Sigma_mod_obs[,index]%*%solve(Sigma_Y,Sigma_mod_obs[index,])
            index_no <-1:length(new_dates)

            model_dt_date <- rbind(model_dt_date,
                               data.frame(
                                   prediction_date   = model_dt_date$prediction_date[1],
                                   date              = new_dates,
                                   sure_deaths       = 0,
                                   predicted_deaths  = 0,
                                   obs               = 0,
                                  total_lCI         = floor(apply(as.matrix(mu_obs[index_no]- 2*sqrt(diag(Sigma_mod_obs_cond)[index_no])),1,function(x){max(0,x)})^2),
                                   total_uCI         = ceiling((mu_obs[index_no]+ 2*sqrt(diag(Sigma_mod_obs_cond)[index_no]))^2),
                                   total             = round(mu_obs[index_no]^2)))
        }
            model_dt_smooth <- rbind(model_dt_smooth,model_dt_date)
    }

    saveRDS(model_dt_smooth, file.path("data", "model", model.file))
    model_dt_smooth <- data.table(model_dt_smooth)
    model_dt_smooth[, prediction_date := as.Date(prediction_date)]
    model_dt_smooth[, date := as.Date(date)]
    setkey(model_dt_smooth, prediction_date, date)
    return(model_dt_smooth)
}
