
#'
#' @param gamm model
CI_pred <- function(model, new.data, obs, sim = 2000,sizes=0:200, prior=NULL){


    beta <- coef(model);
    Vb <- vcov(model)
    ## simulate replicate beta vectors from posterior...
    Cv <- chol(Vb)
    nb <- length(beta)
    br <- t(Cv) %*% matrix(rnorm(sim*nb),nb,sim) + beta

    ## turn these into replicate linear predictors...
    Xp <- predict(model,newdata=new.data,type="lpmatrix")
    lp <- Xp%*%br
    fv <- c(model$family$linkinv(lp)) ## ... finally, replicate expected value vectors
    Total <- rep(0,sim)
    if(is.null(prior)){

        for(i in 1:sim){
            prob_size = dbinom(obs,sizes,prob=fv[i])
            prob_size = prob_size/sum(prob_size)
            Total[i] <- sample(sizes,1,prob=prob_size)
        }
    }else{
        prior_ <- prior(sizes)
        for(i in 1:sim){
            prob_size = dbinom(obs,sizes,prob=fv[i],log=T) + prior_
            prob_size = exp(prob_size-max(prob_size))
            prob_size = prob_size/sum(prob_size)
            Total[i] <- sample(sizes,1,prob=prob_size)
        }
    }
    return(Total)
}

model2.fit <- function(model_dt,
                     model_death_dt ,
                     days.reported=5,
                     lag = 20,
                     prior_=NULL){

    report_cleaned <- report_clean(model_death_dt$detected,model_death_dt$dates)
    new_cases <- newCases(report_cleaned)
    rownames(new_cases) <- as.character(model_death_dt$dates)
    colnames(new_cases) <- as.character(model_death_dt$dates)
    diag(new_cases) <- NA
    N <- apply(new_cases,1,sum, na.rm=T)
    Analysis_data <- c()
    coeff <- c()
    dates <- c()
    prediciton.set <-c()
    vcovs <- list()

    fit = NULL
    for(i in (days.reported+1):(dim(new_cases)[1]-lag)){

        cases <- sum(new_cases[(i-days.reported),1:i], na.rm=T)
        Total <- sum(new_cases[i-days.reported,], na.rm=T)
        state <- colnames(new_cases)[i + lag]
        pred.day <- colnames(new_cases)[i + lag - days.reported]
        if(is.null(model_dt)==F){
            index1 = model_dt$prediction_date == as.Date(state)
            index2 = model_dt$date            == as.Date(pred.day)
            if(sum(index1*index2) >0)
                next
        }
        cat('day = ',colnames(new_cases)[i],' days.behind = ',days.reported,'\n',sep="")
        predicted=F

        if(mean(is.na(new_cases[(i-days.reported),1:i]))<1){

            Analysis_data <- rbind(Analysis_data, c(cases, Total))
            dates <- c(dates,colnames(new_cases)[i])
            if(dim(Analysis_data)[1]>20 &
               mean(is.na(new_cases[(i+lag-days.reported),1:(i+lag)]))<1){
                Analysis_glm  <- data.frame(Y = Analysis_data[,1],
                                            N = Analysis_data[,2]-Analysis_data[,1],
                                            time = as.numeric(as.Date(dates)-as.Date(dates[1])),
                                            days  =as.factor(weekdays(as.Date(dates))))
                #w <- rev(cumprod(rep(0.95,length(Analysis_data[,2]))))
                #fit <- gam(cbind(Y,N)~1+ days, weights = w, data=Analysis_glm, family=binomial(link="logit"), method="REML")
                fit <- gam(cbind(Y,N)~1+s(time,bs="tp")+ days,  data=Analysis_glm, family=binomial(link="logit"), method="REML")

                n_fit <- length(fit$fitted.values)
                #predicition
                if(i + lag <= dim(new_cases)[1]){
                    predicted=T
                    obs <- sum(new_cases[(i+lag-days.reported),1:(i+lag)], na.rm=T)
                    Truth <-  sum(new_cases[(i+lag-days.reported),], na.rm=T)
                    state_day <- weekdays(as.Date(state))

                    if(is.null(prior_)==F){
                        index = which(prior_$theta_cov$dates%in%as.Date(pred.day))
                        lambda =exp(as.vector(c(1,prior_$theta_cov$theta[index]))%*%prior_$coeff)

                        prior <- function(x){dpois(x,lambda =lambda ,log=T)}
                    }
                    Truth_est <- CI_pred(fit, data.frame(time= Analysis_glm$time[length(Analysis_glm$time)],
                                                         days= state_day),
                                         obs,
                                         prior=prior)
                    quantile = quantile(Truth_est,probs = c(0.025,0.5,0.975))
                    model_dt <- rbind(model_dt,
                                      data.frame( prediction_date = as.Date(state), #state
                                                  date            = as.Date(pred.day),                 #date
                                                  sure_deaths     = 0,
                                                  predicted_deaths = quantile[2] - obs,
                                                  obs              = obs,      # observations
                                                  total_lCI        = quantile[1],
                                                  total_uCI        = quantile[3],
                                                  total            = quantile[2]))

                }
            }

        }
        if(is.null(prior_)==F & predicted==F){
            predicted
            index = which(prior_$theta_cov$dates%in%as.Date(pred.day))
            lambda =exp(as.vector(c(1,prior_$theta_cov$theta[index]))%*%prior_$coeff)
            quantile <- qpois(c(0.025,0.5,0.975), lambda = lambda)
            obs <- sum(new_cases[(i+lag-days.reported),1:(i+lag)], na.rm=T)
            model_dt <- rbind(model_dt,
                              data.frame( prediction_date = as.Date(state), #state
                                          date            = as.Date(pred.day),                 #date
                                          sure_deaths     = 0,
                                          predicted_deaths = quantile[2] - obs,
                                          obs              = obs,      # observations
                                          total_lCI        = quantile[1],
                                          total_uCI        = quantile[3],
                                          total            = quantile[2]))
        }
    }
    return(list(model_dt = model_dt, new_cases= new_cases, model = fit,
                vcovs          = vcovs))
}
#' Running the model for all dates
#'
#'@param model_death_dt       - list for deaths output of buildData.R
#'@param model_icu_dt         - list for icu    output of buildData_icu.R
#'@param model_dt             - data.frame of model predicitons
#'@param prior                - use icu to predicit
#'@param days.to.pred         - how long to predicit each days ahead in time
run.model2.all <- function(model_death_dt, model_icu_dt ,prior=T, days.to.pred=15){

    model_dt <- NULL
    if( file.exists(file.path("data", "model", "model_seperate_lag.rds"))){
        model_dt<- readRDS(file.path("data", "model", "model_seperate_lag.rds"))
    }
    prior_ = NULL
    if(prior==T){
        prior_ <- icu_covariates(model_death_dt,model_icu_dt)

    }
    for(i in 0:days.to.pred){
        res <- model2.fit(model_dt,
                                    model_death_dt,
                                    days.reported=i,
                                    prior_ = prior_)
        model_dt <- res$model_dt
    }

    saveRDS(model_dt, file.path("data", "model", "model_seperate_lag.rds"))
    return(model_dt)
}
