###
# function that updates all Npost, nprior
#
###
library(Matrix)
library(invgamma)
source(file.path("src","model","prob_dens.R"))
source(file.path("src","model","regression.R"))
source(file.path("src","model","model.R"))
source(file.path("src","model","icu_regression.R"))


#' Running n.estimate for all data
#'
#'@param list for deaths output of buildData.R
#'@param list for icu    output of buildData_icu.R
#'@param int  days of lag to use to fit coeffients
run.model.all <- function(deaths, icu){
    ###
    #  line 49,56 are file loaded
    #  stored 94,95
    #

    #i icu_covariates  updateras filer
    cov_ <- icu_covariates(deaths, icu)

    store_data_folder <- file.path("data", "model")

    ####

    ###
    # fixed model parameters
    ###
    days_run <- 30
    sim = 40000
    model_parameters <- list(sim           = sim,
                             burnin        = ceiling(0.5*sim),
                             N.days.fixed  =  3,
                             quantile      = c(0.025,0.975))
    prior_list0    <- list(mu_beta        = c(0,0,0,0),
                           Sigma_beta     = 2*diag(4),
                           a_sigma        = c(3,3),
                           b_sigma        = c(5/2,5/2),
                           theta_mu         = c(0,0),
                           theta_Sigma    =  5*diag(2),
                           mu_phi         = 1,
                           sigma_phi      = 1)

    for(j in (days_run+1):length(deaths$dates)){
        start_ = j - days_run # run the last 31 days
        day_is_run = file.exists(paste(store_data_folder,"/Npost_",j,'.rds',sep=""))
        if(day_is_run==F){
            cat('running day  = ', as.character(deaths$dates[j]),'\n',sep="")
            if(start_ <= days_run+ 1){
                prior_list <- prior_list0
                prior_list$n_obs <- 0
            }else{
                prior_list <- readRDS(paste(store_data_folder,"/prior_",start_-1,'.rds',sep=""))
            }

            report_cleaned <- report_clean(deaths$detected[start_:j,start_:j],deaths$dates[start_:j])
            new_cases <- newCases(report_cleaned)
            rownames(new_cases) <- as.character(deaths$dates[start_:j])
            colnames(new_cases) <- as.character(deaths$dates[start_:j])

            cov_index <- cov_$theta_cov$date%in% as.Date(colnames(new_cases))
            cov_data  <- cov_$theta_cov$theta[cov_index]

            result <- model(new_cases,
                            model_parameters,
                            prior_list,
                            covariates = cov_data,
                            beta_covarites = cov_$coeff
                            )
            result$posteriror_list$n_obs <- prior_list$n_obs + days_run

            ##
            # fix update priors
            # adding forgetting factor
            ##
            X_corr <- diag(sqrt(diag(t(result$X_full)%*%result$X_full)))
            result$posteriror_list$Sigma_beta <- 0.5*X_corr%*%result$posteriror_list$Sigma_beta%*%X_corr

            a_ <- result$posteriror_list$a_sigma - prior_list$a_sigma
            a_ <- prior_list$a_sigma + 0.5* (result$posteriror_list$a_sigma - prior_list$a_sigma)
            result$posteriror_list$a_sigma <- a_
            result$posteriror_list$b_sigma <- ((result$posteriror_list$a_sigma - 1)/(a_-1) )* result$posteriror_list$b_sigma

            result$posteriror_list$theta_Sigma <- 20*result$posteriror_list$theta_Sigma
            ##
            #
            ##
            N_est_true <- apply(deaths$detected,1,max,na.rm=T)
            Npost <- cbind(deaths$dates[j],result$Npost, N_est_true[as.character(deaths$dates)%in%as.character(result$Npost$dates)])
            colnames(Npost)[c(1,7)] <- c("State",'Truth')
            saveRDS(Npost, file = paste(store_data_folder,"/Npost_",j,'.rds',sep=""))
            saveRDS(result$posteriror_list, file = paste(store_data_folder,"/prior_",j,'.rds',sep=""))
        }
    }
}
