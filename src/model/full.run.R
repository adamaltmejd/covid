##
#


source(file.path("src","model","prob_dens.R"))
source(file.path("src","model","regression.R"))
source(file.path("src","model","model.R"))
library(foreach)
library(doParallel)
days_run <- 30
sim <- 40000
n.clust <- 7
store_data_folder <- file.path("data","tmp","model2","v12")
model_parameters <- list(sim           = sim,
                         burnin        = ceiling(0.5*sim),
                         N.days.fixed  =  3,
                         quantile      = c(0.025,0.975))

data <- readRDS(file.path("data", "model", "processed_data.rds"))

prior_list0    <- list(mu_beta        = c(0,0,0,0),
                       Sigma_beta     = 2*diag(4),
                       a_sigma        = c(3,3),
                       b_sigma        = c(5/2,5/2),
                       theta_mu         = c(0,0),
                       theta_Sigma    =  5*diag(2),
                       mu_phi         = 1,
                       sigma_phi      = 1)

N_est_true <- apply(data$detected,1,max, na.rm=T)
N.obs <- length(data$dates)

cl <- parallel::makeCluster(n.clust)
doParallel::registerDoParallel(cl)
foreach(j = (days_run+1):(N.obs-30)) %dopar%{
    library(Matrix)
    library(invgamma)
    start_ = j - days_run # run the last 31 days

    if(start_ <= days_run+ 1){
        prior_list <- prior_list0
        prior_list$n_obs <- 0
    }else{
        prior_list <- readRDS(paste(store_data_folder,"/prior_",start_-1,'.rds',sep=""))
    }

    report_cleaned <- report_clean(data$detected[start_:j,start_:j],data$dates[start_:j])
    new_cases <- newCases(report_cleaned)
    rownames(new_cases) <- as.character(data$dates[start_:j])
    colnames(new_cases) <- as.character(data$dates[start_:j])


    result <- model(new_cases, model_parameters, prior_list)
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
    Npost <- cbind(data$dates[j],result$Npost, N_est_true[as.character(data$dates)%in%as.character(result$Npost$dates)])
    colnames(Npost)[c(1,7)] <- c("State",'Truth')
    saveRDS(Npost, file = paste(store_data_folder,"/Npost_",j,'.rds',sep=""))
    saveRDS(result$posteriror_list, file = paste(store_data_folder,"/prior_",j,'.rds',sep=""))
    saveRDS(result$posteriror_sample, file = paste(store_data_folder,"/sample_",j,'.rds',sep=""))
}
parallel::stopCluster(cl)


