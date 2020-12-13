###
# test icu_regression
#
###

source(file.path("src","model", "icu_regression.R"))

deaths <- readRDS(file.path("data", "model", "processed_data.rds"))
icu    <- readRDS(file.path("data", "model", "processed_data_icu.rds"))
cov_ <- icu_covariates(deaths, icu)
