# This file serves the r_*() functions (e.g. r_make()) documented at
# https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity # nolint
# and
# https://ropensci.github.io/drake/reference/r_make.html

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().

source('src/packages.R')
source('src/functions.R')
source('src/model/prepare_data.R')
source('src/model/run.model.all.R')
source('src/model/seperate_lag_model.R')
source('src/model/eval.coverage_prediction.R')
source('src/model/GPsmoothing_model.R')
source('src/plan.R')

drake::drake_config(
    plan = plan,
    verbose = 2, lock_cache = TRUE,
    session_info = TRUE, seed = 12345,
    log_make = file.path("console.log"),
    cache_log_file = file.path("cache.log"),
    keep_going = TRUE # keep going despite error in UK code
)
