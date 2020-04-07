# This file serves the r_*() functions (e.g. r_make()) documented at
# https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity # nolint
# and
# https://ropensci.github.io/drake/reference/r_make.html

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().

source('src/packages.R')
source('src/settings.R')
source('src/functions.R')
source('src/plan.R')

drake::drake_config(
    plan = plan,
    verbose = 2, lock_cache = TRUE,
    session_info = TRUE, seed = 12345,
    log_make = file.path("out", "console.log"),
    cache_log_file = file.path("out", "cache.log")
)
