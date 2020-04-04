# This file serves the r_*() functions (e.g. r_make()) documented at
# https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity # nolint
# and
# https://ropensci.github.io/drake/reference/r_make.html

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().

source('src/packages.R')
source('src/functions.R')
source('src/plan.R')

conf <- drake::drake_config(
    plan = plan
)
