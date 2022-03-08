library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
#remotes::install_github("ropensci/stantargets")
library(stantargets)
# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T), source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets"),
               deployment="main")

          Sys.setenv(HOME="/home/rstudio")
          cmdstanr::set_cmdstan_path()#"/home/rstudio/.cmdstanr/cmdstan-2.28.1")
          cmdstanr::check_cmdstan_toolchain()
          #cmdstanr::install_cmdstan()


## Download the most recent data release
list(
  tar_target(
    envdata,sync_envdata(),
    format="file"
  ),
  tar_target(data,
             tidy_static_data(
    envdata,
    remnant_distance=2, #drop pixels within this distance of remnant edge (km)
    region=c(xmin = 18, xmax = 19.5, ymin = -35, ymax = -33), #core
    #region=c(xmin = 18.301425, xmax = 18.524242, ymin = -34.565951, ymax = -34.055531), #peninsula
    sample_proportion= 0.20)),

  tar_target(
    dyndata,
    tidy_dynamic_data(data,
                      date_window=c(ymd("2000-01-01"),ymd("2017-01-01")))
    ),
    tar_target(
      dyndata_validation,
      tidy_dynamic_data(data,
                        date_window=c(ymd("2017-01-01"),ymd("2022-01-01")))
      ),

  tar_target(
    merge_data,
    merge_data_function(data,dyndata)
  ),
  tar_target(
    group_data,
    group_data_function(merge_data)
  ),
  tar_target(
    stan_data,
    stan_data_function(merge_data,group_data)
  ),

# tried mcmc - 500 samples in ~12 hours
#  tar_stan_compile(
#    model,stan_file="firemodel_predict.stan"),
  tar_stan_vb_rep_draws(
    model_results,
    stan_files = "firemodel_predict.stan",
    data = stan_data,
    batches = 1,
    reps = 1,
    combine=F,
    pedantic=T,
#    stdout = R.utils::nullfile(),
#    stderr = R.utils::nullfile(),
    adapt_engaged=F,
    eta=0.1,
    iter = 10000, #should be 1000 or more - 100 is just to run quickly
    garbage_collection=T,
    init=1,
    tol_rel_obj = 0.001
  )

 #
 #
 #
 #     tar_target(posterior_summary,
 #              summarize_posteriors(model_results,merge_data)),
 # # tar_render(report, "index.Rmd")
)


