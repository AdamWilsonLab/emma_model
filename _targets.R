library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
#remotes::install_github("ropensci/stantargets")
library(stantargets)

source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T)[-4], source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets",
                            "doParallel","raster"),
               deployment="main")

Sys.setenv(HOME="/home/rstudio")
cmdstanr::set_cmdstan_path()#"/home/rstudio/.cmdstanr/cmdstan-2.28.1")
#cmdstanr::check_cmdstan_toolchain()
#cmdstanr::install_cmdstan()


# Testing and training time windows
training_window=c("2000-01-01","2020-01-01")
testing_window=c("2020-01-01","2022-01-01")

## Download the most recent data release
list(
  tar_target(
  envdata,
  robust_pb_download(file=NULL,
                     repo="AdamWilsonLab/emma_envdata",
                     dest="data/envdata/",
                     tag="current"),
  format="file"),
  tar_target(data,
   tidy_static_data(
   envdata,
   remnant_distance=2, #drop pixels within this distance of remnant edge (km)
   #region=c(xmin = 18, xmax = 19.5, ymin = -35, ymax = -33), #core
   region=c(xmin = 18.301425, xmax = 18.524242, ymin = -34.565951, ymax = -34.055531), #peninsula
   sample_proportion= 1)),
tar_target(
  data_training,
  filter_training_data(data,
                       envvars=c("CHELSA_bio10_01_V1.2_clipped.tif", #select env vars to use in model
                                 "CHELSA_bio10_02_V1.2_clipped.tif",
                                 "MODCF_seasonality_concentration.tif",
                                 "alos_chili.tif",
                                 "alos_mtpi.tif"))
  ),
  tar_target(
  dyndata_training,
  tidy_dynamic_data(data,date_window=ymd(training_window))
  ),
  tar_target(
    dyndata_validation,
    tidy_dynamic_data(data,date_window=ymd(testing_window))
    ),
  tar_target(
    stan_data,
    create_stan_data(
      data=data_training,
      dyndata=dyndata_training,
      fit=1,
      predict=1)
),

# tried mcmc - 500 samples in ~12 hours
  tar_stan_vb_rep_summary(
    model_output,
    stan_files = "firemodel_generate.stan",
    data = stan_data,
    batches = 1,
    reps = 1,
    combine=T,
    pedantic=T,
#    stdout = R.utils::nullfile(),
#    stderr = R.utils::nullfile(),
    adapt_engaged=F,
    eta=0.11,
    iter = 1000, #should be 1000 or more - 100 is just to run quickly
    garbage_collection=T,
    init=1,
    tol_rel_obj = 0.001
  ),

tar_target(model_results,
           summarize_model_output(model_output, stan_data, data)),

tar_target(spatial_outputs,
                create_spatial_outputs(model_results,data_training,data)),

tar_render(report, "index.Rmd")
)


