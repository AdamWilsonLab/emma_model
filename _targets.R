library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
library(plotly)
library(leaflet)
#remotes::install_github("ropensci/stantargets")
# if(!"basemapR" %in% rownames(installed.packages())){
#   devtools::install_github('Chrisjb/basemapR')
# }
library(stantargets)
source("scratch_code/report_generator.R") #this should be moved
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

# tar_destroy(ask = F)

# Testing and training time windows
training_window=c("2000-01-01","2020-01-01")
testing_window=c("2020-01-01","2022-01-01")

## Download the most recent data release
list(
  tar_target(
    envdata_files,
    robust_pb_download(file=NULL,
                       repo="AdamWilsonLab/emma_envdata",
                       dest="data/envdata/",
                       tag="current",
                       show_progress=F,
                       overwrite=F,
                       sleep_time=3),
    format="file"),

  tar_target(envdata,
             tidy_static_data(
               envdata_files,
               remnant_distance=2, #drop pixels within this distance of remnant edge (km)
               #region=c(xmin = 18, xmax = 19.5, ymin = -35, ymax = -33), #core
               region=c(xmin = 18.301425, xmax = 18.524242, ymin = -34.565951, ymax = -34.055531), #peninsula
               sample_proportion= 0.8)),
  tar_target(
    data_training,
    filter_training_data(envdata,
                         envvars=c("Mean_Annual_Air_Temperature"="CHELSA_bio10_01_V1.2.tif", #select env vars to use in model
                                   "Mean_Annual_Precipitation"="CHELSA_bio10_12_V1.2.tif",
                                   "Mean_Monthly_Precipitation_In_Driest_Quarter"="CHELSA_bio10_17_V1.2.tif",
                                   "Mean_Annual_Cloud_Frequency"="MODCF_meanannual.tif",
                                   "Cloud_Seasonal_Concentration"="MODCF_seasonality_concentration.tif",
                                   "Topographic_Diversity"="alos_topodiversity.tif",
                                   "ALOS_CHILI"="alos_chili.tif",
                                   "ALOS_MTPI"="alos_mtpi.tif"))
  ),
  tar_target(
    dyndata_training,
    tidy_dynamic_data(envdata,date_window=ymd(training_window))
  ),
  tar_target(
    dyndata_validation,
    tidy_dynamic_data(envdata,date_window=ymd(testing_window))
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
  tar_stan_vb(
    model,
    stan_files = "postfire_season.stan",
    data = stan_data,
    quiet=T,
    pedantic=F,
    adapt_engaged=F,
    eta=0.11,
    iter = 10000, #should be 1000 or more - 100 is just to run quickly - CP converged after 6400
    garbage_collection=T,
    init=1,
    tol_rel_obj = 0.001,
    output_samples = 1000,
    format_df="parquet",
    #format="parquet"
  ),

   tar_target(model_results,
              summarize_model_output(model_summary_postfire_season, stan_data, envdata)),
   tar_target(model_prediction,
             summarize_predictions(model_results,stan_data,envdata)),
  tar_target(spatial_outputs,
             create_spatial_outputs(model_results,data_training,envdata)),
 # tar_target(release,
 #            release_posteriors(
 #              model_output,
 #              file="targets/objects/model_results",
 #              repo = "AdamWilsonLab/emma_model",
 #              tag = "current")),
   tar_render(report, "index.Rmd"),
 tar_target(name = reports,
            command = generate_reports(output_directory = "reports/",
                                       temp_directory = "data/temp/",
                                       report_location = "report_prototype.rmd",
                                       model_results = model_results,
                                       model_prediction = model_prediction,
                                       spatial_outputs = spatial_outputs))
)
