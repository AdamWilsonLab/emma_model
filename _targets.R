library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
#remotes::install_github("ropensci/stantargets")
library(stantargets)

source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")

# source all files in R folder
  #lapply(list.files("R",pattern="[.]R",full.names = T)[-4], source)
  list.files("R",pattern="[.]R",full.names = T) %>%
    as.data.frame()%>%
    rename(file=".")%>%
    filter(!file %in% c("R/sync_envdata.R",
                        "R/detect_anomalies.R",
                        "R/plot_timeseries.R"))%>%
    pull(file)%>%
    lapply(source)


options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets",
                            "doParallel","raster"),
               deployment="main")

#Sys.setenv(HOME="/home/rstudio")
cmdstanr::set_cmdstan_path()#"/home/rstudio/.cmdstanr/cmdstan-2.28.1")
#cmdstanr::check_cmdstan_toolchain()
#cmdstanr::install_cmdstan()

# tar_destroy(ask = F)

# Testing and training time windows
training_window=c("2000-01-01","2020-01-01")
testing_window=c("2020-01-01","2023-01-01")
predicting_window=c("2000-01-01","2023-01-01")
#predicting_window=c("2020-01-01", as.character(Sys.Date()))

#For dev
# training_window=c("2020-01-01","2022-01-01")
# testing_window=c("2022-01-01","2023-01-01")
# predicting_window=c("2022-01-01","2023-01-01") #need to revise the predicting code to make it more memory efficient



## Download the most recent data release
list(

  # tar_age(name = envdata_files,
  #         command = robust_pb_download(file=NULL,
  #                                      repo="AdamWilsonLab/emma_envdata",
  #                                      dest="data/envdata/",
  #                                      tag="current",
  #                                      show_progress=F,
  #                                      overwrite=F),
  #         age = as.difftime(7, units = "days"),
  #         #age = as.difftime(12, units = "hours"),
  #         format = "file"
  #         ),

  tar_target(
    envdata_files,
    robust_pb_download(file=NULL,
                       repo="AdamWilsonLab/emma_envdata",
                       dest="data/envdata/",
                       tag="current",
                       show_progress=F,
                       overwrite=T),
    format="file"),

  tar_target(envdata,
             tidy_static_data(
               envdata_files,
               remnant_distance=2, #drop pixels within this distance of remnant edge (km)
               region=c(xmin = 16, xmax = 28, ymin = -35, ymax = -28), #whole region
               #region=c(xmin = 18, xmax = 19.5, ymin = -35, ymax = -33), #core
               #region=c(xmin = 18.301425, xmax = 18.524242, ymin = -34.565951, ymax = -34.055531), #peninsula
               sample_proportion= .5)),

  tar_target(
    data_training,
    filter_training_data(envdata,
                         envvars=c("CHELSA_bio10_01_V1.2.tif", #select env vars to use in model
                                   "CHELSA_bio10_02_V1.2.tif",
                                   "MODCF_seasonality_concentration.tif",
                                   "alos_chili.tif",
                                   "alos_mtpi.tif",
                                   "alos_landforms.tif",
                                   "alos_topodiversity.tif",
                                   "nasadem.tif",
                                   "soil_EC_mS_m.tif",
                                   "soil_Ext_K_cmol_kg.tif",
                                   "soil_Ext_Na_cmol_kg.tif",
                                   "soil_Ext_P_mg_kg.tif",
                                   "soil_pH.tif",
                                   "soil_soil_Total_N_.tif",
                                   "soil_Total_C_.tif",
                                   "soil_Total_C_pct.tif",
                                   "soil_Total_N_.tif",
                                   "soil_Total_N_pct.tif"))%>%
      dplyr::select(cellID,
                    CHELSA_bio10_01_V1.2.tif,
                    CHELSA_bio10_02_V1.2.tif,
                    MODCF_seasonality_concentration.tif,
                    alos_chili.tif,
                    alos_mtpi.tif,
                    pid)
  ),

  tar_target(
    soil_data_training,
    filter_training_data(envdata,
                         envvars=c("CHELSA_bio10_01_V1.2.tif", #select env vars to use in model
                                   "CHELSA_bio10_02_V1.2.tif",
                                   "MODCF_seasonality_concentration.tif",
                                   "alos_chili.tif",
                                   "alos_mtpi.tif",
                                   "alos_landforms.tif",
                                   "alos_topodiversity.tif",
                                   "nasadem.tif",
                                   "soil_EC_mS_m.tif",
                                   "soil_Ext_K_cmol_kg.tif",
                                   "soil_Ext_Na_cmol_kg.tif",
                                   "soil_Ext_P_mg_kg.tif",
                                   "soil_pH.tif",
                                   "soil_soil_Total_N_.tif",
                                   "soil_Total_C_.tif",
                                   "soil_Total_C_pct.tif",
                                   "soil_Total_N_.tif",
                                   "soil_Total_N_pct.tif"))%>%
      dplyr::select(cellID,
                    alos_chili.tif,
                    alos_landforms.tif,
                    alos_mtpi.tif,
                    alos_topodiversity.tif,
                    nasadem.tif,
                    soil_EC_mS_m.tif,
                    soil_Ext_K_cmol_kg.tif,
                    soil_Ext_Na_cmol_kg.tif,
                    soil_Ext_P_mg_kg.tif,
                    soil_pH.tif,
                    soil_soil_Total_N_.tif,
                    soil_Total_C_.tif,
                    soil_Total_C_pct.tif,
                    soil_Total_N_.tif,
                    soil_Total_N_pct.tif,
                    pid)
  ),

  tar_target(
    dyndata_training,
    tidy_dynamic_data(data = envdata,
                      date_window = ymd(training_window))
  ),

  tar_target(
    dyndata_validation,
    tidy_dynamic_data(envdata,
                      date_window = ymd(testing_window))
  ),
  tar_target(
    stan_data,
    create_stan_data(
      data=data_training,
      soil_data = soil_data_training,
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
    quiet=T,
    reps = 1,
    variables = c("alpha","gamma","lambda","alpha_beta","gamma_beta","lambda_beta"),
    combine=T, #I THINK setting this to false changes the model output, breaking downsteam code
    pedantic=T,
    force_recompile=F,
    #    stdout = R.utils::nullfile(),
    #    stderr = R.utils::nullfile(),
    adapt_engaged=F,
    eta=0.11,
    iter = 10000, #should be 1000 or more - 100 is just to run quickly
    memory = "transient",
    garbage_collection=T,
    init=1,
    tol_rel_obj = 0.001,
    output_samples = 10
  )
  ,



  #~~~~~~~~~~V

  # note: don't make the region too large or it will break things (e.g. -180 to 180, -90 to 90), probably because of great circle issues?


  tar_target(envdata_predicting,
             tidy_static_data(
               envdata = envdata_files,
               remnant_distance=2, #drop pixels within this distance of remnant edge (km)
               region=c(xmin = 16, xmax = 28, ymin = -35, ymax = -28), #whole region
               #region=c(xmin = 18, xmax = 19.5, ymin = -35, ymax = -33), #core
               #region=c(xmin = 18.301425, xmax = 18.524242, ymin = -34.565951, ymax = -34.055531),#peninsula
               sample_proportion= 1)),

  tar_target(
    data_predicting,
    filter_training_data(data = envdata_predicting,
                         envvars=c("CHELSA_bio10_01_V1.2.tif", #select env vars to use in model
                                   "CHELSA_bio10_02_V1.2.tif",
                                   "MODCF_seasonality_concentration.tif",
                                   "alos_chili.tif",
                                   "alos_mtpi.tif",
                                   "alos_landforms.tif",
                                   "alos_topodiversity.tif",
                                   "nasadem.tif",
                                   "soil_EC_mS_m.tif",
                                   "soil_Ext_K_cmol_kg.tif",
                                   "soil_Ext_Na_cmol_kg.tif",
                                   "soil_Ext_P_mg_kg.tif",
                                   "soil_pH.tif",
                                   "soil_soil_Total_N_.tif",
                                   "soil_Total_C_.tif",
                                   "soil_Total_C_pct.tif",
                                   "soil_Total_N_.tif",
                                   "soil_Total_N_pct.tif"))%>%
      dplyr::select(cellID,
                    CHELSA_bio10_01_V1.2.tif,
                    CHELSA_bio10_02_V1.2.tif,
                    MODCF_seasonality_concentration.tif,
                    alos_chili.tif,
                    alos_mtpi.tif,
                    pid)),

  tar_target(
    soil_data_predicting,
    filter_training_data(envdata_predicting,
                         envvars=c("alos_chili.tif",
                                   "alos_landforms.tif",
                                   "alos_mtpi.tif",
                                   "alos_topodiversity.tif",
                                   "nasadem.tif",
                                   "soil_EC_mS_m.tif",
                                   "soil_Ext_K_cmol_kg.tif",
                                   "soil_Ext_Na_cmol_kg.tif",
                                   "soil_Ext_P_mg_kg.tif",
                                   "soil_pH.tif",
                                   "soil_soil_Total_N_.tif",
                                   "soil_Total_C_.tif",
                                   "soil_Total_C_pct.tif",
                                   "soil_Total_N_.tif",
                                   "soil_Total_N_pct.tif",
                                   "CHELSA_bio10_01_V1.2.tif",
                                   "CHELSA_bio10_02_V1.2.tif",
                                   "MODCF_seasonality_concentration.tif"))%>%
      dplyr::select(cellID,
                    alos_chili.tif,
                    alos_landforms.tif,
                    alos_mtpi.tif,
                    alos_topodiversity.tif,
                    nasadem.tif,
                    soil_EC_mS_m.tif,
                    soil_Ext_K_cmol_kg.tif,
                    soil_Ext_Na_cmol_kg.tif,
                    soil_Ext_P_mg_kg.tif,
                    soil_pH.tif,
                    soil_soil_Total_N_.tif,
                    soil_Total_C_.tif,
                    soil_Total_C_pct.tif,
                    soil_Total_N_.tif,
                    soil_Total_N_pct.tif,
                    pid)
  ),

  tar_target(
    dyndata_predicting,
    tidy_dynamic_data(data = envdata_predicting,
                      date_window = ymd(predicting_window))
  ),

  tar_target(
    predict_data,
    create_stan_data_prediction(
      data=data_predicting,
      soil_data = soil_data_predicting,
      dyndata=dyndata_predicting,
      fit=0,
      predict=1,
      default_fire_age = 40.001)
  ),



  tar_target(
    predicted_data,
    predict_from_model(model_output = model_output,
                       predict_data = predict_data)
  ),

  # tar_target(
  # anomaly_detection,
  # detect_anomalies(predicted_data = predicted_data)
  # ),


  # tar_stan_vb_rep_summary(
  #   prior_predict_output,
  #   stan_files = "firemodel_prior_predict.stan",
  #   data = stan_data,
  #   batches = 1,
  #   quiet=T,
  #   reps = 1,
  #   combine=T,
  #   pedantic=T,
  #   force_recompile=F,
  #   #    stdout = R.utils::nullfile(),
  #   #    stderr = R.utils::nullfile(),
  #   adapt_engaged=F,
  #   eta=0.11,
  #   iter = 100, #should be 1000 or more - 100 is just to run quickly
  #   garbage_collection=T,
  #   init=1,
  #   tol_rel_obj = 0.001
  # ),



  #~~~~~~~~~~~^

  # Release outputs
  tar_target(publish_model_output,
             release_model_output(model_output = model_output,
                                  temp_directory_output = "data/model_output",
                                  output_tag = "model_output",
                                  chunk_size = NULL,
                                  sleep_time = 1)),

  tar_target(publish_model_predictions,
             release_ndvi_predictions(predicted_data = predicted_data,
                                      temp_directory_output = "data/predicted_data",
                                      output_tag = "ndvi_predictions",
                                      chunk_size = NULL,
                                      sleep_time = 1)),

  tar_target(publish_training_cellIDs,
             release_training_cellIDs(envdata,
                                      temp_directory_output = "data/cellIDs",
                                      output_tag = "cellIDs",
                                      chunk_size = NULL,
                                      sleep_time = 1)),


  # Summaries

  tar_target(model_results,
             summarize_model_output(model_output, stan_data, envdata)),

  #commenting this out, as we're currently not including mu in model output
    # tar_target(model_prediction,
    #            summarize_predictions(model_results,stan_data,envdata)),

  tar_target(spatial_outputs,
             create_spatial_outputs(model_results,data_training,envdata))
  #,tar_render(report, "index.Rmd")




)#end of list of targets
