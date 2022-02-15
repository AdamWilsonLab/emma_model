library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)

# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T), source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate"))

          Sys.setenv(HOME="/home/rstudio")
          #cmdstanr::set_cmdstan_path("/home/rstudio/.cmdstanr/cmdstan-2.28.1")
          cmdstanr::check_cmdstan_toolchain()
          #cmdstanr::install_cmdstan()


## Download the most recent data release
list(
  tar_target(
    envdata,
    piggyback::pb_download(repo = "AdamWilsonLab/emma_envdata", dest = "data/envdata")
  ),
  tar_target(
    data,
    tidy_static_data()
  ),
  tar_target(
    dyndata,
    tidy_dynamic_data(cells=data$cellID)
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
  tar_target(model,compile_model()),
  tar_target(model_fit, fit_model(model, stan_data)),
  tar_target(posterior_summary,
             summarize_posteriors(model_fit,merge_data)),
 tar_render(report, "index.Rmd")
)
