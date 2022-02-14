library(targets)

# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T), source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate"))



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
    tidy_dynamic_data()
  )
)

# some random EDA

tar_load(data)
data %>%
  gather("variable","value",-x,-y) %>%
  ggplot(aes(x=x,y=y,fill=value))+
  facet_wrap(~variable)+
  geom_raster()

