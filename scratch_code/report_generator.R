library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)

#webshot::install_phantomjs()

temp_directory <- "data/temp/"

# Create a temporary directory if needed
  if(!dir.exists(file.path(temp_directory))){
    dir.create(file.path(temp_directory),recursive = TRUE)
  }

# Load Park Polygons
  source("R/get_park_polygons.R")
  parks <- get_park_polygons()

# Get list of available env data files
  env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")

#get most recent fire data
  env_files %>%
    filter(tag == "processed_most_recent_burn_dates") %>%
    mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
    mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
    slice(which.max(as_date(file_date))) -> most_recent_fire_file

  pb_download(file = most_recent_fire_file$file_name,
              dest = file.path(temp_directory),
              repo = "AdamWilsonLab/emma_envdata",
              tag = most_recent_fire_file$tag)

  most_recent_fire_raster <- terra::rast(file.path(temp_directory, most_recent_fire_file$file_name))
  most_recent_fire_raster[most_recent_fire_raster==0] <- NA #toss NAs

    #plot(most_recent_fire_raster)

  #convert from date of fire to years since fire

    years_since_fire_raster <-
    terra::app(x = most_recent_fire_raster,
               fun = function(x){
                 return( time_length(Sys.Date() - as_date(x,origin = lubridate::origin),unit = "years"))
               })

    fires_wgs <- terra::as.polygons(x = years_since_fire_raster) %>%
      st_as_sf() %>% rename(Years = lyr.1) %>%
      st_transform(crs = st_crs(4326))

#get most recent NDVI data
    unique(env_files$tag)

    env_files %>%
      filter(tag == "raw_ndvi_modis") %>%
      filter(grepl(pattern = ".tif",x = file_name)) %>%
      mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
      mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
      slice(which.max(as_date(file_date))) -> most_recent_ndvi_file

    pb_download(file = most_recent_ndvi_file$file_name,
                dest = file.path(temp_directory),
                repo = "AdamWilsonLab/emma_envdata",
                tag = most_recent_ndvi_file$tag)

    most_recent_ndvi_raster <- terra::rast(file.path(temp_directory, most_recent_ndvi_file$file_name))
    most_recent_ndvi_raster <- (most_recent_ndvi_raster/100)-1
    plot(most_recent_ndvi_raster)

# Generate the reports via a for loop
  for (park_name in unique(parks$CUR_NME)){

    focal_park <- parks %>%
                  filter(CUR_NME == park_name)

    render("scratch_code/report_prototype.rmd",output_file = paste0('report.', park_name, '.html'))

  }
