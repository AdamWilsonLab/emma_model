library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)
#webshot::install_phantomjs()
source("R/get_park_polygons.R")
#tar_load(model_results)

generate_reports <- function(output_directory = "reports/",
                             temp_directory = "data/temp/",
                             report_location = "scratch_code/report_prototype.rmd",
                             model_results = model_results,
                             model_prediction = model_prediction,
                             spatial_outputs = spatial_outputs
){

  #create directories if needed

    print("report env:")
    print(environment())

    if(!dir.exists(file.path(output_directory))){

      dir.create(file.path(output_directory), recursive = TRUE)

    }

  #install basemapR if needed

    if(!"basemapR" %in% rownames(installed.packages())){
      devtools::install_github('Chrisjb/basemapR')
    }

  library(basemapR)

  # Load Park Polygons
  parks <- get_park_polygons(temp_directory = temp_directory,
                             sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                             sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp")

  #Create temp directory (needs to come after get_park_polygons if using the same temp_directory, since the temp folder is deleted)
  if(!dir.exists(file.path(temp_directory))){

    dir.create(file.path(temp_directory),recursive = TRUE)

  }



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

  #convert from date of fire to years since fire

    years_since_fire_raster <-
      terra::app(x = most_recent_fire_raster,
                 fun = function(x){
                   return( time_length(Sys.Date() - as_date(x,origin = lubridate::origin),unit = "years"))
                 })
  #make a polygon version and convert to WGS84 (for plotting ease)
  fires_wgs <- terra::as.polygons(x = years_since_fire_raster) %>%
    st_as_sf() %>% rename(Years = lyr.1) %>%
    st_transform(crs = st_crs(4326))

  #get most recent NDVI data

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

  #Load the NDVI raster
    most_recent_ndvi_raster <- terra::rast(file.path(temp_directory,
                                                     most_recent_ndvi_file$file_name))

  #Fix the values
    most_recent_ndvi_raster <- (most_recent_ndvi_raster/100)-1

  # Generate the reports via a for loop

  for (park_name in unique(parks$CUR_NME)){

  #for (park_name in unique(parks$CUR_NME)[1]){


    focal_park <- parks %>%
      filter(CUR_NME == park_name)

  render(input = report_location,
           output_file = gsub(pattern = " ",replacement = "_",
                                        x = paste0('report.', park_name, '.html')),
           output_dir = output_directory
    )


  }# end for loop



}#end fx

