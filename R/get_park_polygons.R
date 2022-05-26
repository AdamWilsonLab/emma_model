library(sf)
library(tidyverse)
library(piggyback)

#' @param temp_directory Temporary working directory (will be deleted)
#' @param sacad_filename filename of the sacad shapefile
#' @param sapad_filename filename of the sapad shapefile
#' @note This function uses the emma_envdata domain to crop the park polygons
#' @return sf dataframe object containing the parks
get_park_polygons <- function(temp_directory = "data/temp/parks",
                              sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                              sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp"){

  # Pull in different protected areas

    sacad <- st_read(sacad_filename)
    sapad <- st_read(sapad_filename)

  # Combine the two datasets

    sacad %>%
      mutate( PROC_DOC = NA) %>%
      select(colnames(sapad)) %>%
      bind_rows(sapad) -> all_pas

  rm(sacad,sapad)

  # Create directory if needed

    if(!dir.exists(temp_directory)){
      dir.create(temp_directory, recursive = TRUE)
    }

  # Download domain

    pb_download(file = "domain.gpkg",
                tag = "raw_static",
                repo = "AdamWilsonLab/emma_envdata",
                dest = temp_directory)

  # Read domain

    domain <- st_read(file.path(temp_directory, "domain.gpkg"))

  # Match projections

    all_pas <- st_transform(x = all_pas,
                           crs = st_crs(domain))

  # Filter to National Parks only

    all_pas %>%
      filter(grepl(pattern = "national park",
                   x = SITE_TYPE,ignore.case = TRUE)) -> nps


  # Crop PAs

    all_pas <-
    st_intersection(x = domain,
                    y = st_make_valid(all_pas))

  # Update metadata as needed

    all_pas %>%
      select(-domain) -> all_pas #don't need domain column

  # return data product

}
############################################



