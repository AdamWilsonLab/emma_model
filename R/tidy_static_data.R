## Download the most recent release from the emma_envdata
tidy_static_data <- function(envdata){

  # Data pre-processing

data <- open_dataset(sources = "data/envdata/stable_data.gz.parquet")

# region=st_bbox(
#   c(xmin = 18.301425, xmax = 18.524242, ymin = -34.365951, ymax = -34.055531),
#   crs = st_crs(4326)) %>%
#   st_as_sfc() %>%
#   st_transform(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
#   st_bbox()


data %>%
  filter(remnant_distance.tif>=2) %>%
  collect() %>%
  as_tibble() %>%
  return()

}
