## Download the most recent release from the emma_envdata
tidy_static_data <- function(){

# Data pre-processing

data <- open_dataset(sources = "data/envdata/stable_data.gz.parquet")

data %>%
  collect() %>%
  as_tibble() %>%
  return()

}
