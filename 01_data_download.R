x <- open_dataset(sources = "data/processed_data/dynamic_parquet/")

x$schema
x$metadata

x  %>%
  filter(variable != "ndvi")%>%
  filter(date == 11261)%>%
  summarise(mean = mean(value))%>%
  head() %>%
  collect()
