## Download the most recent release from the emma_envdata
tidy_dynamic_data <- function(cells){


# Data pre-processing

dynfiles <- open_dataset(sources = list.files("data/envdata","dynamic",full=T))

#dynfiles$files

#as_date(11706)

data <- dynfiles  %>%
  filter(cellID%in%cells) %>%
  collect() %>%
  as_tibble() %>%
  spread(variable,value) %>%
  mutate(date=as_date(date),
         ndvi=ndvi/100)
return(data)
}
