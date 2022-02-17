## Download the most recent release from the emma_envdata
tidy_dynamic_data <- function(data){


# Data pre-processing

dynfiles <- open_dataset(sources = list.files("data/envdata","dynamic",full=T))

cells_to_keep<- select(data,cellID)

dyndata <- dynfiles  %>%
  semi_join(cells_to_keep,by="cellID") %>% #keep only pixels in data
#  filter(cellID%in%cells) %>%
  collect() %>%
  spread(variable,value) %>%
  mutate(date=as_date(date),
         ndvi=ndvi/100) %>%
  as_tibble()

return(dyndata)
}
