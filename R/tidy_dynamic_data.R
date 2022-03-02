## Download the most recent release from the emma_envdata
tidy_dynamic_data <- function(data){


# Data pre-processing

dynfiles <- open_dataset(sources = list.files("data/envdata","dynamic",full=T))

# dynfiles %>%
#   filter(cellID<5000,variable=="ndvi") %>%
#   mutate(ndvi2=(value/100)-1) %>%
#   collect() %>%
#   as_tibble()

cells_to_keep<- dplyr::select(data,cellID)

dyndata <- dynfiles  %>%
  semi_join(cells_to_keep,by="cellID") %>% #keep only pixels in data
#  filter(cellID%in%cells) %>%
  collect() %>%
  spread(variable,value) %>%
  mutate(date=as_date(date),
         ndvi=(ndvi/100)-1) %>%
  as_tibble()

return(dyndata)
}
