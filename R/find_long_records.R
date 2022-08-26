#' Function to find pixels with some % of ndvi observations and a fire that occurs in the record by specifying needed age range
#' The default range c(2,10) will ensure that at least one fire happens within 2 years of the start of the record and it runs for at least 10 years.
#' @param age_range vector length 2 - include pixels with ages in this range


find_long_records<-function(env_files,age_range=c(2,10),ndvi_prob=0.8){

  age_files <- open_dataset(sources = list.files("data/envdata","dynamic_parquet-time_since_fire",full=T))
  ndvi_files <- open_dataset(sources = list.files("data/envdata","dynamic_parquet-ndvi",full=T))

  long_aged_pixels <-
    age_files  %>%
    filter(value!=0,value>age_range[1]*365,value>age_range[2]*365) %>%
    collect() %>%
    group_by(cellID) %>%
    summarize(n_age=sum(n())) %>%
    as_tibble()

  good_ndvi_pixels <-
    ndvi_files  %>%
    filter(!is.na(value)) %>%
    collect() %>%
    as_tibble() %>%
    group_by(cellID) %>%
    summarize(n=sum(n()))

  max_number_ndvi=max(good_ndvi_pixels$n)

  keep_pixels <- long_aged_pixels %>%
    left_join(good_ndvi_pixels, by="cellID") %>%
    filter(n>(max_number_ndvi*ndvi_prob)) %>%
    dplyr::select(cellID)

  return(keep_pixels)
}
#' =======
#' #' Function to find pixels with some % of ndvi observations and a fire that occurs in the record by specifying needed age range
#' #' The default range c(2,10) will ensure that at least one fire happens within 2 years of the start of the record and it runs for at least 10 years.
#' #' @param age_range vector length 2 - include pixels with ages in this range
#'
#'
#' find_long_records<-function(env_files,age_range=c(2,10),ndvi_prob=0.8){
#'
#'   age_files <- open_dataset(sources = list.files("data/envdata","dynamic_parquet-time_since_fire",full=T))
#'   ndvi_files <- open_dataset(sources = list.files("data/envdata","dynamic_parquet-ndvi",full=T))
#'
#'   long_aged_pixels <-
#'     age_files  %>%
#'     filter(value!=0,value>age_range[1]*365,value>age_range[2]*365) %>%
#'     collect() %>%
#'     group_by(cellID) %>%
#'     summarize(n_age=sum(n())) %>%
#'     as_tibble()
#'
#'   good_ndvi_pixels <-
#'     ndvi_files  %>%
#'     filter(!is.na(value)) %>%
#'     collect() %>%
#'     as_tibble() %>%
#'     group_by(cellID) %>%
#'     summarize(n=sum(n()))
#'
#'   max_number_ndvi=max(good_ndvi_pixels$n)
#'
#'   keep_pixels <- long_aged_pixels %>%
#'     left_join(good_ndvi_pixels, by="cellID") %>%
#'     filter(n>(max_number_ndvi*ndvi_prob)) %>%
#'     dplyr::select(cellID)
#'
#'   return(keep_pixels)
#' }
#' >>>>>>> origin/dev-brian
