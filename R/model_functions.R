
# Summarize posteriors
summarize_model_output <- function(model_output,stan_data, data){
  #posterior predictive

  beta_names<-data.frame(
    pid=as.character(1:ncol(stan_data$x)), #call it pid for easy joining with beta index below
    type="beta",
    xname=as.factor(colnames(stan_data$x)))

  tdata<- model_output %>%
            mutate(pid=gsub("[]]","",gsub(".*[[]","",variable)),
               parameter=gsub("[[].*","",variable),
               type=case_when(
                 grepl("beta",parameter) ~ "beta",
                 grepl("alpha$|gamma$|lambda$",parameter) ~ "spatial",
                 grepl("tau",parameter) ~ "variability",
                 grepl("^mu$",parameter) ~ "state",
                 grepl("ndvi$",parameter) ~ "state",
                 TRUE ~ "other"
               )) %>%  #extract pid from parameter names
  left_join(beta_names)

  if(F)  tdata %>% group_by(parameter,type) %>% summarize(n())

    return(tdata)
}


# Spatial Predictions

create_spatial_outputs <- function(model_results,data_training,data) {
  td <- data %>%
    left_join(dplyr::select(data_training, cellID, pid))

  spatial_params=c("alpha","gamma","lambda")

  stan_spatial <- model_results %>%
    filter(parameter%in%spatial_params) %>%
    mutate(pid=as.numeric(pid)) %>%
    left_join(td,by="pid")

rasters =  foreach(t=spatial_params,.combine=stack) %do% {
    res <- stan_spatial %>%
      filter(parameter==t) %>%
      dplyr::select(x,y,mean) %>%
      rasterFromXYZ()
    names(res)=t
    return(res)
  }
}
