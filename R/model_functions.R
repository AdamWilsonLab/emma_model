
# Summarize posteriors
summarize_model_output <- function(model_output,stan_data, data){
  #posterior predictive
  tdata<- model_output %>%
        mutate(pid=gsub("[]]","",gsub(".*[[]","",variable)),
               parameter=gsub("[[].*","",variable)) #extract pid from parameter names


    return(tdata)
}
  #wrangle
  #  stan_ndvi <- rbind(stan_vb) %>%
  #    select(age=age,NDVI=nd,pid,mean,upper=q95,lower=q5) %>%
  #    filter(pid %in% as.numeric(sample(levels(as.factor(stan_vb$pid)),20)))



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
