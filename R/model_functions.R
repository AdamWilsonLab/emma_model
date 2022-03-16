
# Summarize posteriors
summarize_model_output <- function(model_summary,stan_data, data){
  #posterior predictive

  beta_names<-data.frame(
    pid=as.character(1:ncol(stan_data$x)), #call it pid for easy joining with beta index below
    type="beta",
    xname=as.factor(colnames(stan_data$x)))

  tdata<- model_summary %>%
            mutate(pid=gsub("[]]","",gsub(".*[[]","",variable)),
               parameter=gsub("[[].*","",variable),
               type=case_when(
                 grepl("beta",parameter) ~ "beta",
                 grepl("alpha$|gamma$|lambda$",parameter) ~ "spatial",
                 grepl("tau",parameter) ~ "variability",
                 grepl("^mu$",parameter) ~ "state",
                 grepl("y_obs",parameter) ~ "state",
                 TRUE ~ "other"
               )) %>%  #extract pid from parameter names
  left_join(beta_names)

  if(F)  tdata %>% group_by(parameter,type) %>% summarize(n())

    return(tdata)
}

## Summarize trajectories
summarize_predictions <- function(model_results,stan_data,envdata){


  sdata=tibble(
    cellID=stan_data$y_cellID,
    date=as_date(stan_data$y_date),
    pid=stan_data$pid,
    y_obs=stan_data$y_obs,
    age=stan_data$age,
  )

  state_vars <- model_results %>%
    dplyr::filter(parameter=="y_pred") %>%
    dplyr::select(variable,q5,median,q95) %>%
    bind_cols(sdata) %>% #this assumes there has been no row-shuffling
    left_join(dplyr::select(envdata,cellID,x,y),by="cellID")

  return(state_vars)
}

# Spatial Predictions

create_spatial_outputs <- function(model_results,data_training,envdata) {
  td <- envdata %>%
    left_join(dplyr::select(data_training, cellID, pid))

  spatial_params=c("alpha","gamma","lambda","A")

  stan_spatial <- model_results %>%
    filter(parameter%in%spatial_params) %>%
    mutate(pid=as.numeric(pid)) %>%
    left_join(td,by="pid")

rasters =  foreach(t=spatial_params,.combine=stack) %do% {
    res <- stan_spatial %>%
      filter(parameter==t) %>%
      dplyr::select(x,y,median) %>%
      rasterFromXYZ()
    names(res)=t
    return(res)
  }
    crs(rasters)=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
    return(rasters)
}
