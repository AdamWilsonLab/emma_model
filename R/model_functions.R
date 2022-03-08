#tidy up
merge_data_function <- function(data,dyndata){
  dyndata %>%
    #filter(ndvi>0) %>% #remove impossible NDVI values
    mutate(age=time_since_fire/365.23) %>% #convert age from days to years
    left_join(data,by="cellID") %>%
    select(cellID,date,age,ndvi,
           env1=CHELSA_bio10_01_V1.2_clipped.tif,
           env2=CHELSA_bio10_02_V1.2_clipped.tif) %>%
    na.omit() %>%
    mutate(pid = as.numeric(as.factor(cellID))) %>%
    mutate(env1 = (env1 - mean(env1))/sd(env1),env2 = (env2 - mean(env2))/sd(env2)) %>%  #standardize env variables
    return()
}


# group data
#plots level data on env conditions
group_data_function <- function(data){
  data %>%
    group_by(pid) %>%
    summarise(env1 = mean(env1),env2 = mean(env2))
}


#prep data for stan
stan_data_function <- function(data,group_data){
  list(N = nrow(data),
       J= nrow(group_data),
       nd=data$ndvi,
       age= data$age,
       pid=data$pid,
       envg1 = group_data$env1,
       envg2 = group_data$env2)
}



# Summarize posteriors
parameter_summary <- function(model_output,data){
  #posterior predictive
  tdata<- model_output %>%
        mutate(pid=gsub("[]]","",gsub(".*[[]","",variable)),
               parameter=gsub("[[].*","",variable)) %>%  #extract pid from parameter names
    filter(!variable%in%c("mu","nd_new"))

  #wrangle
  #  stan_ndvi <- rbind(stan_vb) %>%
  #    select(age=age,NDVI=nd,pid,mean,upper=q95,lower=q5) %>%
  #    filter(pid %in% as.numeric(sample(levels(as.factor(stan_vb$pid)),20)))

  return(tdata)
}

# Spatial Predictions

spatial_outputs <- function(posteriors) {
  message("This function doesn't work - need to get actual dates from the original data!!!!!")
  stan_spatial <- stan_vb %>%
    mutate(pid=gsub("[]]","",gsub(".*[[]","",variable)))

  foreach(t=unique(raw_data$DA),.combine=stack) %do% {
    stan_spatial %>%
      filter(DA=t) %>%
      select(x,y,age,nd,mean,q5) %>%
      rasterFromXYZ()
  }
}
