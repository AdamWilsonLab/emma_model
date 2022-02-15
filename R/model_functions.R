#tidy up
merge_data_function <- function(data,dyndata){
  dyndata %>%
    filter(ndvi>0) %>% #remove impossible NDVI values
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

# compile model
compile_model<- function(stan_file='firemodel_predict.stan',dir="_targets/objects"){
#  output_file=file.path(dir,sub("[.]stan","",stan_file))
  cmdstanr::cmdstan_model(stan_file=stan_file,
                compile = TRUE,
                dir=dir)
 # return(output_file) #return the filename so targets can track it
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


# fit model
fit_model<- function(model,data){
  model_results=
    model$variational(
      data = data,
      adapt_engaged=FALSE,
      eta=0.1,
      tol_rel_obj = 0.001,
      seed = 123)
  # the next few lines ensure all the data is actually read into RAM (and not stored in temp files)
  # from https://github.com/stan-dev/cmdstanr/blob/622fa94e31e677878f7d3ed8d79a186edb7e0d6b/R/fit.R#L99-L103
  model_results$draws()
  try(model_results$sampler_diagnostics(), silent = TRUE)
  try(model_results$init(), silent = TRUE)
  try(model_results$profiles(), silent = TRUE)
  #  model_results$save_object(file)
  return(model_results)
}


# Summarize posteriors
summarize_posteriors <- function(model_output,data){
  #posterior predictive
  tdata<- model_output$summary("nd_new","mean","quantile2") %>%
        mutate(pid=gsub("[]]","",gsub(".*[[]","",variable))) %>%  #extract pid from parameter names
    bind_cols(select(data,cellID,date,age,ndvi))  # be careful - this just binds and not a full join - don't change row order!!!!!

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
    mutate(pid=gsub("[]]","",gsub(".*[[]","",variable))) %>%
    bind_cols(select(data,x,y,age,nd))

  foreach(t=unique(raw_data$DA),.combine=stack) %do% {
    stan_spatial %>%
      filter(DA=t) %>%
      select(x,y,age,nd,mean,q5) %>%
      rasterFromXYZ()
  }
}
