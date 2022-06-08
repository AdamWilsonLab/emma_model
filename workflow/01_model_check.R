## explore model fitting and generate data from prior
library(cmdstanr)
library(posterior)
library(bayesplot)
# load data from workflow
tar_load(data)
tar_load(dyndata)

envvars=c("CHELSA_bio10_01_V1.2_clipped.tif","CHELSA_bio10_02_V1.2_clipped.tif")

data_training <- data %>%
  filter(fynbos,model_domain,training) %>%
  ungroup() %>%
  dplyr::select(cellID,envvars) %>%
  na.omit() %>%
  arrange(cellID) %>%
  mutate(pid = as.numeric(as.factor(cellID))) #%>% #reset pid to 1:length(pid) to ease in indexing

pid_lookup= data_training %>%
  dplyr::select(cellID, pid)

dyndata_training= dyndata %>%
  left_join(pid_lookup) %>%
  filter(!is.na(pid),!is.na(ndvi),!is.na(age)) %>%
  arrange(pid,date)

if(F){
ggplot(dyndata_lookup,aes(x=date,y=ndvi))+geom_line()+facet_wrap(~cellID)
ggplot(filter(dyndata_lookup,cellID==2963135),aes(x=date,y=ndvi))+geom_line()+facet_wrap(~cellID)

dyndata %>%
  filter(!is.na(ndvi)) %>%
  ggplot(aes(x=date,y=as.numeric(as.factor(cellID)),fill=ndvi))+geom_tile()+
  ylab("Pixel ID")

}

xvar=dplyr::select(data_training,-cellID, -pid)

mod <- cmdstan_model("firemodel_generate.stan")
mod$print()

fit_vb <- mod$variational(
  data =  list(N = nrow(dyndata_training),
               J= nrow(data_training),
               ndvi=dyndata_training$ndvi,
               age= dyndata_training$age,
               pid=dyndata_training$pid,
               x = xvar,
               P = ncol(xvar),
               run_estimation=T),
  seed = 123,
  output_samples = 500,
  adapt_engaged=F,
  eta=0.1,
  iter = 100, #should be 1000 or more - 100 is just to run quickly
  init=1,
  tol_rel_obj = 0.001
)

tar_load(data)
tar_load(data_training)
tar_load(model_results)




bayesplot_grid(
  mcmc_hist(fit_vb$draws(c("lambda_beta","gamma_beta")), binwidth = 0.025),
  titles = c("Approximate posterior from VB"),
  xlim = c(-3, 3)
)
