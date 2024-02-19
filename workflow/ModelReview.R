## explorations of model output - not meant to be part of workflow
## Adam Wilson


#library(invgamma)
tar_load(stan_data)
tar_load(envdata)
#sdata=envdata

library(cmdstanr)
cpp_options = list(stan_threads = TRUE,stan_opencl = FALSE)
mod <- cmdstan_model("postfire_constrained.stan",compile = T,cpp_options=cpp_options)

modr <- mod$pathfinder(
  data = stan_data,
  init = 0.5,
  tol_rel_obj = 0.01,
  num_paths=4,
  single_path_draws=40,
  draws = 50,
  history_size=50,
  max_lbfgs_iters=1000,
  num_threads=8
)

#model_summary<-summarize_model_output(modr$summary(),stan_data, envdata)

mods<- modr$summary(variables = c("gamma_mu","gamma",
                                  "lambda","lambda_mu",
                                  "alpha","A","A_mu",
                                  "gamma_beta","lambda_beta","A_beta",
                                  "tau", "gamma_tau", "lambda_tau",
                                  "alpha_tau", "A_tau"))

model_summary <- mods %>%
  mutate(uid=gsub("[]]","",gsub(".*[[]","",variable)),
         parameter=gsub("[[].*","",variable),
         type=case_when(
           grepl("beta",parameter) ~ "beta",
           grepl("alpha$|gamma$|lambda$",parameter) ~ "spatial",
           grepl("tau",parameter) ~ "variability",
           grepl("^mu$",parameter) ~ "state",
           grepl("y_obs",parameter) ~ "state",
           TRUE ~ "other"
         ))

model_summary %>%
  ggplot(aes(x=median))+geom_density()+facet_wrap(~parameter,scales = "free")

#betas
model_summary %>%
  filter(type=="beta") %>%
  dplyr::select(uid,parameter,median) %>%
  ggplot(aes(x=median,y=uid))+geom_point()+facet_wrap(~parameter,scales = "free")


#taus
model_summary %>%
  filter(type=="variability") %>%
  dplyr::select(uid,parameter,median) %>%
  ggplot(aes(x=median,y=uid))+geom_point()


model_summary %>%
  filter(type=="spatial") %>%
  dplyr::select(uid,parameter,median) %>%
  spread(key=parameter,value=median) %>%
  ggplot(aes(y=lambda,x=gamma))+geom_point()

model_summary %>%
#  filter(parameter%in%c("lambda","lambda_mu")) %>% #"gamma","gamma_mu",
#  filter(parameter%in%c("gamma","gamma_mu")) %>%
  dplyr::select(uid,parameter,median) %>%
  spread(key=parameter,value=median) %>%
#  ggplot(aes(y=exp(A_mu),x=A))+geom_point()+geom_abline()
  ggplot(aes(y=exp(gamma_mu),x=gamma))+geom_point()+geom_abline()
#  ggplot(aes(y=exp(lambda_mu),x=lambda))+geom_point()+geom_abline()


mody<- modr$summary(variables = c("mu","y_pred")) #

mody2 <- bind_cols(
  mu_median=pull(filter(mody,grepl("mu",mody$variable)),median),
  ypred_median=pull(filter(mody,grepl("y_pred",variable)),median),
  ypred_q5=pull(filter(mody,grepl("y_pred",variable)),q5),
  ypred_q95=pull(filter(mody,grepl("y_pred",variable)),q95),
  y_obs=stan_data$y_obs,age=stan_data$age,pid=stan_data$pid)

mody2 %>%
  filter(pid%in%sample(mody_mu$pid,20)) %>%
  ggplot(aes(x=age))+
  geom_line(aes(y=y_obs),col="red")+
#  geom_ribbon(aes(ymin=ypred_q5,ymax=ypred_q95),col="blue")+
  geom_line(aes(y=ypred_median),col="blue")+
  geom_line(aes(y=mu_median))+
  facet_wrap(~pid)

 mody2 %>%
#  filter(pid%in%sample(model_prediction$pid,20)) %>%
  ggplot(aes(x=y_obs,y=ypred_median))+
  geom_hex(bins=30)+
  scale_fill_viridis_c()+
  geom_abline(col="red")
#  facet_wrap(~pid)


tar_load(model_draws_postfire_season2)


ms <- model_draws_postfire_season2 %>%
  slice(1:100) %>%
  gather(key = var_name, value = value, 2:ncol(df)) %>%
  spread_(key = names(df)[1],value = 'value')
  t() %>%
  tdata<- model_summary %>%
  mutate(uid=gsub("[]]","",gsub(".*[[]","",variable)),
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
