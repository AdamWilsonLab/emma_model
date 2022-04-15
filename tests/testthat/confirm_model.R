library(prioritizr)
library(cmdstanr)
library(tidyverse)


Sys.setenv(HOME="/home/rstudio")
cmdstanr::set_cmdstan_path()#"/home/rstudio/.cmdstanr/cmdstan-2.28.1")
# Define functions

make_landscape=function(nrow=10,ncol=10,nvar=3,model=RandomFields::RMgauss(scale=200),...){
  x=raster::raster(nrows=nrow,ncols=ncol,vals=1)
  env=stack(x,scale(simulate_data(x, n=nvar,model=model,...)))
  names(env)=c("intercept",paste0("env_",1:nvar))
  return(env)
}


make_static_data<-function(env, #landscape output from make_landscape()
                             alpha_beta,
                             gamma_beta,
                             lambda_beta,
                             phi_beta,
                             A_beta){

  envmatrix=as.matrix(env)

  return(
    tibble(
    cellID=1:nrow(envmatrix),
    x=coordinates(env)[,1],
    y=coordinates(env)[,2],
    alpha=as.vector(envmatrix%*%alpha_beta),
    gamma=as.vector(envmatrix%*%gamma_beta),
    lambda=as.vector(envmatrix%*%lambda_beta),
    A=as.vector(envmatrix%*%A_beta),
    phi=as.vector(envmatrix%*%phi_beta))
  )

}

make_recovery_data<-function(static, #landscape output from make_landscape()
                   ntime=100,  # number of time steps
                   maxage=30, # max age of pixels
                   nseasons=10 # number of 'seasonal' cycles in maxage
                   ){

  dyn=tibble(
    date=as.Date(30*1:ntime,origin=as.Date("2020-01-01")),
    age=seq(0,maxage,len=ntime),
    month=rep(seq(0,1,len=ntime/nseasons),nseasons)
  )

  res=expand_grid(static,dyn) %>%
    mutate(
     exp = alpha+gamma-gamma*exp(-(age/lambda)), #just the exponential part
     mu = exp+sin((phi+(2*pi*month)))*A, # add seasonality
     error = rnorm(n(),0,.1),
     obs = mu + error,  # add some noise
     obs = ifelse(obs>1,1,obs),
     obs = ifelse(obs>-1,obs,-1)
    )

return(res)

}


# Generate data

set.seed(999)
env=make_landscape()
plot(env)

# define the regression coefs
params=tibble(
  xname=names(env),
  alpha_beta=c(.1,.01,.02,.01),
  gamma_beta=c(.4,.15,.05,.1),
  lambda_beta=c(4,1,.5,.3),
  phi_beta=c(0, .8,-.2,.5),
  A_beta=c(.1, .05,-.07,.03)
  )

# generate the dynamic data
static=make_static_data(env=env,
                       alpha_beta=params$alpha_beta,
                       gamma_beta=params$gamma_beta,
                       lambda_beta=params$lambda_beta,
                       phi_beta=params$phi_beta,
                       A_beta=params$A_beta
                       )

# generate the dynamic data
dynamic=make_recovery_data(static)

# make the data for stan
stan_data =  list(
  N = nrow(dynamic),
  J= nrow(static),
  y_obs=dynamic$obs, #select variable to model
  age= dynamic$age,
  pid=dynamic$cellID,
  month=dynamic$month,
  x = as.matrix(env),
  x_pid = static$cellID,
  P = ncol(as.matrix(env)),
  fit=1,
  predict=1,
  #keeping the following for easier matching later - not needed for model
  y_cellID=dynamic$cellID,
  x_cellID = static$cellID,
  y_date = as.integer(dynamic$date)
)

# fit model
file="postfire_season2.stan"
mod <- cmdstan_model(file)
#mod$print()

fit_vb <- mod$variational(data = stan_data, seed = 123)

# summarize model
s1=fit_vb$summary()

s2=summarize_model_output(s1,stan_data, static)

s3=summarize_predictions(model_results = s2,stan_data = stan_data,envdata=static)

## compare betas
betas=s2 %>%
  filter(type=="beta") %>%
  left_join(gather(params,parameter,true,-xname))

cor(betas$true,betas$median)

ggplot(betas,aes(x=median,y=xname))+
  geom_point()+
  geom_segment(aes(x=q5,xend=q95,yend=xname))+
  geom_point(aes(x=true),col="red")+
  geom_vline(aes(xintercept=0),alpha=.3)+
  facet_wrap(~parameter,scales="free_x")

## compare spatial parameters
pmaps=s2 %>%
  filter(type=="spatial") %>%
  mutate(pid=as.integer(pid)) %>%
  dplyr::select(parameter,estimated=median,cellID=pid) %>%
  left_join(gather(static,parameter,true,-cellID,-x,-y)) %>%
  gather(type,value,-parameter,-x,-y,-cellID)



ggplot(pmaps,aes(x=x,y=y,fill=value))+
  geom_tile()+
  facet_grid(parameter~type)+
  scale_fill_viridis_c()



## compare predictions
pred <- dynamic %>%
  left_join(s3,by=c("cellID"="pid","date"="date"))


ggplot(pred,aes(x=as.integer(date)))+
  geom_line(aes(y=exp),col="red",alpha=.5)+
  geom_line(aes(y=mu),col="red",alpha=.5)+
  geom_line(aes(y=obs),col="red",alpha=.5)+
  geom_line(aes(y=median),col="black")+
  facet_wrap(~cellID)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# predicted vs observed
ggplot(pred,aes(x=obs,y=median))+
  geom_hex()+
  geom_abline(aes(intercept=0,slope=1),col="red")


# ggplot(res,aes(x=age))+
#   geom_line(aes(y=month))+
#   geom_line(aes(y=age))+
#   facet_wrap(~id)


res %>%
  dplyr::select(id,alpha,gamma,lambda,A,phi) %>%
  distinct() %>%
  gather(parameter,value,-id) %>%
  ggplot(aes(x=value,col=parameter))+
  geom_density()+
  facet_wrap(~parameter,scales = "free",ncol=1)
