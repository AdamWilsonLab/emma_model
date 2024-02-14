# Summarize posteriors
summarize_model_output <- function(model_summary,stan_data, data, ...){
  #posterior predictive

  beta_names<-data.frame(
    uid=as.character(1:ncol(stan_data$x)), #call it pid for easy joining with beta index below
    type="beta",
    xname=as.factor(colnames(stan_data$x)))

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

  #append pid from model. this is tricky because some of the model parameters use uid, other pid

    unique(tdata$parameter)

    tdata%>%
      group_by(parameter)%>%
      summarize(n=n())%>%
      ungroup()->t_summary

    #probably a smarter way to do this in tidyverse, but meh
    tdata$pid <- NA
    for(i in 1:nrow(t_summary)){

      var_i <-t_summary$parameter[i]

      if(t_summary$n[i] == stan_data$J){

        tdata$pid[which(tdata$parameter== t_summary$parameter[i])] <- stan_data$x_pid

      }


      if(t_summary$n[i] == stan_data$N){

        tdata$pid[which(tdata$parameter== t_summary$parameter[i])] <- stan_data$pid



      }


    }


  if(F){  tdata %>% group_by(parameter,type) %>% summarize(n())}

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

