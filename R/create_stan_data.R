#prep data for stan
create_stan_data <- function(data,dyndata,fit=1,predict=0){

    pid_lookup= data %>%
    dplyr::select(cellID, pid)

  dyndata2= dyndata %>%
    left_join(pid_lookup) %>%
    filter(!is.na(pid),!is.na(ndvi),!is.na(age)) %>%
    arrange(pid,date)

  xvar=dplyr::select(data,-cellID, -pid)

  stan_data =  list(
    N = nrow(dyndata2),
    J= nrow(data),
    ndvi=dyndata2$ndvi,
    age= dyndata2$age,
    pid=dyndata2$pid,
    x = xvar,
    P = ncol(xvar),
    fit=fit,
    predict=predict)
}

