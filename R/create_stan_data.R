#prep data for stan
create_stan_data <- function(data,dyndata,fit=1,predict=0){

    # table to link cellID to pid
    pid_lookup= data %>%
    dplyr::select(cellID, pid)

    # subset dynamic data to just these cells
  dyndata2= dyndata %>%
    left_join(pid_lookup) %>%
    filter(!is.na(pid),!is.na(ndvi),!is.na(age)) %>%
    arrange(pid,date)

  # keep only x data for matrix multiplication in model
  xvar=dplyr::select(data,-cellID, -pid)

  # assemble the list for stan
  stan_data =  list(
    N = nrow(dyndata2),
    J= nrow(data),
    ndvi=dyndata2$ndvi,
    age= dyndata2$age,
    pid=dyndata2$pid,
    date=as.numeric(dyndata2$date),
    x = xvar,
    P = ncol(xvar),
    fit=fit,
    predict=predict)
}

