#prep data for stan
create_stan_data_prediction <- function(data,soil_data,dyndata,fit=1,predict=0,default_fire_age = 40.001){

    # table to link cellID to pid
    pid_lookup= data %>%
    dplyr::select(cellID, pid)

    # subset dynamic data to just these cells
  dyndata2= dyndata %>%
    left_join(pid_lookup) %>%
    mutate(age = case_when(is.na(age) ~ default_fire_age,
                           !is.na(age) ~ age)) %>%
    # filter(!is.na(pid),!is.na(ndvi),!is.na(age)) %>%
    filter(!is.na(pid) & !is.na(age)) %>%
    arrange(pid,date)

  # Toss days where ALL the ndvi values are missing

    dyndata2 %>%
      group_by(date) %>%
      mutate(prop_na = sum(is.na(ndvi)) / n()) %>%
      ungroup() %>%
      filter(prop_na < 1)%>%
      dplyr::select(-prop_na) -> dyndata2


  # keep only x data for matrix multiplication in model
  xvar=dplyr::select(data,-cellID, -pid)

  # keep only s (soil) data for matrix multiplication in model
  svar = dplyr::select(soil_data,-cellID, -pid)

  # assemble the list for stan
  stan_data =  list(
    N = nrow(dyndata2),
    J= nrow(data),
    y_obs=dyndata2$ndvi, #select variable to model
    age= dyndata2$age,
    pid=dyndata2$pid,
    x = xvar,
    x_pid = data$pid,
    s = svar,
    P = ncol(xvar),
    Q = ncol(svar),
    fit=fit,
    predict=predict,
    #keeping the following for easier matching later - not needed for model
    y_date=as.numeric(dyndata2$date),
    y_cellID=dyndata2$cellID,
    x_cellID = data$cellID
  )
    }

