#' @description Code to
#' @param predicted_data Output from the `function predict_from_model.R`

make_prediction_rasters <- function(predicted_data,
                                    out_folder = "data/temp/prediction_rasters"){
  
  robust_pb_download(file = "template.tif",
                     dest = "data",
                     repo = "AdamWilsonLab/emma_envdata",
                     tag = "processed_static")
  
  template <- terra::rast("data/template.tif")
  
  values(template) <- NA
  
  predicted_dates <- predicted_data$date %>%
    unique() %>%
    sort()
  
  if(!dir.exists(temp_folder)){dir.create(temp_folder,recursive = TRUE)}

  for(i in 1:length(predicted_dates)){
    
    
    data_i <-
    predicted_data %>%
      dplyr::filter(date == predicted_dates[i])
  
    rast_i <- template
    rast_i[data_i$cellID] <- data_i$mu  
    
    terra::writeRaster(rast_i,filename = file.path(temp_folder,paste(predicted_dates[i],"prediction.tif",sep = "_")),
                       overwrite=TRUE)
    
    
  }
  

}