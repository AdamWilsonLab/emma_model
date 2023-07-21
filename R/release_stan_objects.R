#' @destription Code to release model objects as rds
#' @author Brian S Maitner
#' @note Most targets objects can be loaded just fine as RDS files.  For some reason, the stan files are having issues.
#' So, I'm trying a work-around here.
#' @note Also, this is designed for targets objects that are tables/dataframes

release_stan_objects <- function(object_names = c(),
                                 temp_directory = "temp/objects",
                                 tag = "model_output",
                                 objects  = c("model_summary_postfire_season",
                                              "model_w_pred_summary_postfire_season_predict"),
                                 ...
                                 ){

  # create temp dir if needed
    if(!dir.exists(temp_directory)){
      dir.create(temp_directory,recursive = TRUE)
    }
  
  # check/create release
  
    assets <- pb_list(repo = "AdamWilsonLab/emma_model")
  
  if(!release %in% assets$tag){
    
    pb_new_release(repo = "AdamWilsonLab/emma_model",
                   tag = tag)
  }
  
  #temporarily save objects
  
    for(i in 1:length(objects)){
      
      tar_load(objects[i])
      
      write_parquet(x = get(objects[i]),
                    sink = file.path(temp_directory,
                                     paste(objects[i],".gz.parquet", sep = "")),
                    compression = "gzip")
      
      rm(list = objects[i])
      
      pb_upload(file = file.path(temp_directory,
                                 paste(objects[i],".gz.parquet", sep = "")),
                repo = "AdamWilsonLab/emma_model",
                tag = tag)
      
      file.remove(file.path(temp_directory,
                            paste(objects[i],".gz.parquet", sep = "")))
      
    }  

  unlink(x = temp_directory,recursive = TRUE,force = TRUE)
  
  return(Sys.Date())
  
  
}
