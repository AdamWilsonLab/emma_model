#' @destription Code to release model objects as rds
#' @author Brian S Maitner
#' @note Most targets objects can be loaded just fine as RDS files.  The ones genreated by stantargets are more problematic because they rely on alternative storage formats (which may also be verison-specific)
#' @note Also, this is designed for targets objects that are tables/dataframes and uploads both the model object and a parquet by default.

release_stan_objects <- function(object_names = c("model_summary_postfire_season",
                                                  "model_w_pred_summary_postfire_season_predict"),
                                 tag = "model_output",
                                 max_attempts = 10,
                                 sleep_time = 10,
                                 temp_directory="data/temp/pb_upload",
                                 parquet=TRUE,
                                 raw=TRUE,
                                 ...
                                 ){

  # create temp dir if needed

    if(!dir.exists(temp_directory)){
      dir.create(temp_directory,recursive = TRUE)
    }

  # check/create release

    assets <- pb_list(repo = "AdamWilsonLab/emma_model")

  if(!tag %in% assets$tag){

    caught<-tryCatch(pb_new_release(repo = "AdamWilsonLab/emma_model",
                   tag = tag),
             error = function(e) e)

    if(exists("caught")){rm(caught)}

  }

  #temporarily save objects

    for(i in 1:length(object_names)){

      if(parquet){

        tar_load(object_names[i])

        write_parquet(x = get(object_names[i]),
                      sink = file.path(temp_directory,
                                       paste(object_names[i],".gz.parquet", sep = "")),
                      compression = "gzip")

        rm(list = object_names[i])

        robust_pb_upload(file = file.path(temp_directory,
                                          paste(object_names[i],
                                                ".gz.parquet", sep = "")),
                  repo = "AdamWilsonLab/emma_model",
                  tag = tag,
                  max_attempts = max_attempts,
                  sleep_time = sleep_time,
                  temp_directory = temp_directory)

              file.remove(file.path(temp_directory,
                                    paste(object_names[i],".gz.parquet", sep = "")))

      }#if parquet = TRUE



      #Upload raw objects

      if(raw){
      robust_pb_upload(file = file.path("_targets/objects/",object_names[i]),
                       repo = "AdamWilsonLab/emma_model",
                       tag = tag,
                       max_attempts = max_attempts,
                       sleep_time = sleep_time,
                       temp_directory = temp_directory)

      }#if raw=TRUE



    }

  unlink(x = temp_directory,recursive = TRUE,force = TRUE)

  return(Sys.Date())


}
