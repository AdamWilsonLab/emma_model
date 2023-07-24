#' @destription Code to release model objects as rds
#' @author Brian S Maitner
#' @note Most targets objects can be loaded just fine as RDS files.  For some reason, the stan files are having issues.
#' So, I'm trying a work-around here.
#' @note: this issue is because stantargets uses other options than rds for saving.
#' @note Also, this is designed for targets objects that are tables/dataframes

release_stan_objects <- function(object_names = c("model_summary_postfire_season",
                                                  "model_w_pred_summary_postfire_season_predict"),
                                 temp_directory = "temp/objects",
                                 tag = "model_output",
                                 max_attempts = 10,
                                 sleep_time = 10,
                                 temp_directory="data/temp/pb_upload"
                                 ...
                                 ){

  # create temp dir if needed

    if(!dir.exists(temp_directory)){
      dir.create(temp_directory,recursive = TRUE)
    }

  # check/create release

    assets <- pb_list(repo = "AdamWilsonLab/emma_model")

  if(!tag %in% assets$tag){

    pb_new_release(repo = "AdamWilsonLab/emma_model",
                   tag = tag)
  }

  #temporarily save objects

    for(i in 1:length(object_names)){

      # tar_load(object_names[i])
      #
      # write_parquet(x = get(object_names[i]),
      #               sink = file.path(temp_directory,
      #                                paste(object_names[i],".gz.parquet", sep = "")),
      #               compression = "gzip")
      #
      # rm(list = object_names[i])
      #
      # robust_pb_upload(file = file.path(temp_directory,
      #                                   paste(object_names[i],
      #                                         ".gz.parquet", sep = "")),
      #           repo = "AdamWilsonLab/emma_model",
      #           tag = tag,
      #           max_attempts = max_attempts,
      #           sleep_time = sleep_time,
      #           temp_directory = temp_directory)
#
#       file.remove(file.path(temp_directory,
#                             paste(object_names[i],".gz.parquet", sep = "")))


      #Upload raw objects
      robust_pb_upload(file = file.path("_targets/objects/",object_names[i]),
                       repo = "AdamWilsonLab/emma_model",
                       tag = tag,
                       max_attempts = max_attempts,
                       sleep_time = sleep_time,
                       temp_directory = temp_directory)



    }

  unlink(x = temp_directory,recursive = TRUE,force = TRUE)

  return(Sys.Date())


}
