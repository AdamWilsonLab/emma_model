
#' @author Brian Maitner
#' @description This function releases the target object model_output as a parquet file
#' @param temp_directory_output Where the parquet files should be temporarily stored
#' @param output_tag The tag to use for the release
#' @param chunk_size The number of rows to include in each parquet chunk.  If left NULL, will use all (up to 250 million)
#' @param sleep_time Positive numeric.  Number of seconds to wait between each Github or piggyback query
release_training_cellIDs <- function(envdata,
                                     temp_directory_output = "data/cellIDs",
                                     output_tag = "cellIDs",
                                     chunk_size = NULL,
                                     sleep_time = 1){

  # make folder if needed

  if(!dir.exists(temp_directory_output)){
    dir.create(temp_directory_output, recursive = TRUE)
    }

  # clear out any accidental remnants

  file.remove(list.files(temp_directory_output,full.names = TRUE))

  # Make sure there is a release or else create one.

  pb_assests <- pb_list(repo = "AdamWilsonLab/emma_model")

  if(!output_tag %in% pb_assests$tag){

    tryCatch(expr =   pb_new_release(repo = "AdamWilsonLab/emma_model",
                                     tag =  output_tag),
             error = function(e){message("Previous release found")})

    Sys.sleep(sleep_time) #We need to limit our rate in order to keep Github happy

  }

  # Create arrow datasets

  envdata %>%
    dplyr::filter(sample == 1) %>%
    dplyr::select(cellID) %>%
  write_parquet(sink = file.path(temp_directory_output,
                                 paste(output_tag,
                                       ".gz.parquet", sep = "")),
                compression = "gzip",
                chunk_size = chunk_size)

  # Upload ith file

  pb_upload(file = file.path(temp_directory_output,
                             paste(output_tag,
                                   ".gz.parquet", sep = "")),
            repo = "AdamWilsonLab/emma_model",
            tag = output_tag,
            overwrite = TRUE
  )

  #Clear out the temporary directory

  unlink(temp_directory_output, recursive = TRUE, force = TRUE)

  #End
  return(invisible(NULL))



}
