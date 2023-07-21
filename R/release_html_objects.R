#' @destription Code to release model objects as rds
#' @author Brian S Maitner
#' @note Most targets objects can be loaded just fine as RDS files.  For some reason, the stan files are having issues.
#' So, I'm trying a work-around here.
#' @note Also, this is designed for targets objects that are tables/dataframes

release_html_objects <- function(file_names = c("index.html"),
                                 tag = "model_output",...
){

  # check/create release
  
  assets <- pb_list(repo = "AdamWilsonLab/emma_model")
  
  if(!release %in% assets$tag){
    
    pb_new_release(repo = "AdamWilsonLab/emma_model",
                   tag = tag)
  }

  #iterative uploads
  
  for(i in 1:length(file_names)){
    

    pb_upload(file = file_names[i],
              repo = "AdamWilsonLab/emma_model",
              tag = tag)
    
  }  
  
  return(Sys.Date())
   
  
}
