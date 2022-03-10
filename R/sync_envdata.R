

sync_envdata <- function(repo="AdamWilsonLab/emma_envdata",
                         path="data/envdata/",
                         delete_all_first=F)  {

  #if(delete_all_first)  file.remove(list.files(path,full=T)) # delete all old files

  # if running on github - delete the data files and get them again
  #if(Sys.getenv("CI")!="") file.remove(list.files(path,full=T)) # delete all old files

  if(!file.exists(path)) dir.create(path,recursive = T)


  # piggyback::pb_download(repo = repo,
  #                        tag = "current",
  #                        dest = path,
  #                        overwrite = F,
  #                        show_progress = F)
  #


    robust_pb_download(repo=repo,
                        tag = "current",
                        dest = path,
                      overwrite = F)


  #  tryCatch(
  #   piggyback::pb_download(file="stable_data.gz.parquet",
  #                        repo = repo,
  #                        tag = "current",
  #                        dest = path,
  #                        overwrite = T,
  #                        show_progress = F)
  # )
  #   Sys.sleep(60)
  #   }

    return(path)
}
