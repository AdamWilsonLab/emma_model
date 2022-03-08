

sync_envdata <- function(repo="AdamWilsonLab/emma_envdata",
                         path="data/envdata",delete_all_first=F){
if(delete_all_first)  file.remove(list.files(path,full=T)) # delete all old files

  #file.path(path,"stable_data.gz.parquet"))
  if(!file.exists(path)) dir.create(path,recursive = T)
  piggyback::pb_download(repo = repo,
                         tag = "current",
                         dest = path,overwrite = T,
                         show_progress = F)
  piggyback::pb_download(file="stable_data.gz.parquet",
                         repo = repo,
                         tag = "current",
                         dest = path,overwrite = T,
                         show_progress = F)

    return(path)
}
