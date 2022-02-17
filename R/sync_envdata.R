

sync_envdata <- function(repo="AdamWilsonLab/emma_envdata",
                         path="data/envdata"){
  if(!file.exists(path)) dir.create(path,recursive = T)
  piggyback::pb_download(repo = repo,
                         dest = path,overwrite = T,
                         show_progress = F)
  file.remove(file.path(path,"stable_data.gz.parquet"))
  piggyback::pb_download(file="stable_data.gz.parquet",
                         repo = repo,
                         dest = path,overwrite = T,
                         show_progress = F)

    return(path)
}
