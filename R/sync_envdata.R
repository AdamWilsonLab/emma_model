

sync_envdata <- function(repo="AdamWilsonLab/emma_envdata",
                         path="data/envdata"){
  if(!file.exists(path)) dir.create(path,recursive = T)
  piggyback::pb_download(repo = repo,
                         dest = path,overwrite = T)
  return(path)
}
