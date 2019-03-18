
asnum <- function(x) as.numeric(as.character(x))

pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))

create_backup_dir <- function(dir){
  if(!dir.exists(paste0(dir,"/backup"))) dir.create(paste0(dir,"/backup"))
}

strip_leading_zero <- function(x) gsub("^\\s+","",x)

save_with_backup <- function(..., stem, dir){
  create_backup_dir(dir)
  
  timestamp <- gsub(':|-|\\s','_',Sys.time())
  save(
    ...,
    file = paste0(dir, '/backup/', stem, '_', timestamp,'.Rda')
  )
  
  save(
    ...,
    file = paste0(dir, '/', stem, '.Rda')
  )
}

ggsave_with_backup <- function(
  plot, 
  filestem,
  plottype="png",
  dir='.',
  ...
){
  create_backup_dir(dir)
  timestamp <- gsub(':|-|\\s','_',Sys.time())
  ggsave(
    plot, 
    filename = paste0(dir, '/backup/', filestem, "_", timestamp,".", plottype),
    ...
  )
  
  ggsave(
    plot, 
    filename = paste0(dir, '/', filestem, ".", plottype),
    ...
  )
}


safe_load <- function(file){
  e <- new.env()
  load(file, envir = e)
  if(length(e) == 1) return(e[[names(e)]]) else return(as.list(e))
}
