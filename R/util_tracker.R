source("util.R")

save_with_backup <- function(x, stem, dir="outputs"){
  filename <- sprintf("%s/%s.Rds", dir, stem)
  
  backup_dir <- sprintf("%s/backup", dir)
  if(!dir.exists(backup_dir)) dir.create(backup_dir)
  
  filename_backup <- sprintf(
    "%s/%s_%s.Rds",
    backup_dir,
    stem,
    Sys.time() %>% format("%Y_%m_%d_%I_%M_%S")
  )
  saveRDS(x, file=filename)
  saveRDS(x, file=filename_backup)
}

strip_leading_zero <- function(x) gsub("^(0|\\s)+","", x)
