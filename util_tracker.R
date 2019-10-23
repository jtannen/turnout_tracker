source("util.R")

save_with_backup <- function(x, stem, dir="outputs"){
  filename <- sprintf("%s/%s.Rds", dir, stem)
  filename_backup <- sprintf(
    "%s/backup/%s_%s.Rds",
    dir,
    stem,
    Sys.time() %>% format("%Y_%m_%d_%I_%M_%S")
  )
  saveRDS(x, file=filename)
  saveRDS(x, file=filename_backup)
}

strip_leading_zero <- function(x) gsub("^(0|\\s)+","", x)
