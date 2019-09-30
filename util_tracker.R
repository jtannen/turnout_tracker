source("util.R")

save_with_backup <- function(x, stem, dir="outputs"){
  filename <- sprintf("%s/%s.Rda", dir, stem)
  filename_backup <- sprintf(
    "%s/backup/%s_%s.Rda",
    dir,
    stem,
    Sys.time() %>% format("%Y_%m_%d_%I_%M_%S")
  )
  save(x, file=filename)
  save(x, file=filename_backup)
}

strip_leading_zero <- function(x) gsub("^(0|\\s)+","", x)
