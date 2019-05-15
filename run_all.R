## SET THE WORKING DIRECTORY FIRST
## TO THE ELECTION-SPECIFIC FOLDER

source("config.R")
source("../util.R")
source("../download_google_sheet.R", chdir = TRUE)
source("../fit_submissions.R", chdir = TRUE)
source("../bootstrap.R", chdir = TRUE)
source("../generate_plots.R", chdir=TRUE)

library(rmarkdown)
library(tidyverse)

# #######################
# ## BEFORE ELECTION DAY
# #######################
# source("../prep_shapefiles.R", chdir = TRUE)
# prep_shapefile(
#   config$precinct_shp_path,
#   config$get_precinct_id,
#   config$get_ward_from_precinct
# )
# 
# ## Precompute the historic parameters
# source("../calc_params.R", chdir=TRUE)
# 
# df <- read_csv(config$turnout_df_path, col_types = "ccd")
# 
# precincts <- safe_load("data/precincts.Rda")
# wards <- safe_load("data/wards.Rda")
# 
# params <- calc_params(
#   turnout_df=df,
#   n_svd=3
# )
# diagnostics(params, precincts, config)
# 
# save_with_backup(params, stem="params", dir="outputs")

#####################
## ON ELECTION DAY
#####################

params <- safe_load("outputs/params.Rda")

if(
  !exists("use_real_data") |
  !exists("is_test")
) stop("must specify values first!")


run_iter <- 0
while(TRUE){
  run_iter <- run_iter + 1
  print(paste("Run", run_iter))
  print(Sys.time())
  
  print("download_google_sheet")
  if(use_real_data) raw_data <- download_google_sheet(config)
  
  print("load_data")
  data_load <- load_data(use_real_data, config, params)
  raw_data <- data_load$raw_data
  
  print("save_with_backup")
  save(raw_data, file=paste0("outputs/raw_data.Rda"))
  write.csv(raw_data, file="outputs/raw_data.csv")
  if(!use_real_data) print(data_load$fake_data$true_turnout)

  print("fit_bootstrap")
  bs <- fit_bootstrap(
    raw_data,
    params,
    election_config=config,
    n_boot=40,
    use_inverse=FALSE,
    verbose=FALSE
  )
  save_with_backup(bs, stem="bootstrap", dir="outputs")
  
  if(is_test){
    filename <- sprintf("turnout_tracker_%s_test.html", config$city_filename)
  } else {
    filename <- sprintf("turnout_tracker_%s.html", config$city_filename)
  }
  
  print("rmarkdown")
  rmarkdown::render( 
    "../election_tracker.Rmd", 
    knit_root_dir = getwd(),
    output_dir = "outputs",
    output_file = filename
  )
  
  print("copy and git")
  for(f in c(filename, "precinct_turnout.csv", "raw_data.csv")){
    file.copy(
      paste0("outputs/", f),
      paste0("C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/", f),
      overwrite=TRUE
    )
  }
  
  system("../upload_git.bat")
}
