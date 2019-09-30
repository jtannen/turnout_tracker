## SET THE WORKING DIRECTORY FIRST
## TO THE ELECTION-SPECIFIC FOLDER

source("config.R")
source("../util_tracker.R", chdir=TRUE)
source("../download_google_sheet.R", chdir = TRUE)
source("../fit_submissions.R", chdir = TRUE)
source("../bootstrap.R", chdir = TRUE)
source("../generate_plots.R", chdir=TRUE)
source("../tweets.R", chdir=TRUE)

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

SHOULD_TWEET <- TRUE
reply_tweet_id <- NA
time_of_last_tweet <- NA

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
  write.csv(raw_data, file=sprintf("outputs/raw_data_%s.csv", config$city_filename), row.names=FALSE)
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
    filename <- "turnout_tracker_%s_test.html"
  } else {
    filename <- "turnout_tracker_%s.html"
  }
  
  print("rmarkdown")
  rmarkdown::render( 
    "../election_tracker.Rmd", 
    knit_root_dir = getwd(),
    output_dir = "outputs",
    output_file = sprintf(filename, config$city_filename)
  )
  
  print("copy and git")
  for(f0 in c(filename, "precinct_turnout_%s.csv", "raw_data_%s.csv")){
    f <- sprintf(f0, config$city_filename)
    file.copy(
      paste0("outputs/", f),
      paste0("C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/", f),
      overwrite=TRUE
    )
  }

  system("../upload_git.bat")
  
  if(SHOULD_TWEET){
    is_time_to_tweet <- is.na(time_of_last_tweet) | (
      (ymd_hms(current_time) - ymd_hms(time_of_last_tweet)) >= hours(1)
    )

    if(is_time_to_tweet){
      reply_tweet_id <- tweet_update(
        reply_tweet_id, 
        turnout_ci,  
        current_time, 
        config, 
        c(turnout_plot_file, relative_map_file)
      )
      time_of_last_tweet <- current_time
    }
  }
}