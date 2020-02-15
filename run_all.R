
ELECTION <- "phila_201911"

TRACKER_FOLDER <- "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/"
CONFIG_FOLDER <- sprintf("%s/elections/%s", TRACKER_FOLDER, ELECTION)

setwd(TRACKER_FOLDER)

add_election_path <- function(file){
  sprintf("%s/%s", CONFIG_FOLDER, file)
}

OUTPUT_DIR <- add_election_path("outputs")
output_file <- function(file) sprintf("%s/%s", OUTPUT_DIR, file)

source(add_election_path("config.R"))
source('load_data.R')
source("util_tracker.R")
source("download_google_sheet.R")
source("fit_submissions.R")
source("bootstrap.R")
source("generate_plots.R")
source("tweets.R")

library(rmarkdown)
library(tidyverse)

#######################
## BEFORE ELECTION DAY
#######################

if(FALSE){
  source("prep_shapefiles.R")
  prep_shapefile(
    add_election_path(config$precinct_shp_path),
    config$get_precinct_id,
    config$get_ward_from_precinct,
    save_dir = add_election_path("data")
  )
  
  precincts <- readRDS(add_election_path("data/precincts.Rds"))
  
  ## Precompute the historic parameters
  source("precalc_params.R")
  
  df <- read_csv(add_election_path(config$turnout_df_path), col_types = "ccd")
  
  params <- calc_params(
    turnout_df=df,
    n_svd=3
  )
  diagnostics(params, precincts, config)
  
  save_with_backup(params, stem="params", dir=OUTPUT_DIR)
}

#####################
## ON ELECTION DAY
#####################

params <- readRDS(output_file("params.Rds"))

if(FALSE){
  USE_GOOGLE_DATA <- TRUE
  IS_TEST <- TRUE
  IS_TEST <- FALSE
}

if(
  !exists("USE_GOOGLE_DATA") |
  !exists("IS_TEST")
) stop("must specify values first!")

SHOULD_TWEET <- TRUE
last_tweet <- Tweet(id = 1191713446808686598, time='NA')

run_iter <- 0
while(TRUE){
  run_iter <- run_iter + 1
  print(paste("Run", run_iter))
  print(Sys.time())
  
  if(USE_GOOGLE_DATA){
    print("download_google_sheet")
    download_google_sheet(config, test_data=IS_TEST, save_dir=OUTPUT_DIR)
  }
  
  print("load_data")
  data_load <- load_data(
    USE_GOOGLE_DATA, 
    config, 
    params, 
    google_rds=output_file("google_download.Rds")
  )
  raw_data <- data_load$raw_data
  saveRDS(raw_data, file=output_file("raw_data.Rds"))
  write.csv(
    raw_data, 
    file=output_file("raw_data.csv"), 
    row.names=FALSE
  )

  print("fit bootstrap")
  bs <- fit_and_bootstrap(
    raw_data=raw_data,
    params=params,
    election_config=config,
    n_boot=40,
    use_inverse=FALSE,
    verbose=TRUE
  )
  save_with_backup(bs, stem="bootstrap", dir=OUTPUT_DIR)
  
  if(IS_TEST){
    filename <- "turnout_tracker_%s_test.html"
  } else {
    filename <- "turnout_tracker_%s.html"
  }
  
  print("rmarkdown")
  rmarkdown::render( 
    "election_tracker.Rmd", 
    knit_root_dir = getwd(),
    output_dir = OUTPUT_DIR,
    output_file = sprintf(filename, config$city_filename)
  )
  
  print("copy and git")
  for(f0 in c(filename, "precinct_turnout_%s.csv", "raw_data_%s.csv")){
    f <- sprintf(f0, config$city_filename)
    file.copy(
      output_file(f),
      paste0("C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/", f),
      overwrite=TRUE
    )
  }

  system("upload_git.bat")

  if(SHOULD_TWEET){
    ## current_time et al are defined in election_tracker.Rmd
    tweet_if_time(
      last_tweet, 
      current_time,
      turnout_ci,
      config,
      plot_files=c(turnout_plot_file, relative_map_file)
    )
  }
}


