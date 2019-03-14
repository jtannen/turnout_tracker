setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_day_tracker/tracker_v0/phila_example/")

source("config.R")
source("../util.R")
# source("download_google_sheet.R", chdir = TRUE)
source("../fit_submissions.R", chdir = TRUE)
source("../bootstrap.R", chdir = TRUE)
source("../download_google_sheet.R", chdir=TRUE)

library(rmarkdown)

# #######################
# ## BEFORE ELECTION DAY
# #######################
# source("../prep_shapefiles.R", chdir = TRUE)
# prep_shapefile(
#   config$precinct_shp_path,
#   config$precinct_id_col,
#   config$get_ward_from_precinct
# )
# 
# ## Precompute the historic parameters
# source("../calc_params.R", chdir=TRUE)
# 
# df <- read.csv(config$turnout_df_path)
# df$precinct <- sprintf("%04d", df$precinct)
# 
# precincts <- safe_load("data/precincts.Rda")
# wards <- safe_load("data/wards.Rda")
# 
# params <- calc_params(
#   turnout_df=df,
#   n_svd=3
# )
# diagnostics(params, precincts)
# 
# save_with_backup(params, stem="params", dir="outputs")

#####################
## ON ELECTION DAY
#####################

params <- safe_load("outputs/params.Rda")

run_iter <- 0
while(TRUE){
  run_iter <- run_iter + 1
  print(paste("Run", run_iter))
  print(Sys.time())
  
  raw_data <- download_google_sheet(config)
  raw_data <- load_data(TRUE, config, params)$raw_data
  
  bs <- fit_bootstrap(
    raw_data,
    params,
    election_config=config,
    n_boot=100,
    use_inverse=FALSE,
    verbose=FALSE
  )
  save_with_backup(bs, stem="bootstrap", dir="outputs")
  
  rmarkdown::render( 
    "../election_tracker.Rmd", 
    knit_root_dir = getwd(),
    output_dir = "outputs",
    output_file = "election_tracker.html"
  )
  
  file.copy(
    "outputs/election_tracker.html",
    sprintf(
      "C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/election_tracker_%s.html",
      config$city_filename
    ),
    overwrite=TRUE
  )
  
  system("upload_git.bat")
}
