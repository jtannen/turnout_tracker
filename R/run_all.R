ELECTION <- "phila_202105"

source('R/load_data.R', chdir=TRUE)
source("R/util_tracker.R", chdir=TRUE)
source("R/download_google_sheet.R", chdir=TRUE)
source("R/fit_submissions.R", chdir=TRUE)
source("R/bootstrap.R", chdir=TRUE)
source("R/generate_plots.R", chdir=TRUE)
source("R/tweets.R", chdir=TRUE)
source("R/wordpress.R", chdir=TRUE)


CONFIG_FOLDER <- sprintf("election_configs/%s", ELECTION)

add_election_path <- function(file){
  sprintf("%s/%s", CONFIG_FOLDER, file)
}

OUTPUT_DIR <- add_election_path("outputs")
output_file <- function(file) sprintf("%s/%s", OUTPUT_DIR, file)
  

if(FALSE){
  USE_GOOGLE_DATA <- TRUE
  IS_TEST <- TRUE
  IS_TEST <- FALSE
} else {
  USE_GOOGLE_DATA <- TRUE
  IS_TEST <- TRUE
}


CONFIG <- source(add_election_path("config.R"), local=new.env())$value


#######################
## BEFORE ELECTION DAY
#######################


if(FALSE){
  source("R/prep_shapefiles.R", chdir=TRUE)
  prep_shapefile(
    shp_path=add_election_path(CONFIG$precinct_shp_path),
    get_precinct_id=CONFIG$get_precinct_id,
    get_ward_from_precinct=CONFIG$get_ward_from_precinct,
    save_dir = add_election_path("data")
  )
  
  precincts <- readRDS(add_election_path("data/precincts.Rds"))
  
  ## Precompute the historic parameters
  source("R/precalc_params.R", chdir=TRUE)
  
  df_major <- readRDS("../../../data/processed_data/df_major_20210118.Rds") %>%
    mutate(warddiv = paste0(substr(warddiv, 1, 2), substr(warddiv, 4, 5)))
  turnout_df <- prep_turnout_df(df_major)
  readr::write_csv(turnout_df, add_election_path(CONFIG$turnout_df_path))
  df <- readr::read_csv(add_election_path(CONFIG$turnout_df_path), col_types = "ccd")
  
  params <- calc_params(
    turnout_df=df,
    n_svd=3
  )
  diagnostics(params, precincts, CONFIG)
  save_with_backup(params, stem="params", dir=OUTPUT_DIR)
  
  mail_in <- read.csv("../../election_night_2020/tmp/mailin_phila.csv")
  mail_in %<>% 
    mutate(
      ward=substr(warddiv, 1, 2)
    ) %>%
    group_by(ward) %>%
    summarise(
      ballots = sum(n_mailed),
      returned = sum(n_ret)
    )

  save_with_backup(mail_in, stem="mail_in", dir=OUTPUT_DIR)
}

#####################
## ON ELECTION DAY
#####################

params <- readRDS(output_file("params.Rds"))

if(
  !exists("USE_GOOGLE_DATA") |
  !exists("IS_TEST")
) stop("must specify values first!")

SHOULD_TWEET <- FALSE
# last_tweet <- Tweet(id = 1191713446808686598, time='NA')

run_once <- function(){
  if(USE_GOOGLE_DATA){
    print("download_google_sheet")
    download_google_sheet(CONFIG, test_data=IS_TEST, save_dir=OUTPUT_DIR)
  }
  
  print("load_data")
  data_load <- load_data(
    USE_GOOGLE_DATA, 
    CONFIG, 
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
    election_config=CONFIG,
    n_boot=40,
    verbose=TRUE
  )
  save_with_backup(bs, stem="bootstrap", dir=OUTPUT_DIR)
  
  if(IS_TEST){
    filename <- "turnout_tracker_%s_test.html"
  } else {
    filename <- "turnout_tracker_%s.html"
  }
  html_file <- sprintf(filename, CONFIG$city_filename)
  
  print("rmarkdown")
  rmarkdown::render(
    "R/election_tracker.Rmd", 
    knit_root_dir = getwd(),
    output_dir = OUTPUT_DIR,
    output_file = html_file
  )
  
  print("Uploading Report")
  
  upload_resp <- upload_media(
    sprintf("%s/%s", OUTPUT_DIR, html_file),
    slug = "phila_turnout_tracker",
    verbose=TRUE,
    overwrite=TRUE, 
    allow_filename_change=FALSE
  )
  
  # for(f0 in c(filename, "precinct_turnout_%s.csv", "raw_data.csv")){
  #   f <- sprintf(f0, CONFIG$city_filename)
  #   file.copy(
  #     output_file(f),
  #     paste0("C:/Users/Jonathan Tannen/Dropbox/github_page/jtannen.github.io/", f),
  #     overwrite=TRUE
  #   )
  # }
  # 
  # system("upload_git.bat")
  
  
  if(SHOULD_TWEET){
    ## current_time et al are defined in election_tracker.Rmd
    tweet_if_time(
      last_tweet, 
      current_time,
      turnout_ci,
      CONFIG,
      plot_files=c(turnout_plot_file, relative_map_file)
    )
  }
}

run_iter <- 0
while(TRUE){
  run_iter <- run_iter + 1
  print(paste("Run", run_iter))
  print(Sys.time())
  
  run_once()
}


