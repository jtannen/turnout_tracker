library(testthat)
library(dplyr)
library(readr)

test_elections <- tribble(
  ~folder, ~turnout,
  #"phila_201911", 306e3,
  "phila_201905", 243e3,
  "phila_201811", 553e3,
  "phila_201805", 170e3
)


TRACKER_DIR <- "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/"
olddir <- setwd(TRACKER_DIR)

source("load_data.R", chdir=TRUE)
source("fit_submissions.R", chdir=TRUE)
source("bootstrap.R", chdir=TRUE)
source("precalc_params.R", chdir=TRUE)

config_dir <- function(election){
  sprintf("%s/elections/%s", TRACKER_DIR, election)
}

set.seed(215)

results <- data.frame()

for(i in 1:nrow(test_elections)){
  folder <- test_elections$folder[i]
  true_turnout <- test_elections$turnout[i]
  
  print("#########################################")
  print(folder)
  print("#########################################")
  
  setwd(config_dir(folder))
  source("config.R")
  
  print("CALCULATING PARAMS")
  
  turnout_df <- read_csv(config$turnout_df_path, col_types = "ccd")

  precincts <- readRDS("data/precincts.Rds")
  wards <- readRDS("data/wards.Rds")
  
  params <- calc_params(
    turnout_df=turnout_df,
    n_svd=3
  )
  # diagnostics(params, precincts, config)

  print("FITTING DATA")
  
  google_rds <- "outputs/google_download.Rds"
  data_load <- load_data(TRUE, config, params, google_rds)
  raw_data <- data_load$raw_data
  bs <- fit_and_bootstrap(
    raw_data=raw_data,
    params=params,
    election_config=config,
    n_boot=40,
    use_inverse=FALSE,
    verbose=TRUE
  )
 
  ci <- get_ci_from_bs(
    bs, 
    predict_topline,
    keys=c("time_of_day"), 
    eod=TRUE
  )
  
  print("Boostrapped CIs") 
  print(round(ci[c("p025", "p975")]))
  print(sprintf("True Turnout: %s", true_turnout))
  
  expect_gt(ci$p975, true_turnout) 
  expect_lt(ci$p025, true_turnout) 
  
  results <- bind_rows(
    results,
    data.frame(
      election=folder, 
      true_turnout=true_turnout, 
      ci_median=ci$turnout,
      ci_low=ci$p025, 
      ci_high=ci$p975
    )
  )
}

print(results)
setwd(olddir)