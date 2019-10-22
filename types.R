library(dplyr)

setClass(
  "modelParams",
  slots = c(
    'turnout_df',
    'election_fe',
    'precinct_fe',
    'svd',
    'precinct_cov',
    'precinct_cov_inv'
  )
)

modelParams <- function(
  turnout_df,
  election_fe,
  precinct_fe,
  svd,
  precinct_cov,
  precinct_cov_inv
){
  new(
    "modelParams",
    turnout_df=turnout_df,
    election_fe=election_fe,
    precinct_fe=precinct_fe,
    svd=svd,
    precinct_cov=precinct_cov,
    precinct_cov_inv=precinct_cov_inv
  )
}

validate_turnout_df <- function(df){
  required_columns <- c("precinct", "election", "turnout")
  if(!all(required_columns %in% names(df))) stop(
    "turnout_df must have columns %s.", 
    paste(required_columns, collapse = ", ")
  )
  
  precincts <- unique(df$precinct)
  elections <- unique(df$election)
  
  all_rows <- expand.grid(precinct=precincts, election=elections)
  missing_rows <- anti_join(all_rows, df)
  if(nrow(missing_rows) > 0) stop(sprintf("df is missing rows %s", missing_rows))
  
  dup_rows <- df %>% select(precinct, election) %>% filter(duplicated(.))
  if(nrow(dup_rows) > 0) stop(sprintf("df has duplicated rows for %s", dup_rows))
  
  return(TRUE)
  ## validate that df has observations for each election x precinct
}

validate_precinct_sf <- function(sf, params){
  ## validate that precincts match

    return(TRUE)
}

extend_config <- function(config){
  config$election_day <- ymd(config$election_ds, tz=config$timezone)
  config$base_time <- config$election_day + hours(config$start_hour)
  config$n_minutes <- (config$end_hour - config$start_hour) * 60
  config$minutes_seq <- 1:config$n_minutes
  return(config)
}

validate_config <- function(config){
  # config_fields <-c(
  #   'city',
  #   'timezone',
  #   'election_ds',
  #   'start_hour',
  #   'end_hour',
  #   'precinct_shp_path',
  #   'precinct_id_col',
  #   'turnout_df_path'
  # )
  # if(!all(config_fields %in% names(config))) {
  #   stop("config missing %s", config_fields[which(!config_fields %in% names(config))])
  # }
    
  return(TRUE)
}


