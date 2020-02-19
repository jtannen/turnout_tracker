library(dplyr)

setClass(
  "modelParams",
  slots = c(
    turnout_df="data.frame",
    election_fe="data.frame",
    precinct_fe="data.frame",
    svd="list",
    precinct_cov="matrix",
    precinct_cov_inv="matrix",
    n_precincts="integer"
  )
)

setClass(
  "modelFit",
  slots = c(
    precinct_re_fit="numeric", 
    loess_fit="list",  #loess class doesn't seem to work, hack is wrap it in a list 
    Sigma_inv_plus_N="Matrix",
    sigma_noise="numeric",
    raw_data="data.frame",
    params="modelParams"
  )
)

setClass(
  "modelPredictions",
  slots=c(
      precinct_df="data.frame",
      time_df="data.frame",
      raw_data="data.frame"
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
    precinct_cov_inv=precinct_cov_inv,
    n_precincts=nrow(precinct_fe)
  )
}

modelFit <- function(
  precinct_re_fit, 
  loess_fit, 
  Sigma_inv_plus_N,
  sigma_noise,
  raw_data,
  params
){
  new(
    "modelFit",
    precinct_re_fit=precinct_re_fit, 
    loess_fit=list(loess_fit), 
    Sigma_inv_plus_N=Sigma_inv_plus_N,
    sigma_noise=sigma_noise,
    raw_data=raw_data,
    params=params
  )
}

winsorize <- function(x, t = 0.95){
  mean_x <- mean(x)
  x_demean <- x - mean_x
  cutoff <- quantile(abs(x_demean), probs=t, na.rm=TRUE)
  return(
    mean_x + sign(x_demean) * pmin(abs(x_demean), cutoff)
  )
}

validate_model_fit <- function(model_fit){
  ## Dumb for now. Can add checks on data quality.
  if(!is(model_fit, "modelFit")) stop("Must be class modelFit")
}

predict_obs <- function(model_fit){
  validate_model_fit(model_fit)
  
  obs_precinct_num <- model_fit@raw_data$precinct_num
  precinct_fit <- model_fit@precinct_re_fit
  loess_predicted <- predict_loess_obs(model_fit)

  return(
    precinct_fit[obs_precinct_num] + loess_predicted
  )  
}

predict_loess_obs <- function(model_fit, end_of_day=FALSE){
  validate_model_fit(model_fit)
  if(end_of_day){
    newdata <- data.frame(minute=max(model_fit@raw_data$minute))
    predict(model_fit@loess_fit[[1]], newdata=newdata)
  } else {
    predict(model_fit@loess_fit[[1]])
  }
}

calc_resid <- function(model_fit, winsorize=FALSE){
  validate_model_fit(model_fit)
  
  log_obs <- model_fit@raw_data$log_obs
  
  resid <- log_obs - predict_obs(model_fit)
  if(winsorize) resid <- winsorize(resid)
  
  return(resid)
}

print_resid <- function(model_fit){
  loess_predicted_eod <- 0
  sum_resid_sq <- sum(calc_resid(current_fit)^2)
}

modelPredictions <- function(
  precinct_df,
  time_df,
  raw_data
){
  new(
    "modelPredictions",
    precinct_df=precinct_df,
    time_df=time_df,
    raw_data=raw_data
  )
}

filter_to_eod <- function(df){
  df %>% 
    ungroup() %>% 
    filter(time_of_day == max(time_of_day))
}

predict_precinct_eod <- function(model_predictions){
  exp_time_fit_eod <- model_predictions@time_df %>%
    filter_to_eod() %>%
    with(exp(log_fit))
  
  model_predictions@precinct_df %>%
    mutate(turnout = exp(re_fit) * exp_time_fit_eod)
}

predict_topline <- function(model_predictions, eod=FALSE){
  sum_exp_divs <- sum(exp(model_predictions@precinct_df$re_fit))
  if(eod) { 
    time_df <- filter_to_eod(model_predictions@time_df)
  } else {
    time_df <- model_predictions@time_df
  }
  time_df %>% 
    mutate(turnout = exp(log_fit) * sum_exp_divs) %>%
    select(time_of_day, turnout)
}

predict_turnout <- function(model_predictions, precinct, time_of_day){
  data.frame(
    precinct = precinct,
    time_of_day = time_of_day
  ) %>%
    left_join(model_predictions@precinct_df) %>%
    left_join(model_predictions@time_df) %>%
    mutate(turnout = exp(log_fit + re_fit)) %>%
    select(precinct, time_of_day, turnout)
}

predict_ward_turnout <- function(
  model_predictions, 
  get_ward = function(x) substr(x, 1, 2)
){
  predict_precinct_eod(model_predictions) %>%
    mutate(ward = get_ward(precinct)) %>%
    group_by(ward) %>%
    summarise(
      turnout = sum(turnout),
      sum_fe = sum(exp(precinct_fe)),
      re_over_fe = sum(exp(re_fit)) / sum(exp(precinct_fe))
    ) %>% ungroup
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


