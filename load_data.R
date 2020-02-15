library(MASS)
library(lubridate)
source('types.R')


#######################
## SET ELECTION PARAMS
#######################

load_google_data <- function(
  election_config,
  rds="outputs/google_download.Rds"
){
  validate_config(election_config)
  config <- extend_config(election_config)
  
  raw_data <- readRDS(rds)
  raw_data$obs <- asnum(raw_data$obs)
  
  raw_data <- raw_data %>% filter(
    !is.na(obs) & 
      !is.na(time) & 
      !is.na(precinct)
  )
  
  # filter obviously unreasonable raw_data
  raw_data$obs[raw_data$obs == 0] <- 1
  raw_data <- raw_data %>% filter(obs > 0 & obs <= 900) 
  raw_data <- raw_data %>% filter(
    (time >= config$base_time) &
      (time <= (config$base_time + hours(config$end_hour - config$start_hour)))  
  )
  
  return(raw_data)
}

bimodal_cdf <- function(n_minutes){
  0.5 * (pnorm(1:n_minutes, 100, 50) + pnorm(1:n_minutes, 400, 50))
}

generate_fake_data <- function(
  params, 
  election_config,
  n_obs=500, 
  true_pattern=bimodal_cdf,
  frac_of_day=0.6,
  sigma_noise=0.05
){
  validate_config(election_config)
  config <- extend_config(election_config)
  
  election_fe_samp <- sample(params@election_fe$election_fe, 1)
  precinct_re_samp <- mvrnorm(
    1, 
    params@precinct_fe$precinct_fe, 
    params@precinct_cov
  )
  n_precincts <- length(precinct_re_samp)
  
  true_log_pattern <- log(true_pattern(config$n_minutes))
  
  fake_data <- data.frame(
    precinct_num = sample(n_precincts, size = n_obs, replace = TRUE),
    minute = sample(
      round(config$n_minutes * frac_of_day), 
      size = n_obs, 
      replace = TRUE
    ),
    noise = rnorm(n_obs, sd=sigma_noise)
  )
  
  fake_data$obs <- with(
    fake_data,
    exp(
      election_fe_samp + 
        precinct_re_samp[precinct_num] +
        noise +
        true_log_pattern[minute]
    ) %>% round()
  )  
  
  raw_data <- fake_data %>% 
    mutate(
      row_number = 1:n(),
      precinct = params@precinct_fe$precinct[precinct_num],
      time = as.character(config$base_time + minutes(minute))
    ) %>%
    dplyr::select(row_number, precinct, time, obs)
  
  ## valid final turnout since the pattern ends at 1
  true_turnout <- sum(exp(election_fe_samp + precinct_re_samp))
  
  # print turnout  
  curr_minutes <- max(fake_data$minute)
  frac <- exp(true_log_pattern[curr_minutes])
  print("True Turnout")
  print(true_turnout * frac)
  
  return(
    list(
      raw_data=raw_data, 
      election_fe_samp=election_fe_samp, 
      precinct_re_samp=precinct_re_samp,
      true_pattern=true_pattern,
      true_turnout=true_turnout
    )
  )
}


load_data <- function(
  use_google_data, 
  election_config, 
  params, 
  google_rds="outputs/google_download.Rds"
){
  validate_config(election_config)
  election_config <- extend_config(election_config)
  
  if(use_google_data){
    fake_data <- NULL
    raw_data <- load_google_data(election_config, google_rds)
  } else {
    fake_data <- generate_fake_data(params, election_config)
    raw_data <- fake_data$raw_data
  }  
  
  raw_data$obs[raw_data$obs == 0] <- 1
  raw_data$log_obs <- log(raw_data$obs)
  raw_data$precinct_num <- match(
    raw_data$precinct,
    params@precinct_fe$precinct
  )
  
  raw_data <- raw_data %>% filter(!is.na(precinct_num))
  
  raw_data$minute <- minutes_after_start(raw_data$time, election_config)
  raw_data$minute[raw_data$minute == 0] <- 1
  raw_data$time <- NULL
  
  return(list(fake_data=fake_data, raw_data=raw_data))
}

minutes_after_start <- function(time, config){
  dtime <- ymd_hms(time, tz=config$timezone) - config$base_time
  units(dtime) <- "mins"
  return(as.numeric(dtime))
}