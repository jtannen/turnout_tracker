library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(Matrix)
library(magrittr)
select <- dplyr::select

source("theme_sixtysix.R")
source('util.R')
source('types.R')

WINSORIZE_RESID <- TRUE

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

  config <- extend_config(election_config)
  
  dtime <- ymd_hms(raw_data$time, tz=election_config$timezone) - config$base_time
  units(dtime) <- "mins"
  raw_data$minute <- as.numeric(dtime)
  raw_data$minute[raw_data$minute == 0] <- 1
  raw_data$time <- NULL
  
  return(list(fake_data=fake_data, raw_data=raw_data))
}


#######################
## Analyze
#######################

tabulate_x <- function(
  precinct,
  x,
  n_p
){
  n_obs_p <- rep(0, n_p)
  x_sum_p <- rep(0, n_p)
  
  p_tab <- table(precinct)
  n_obs_p[asnum(names(p_tab))] <- p_tab
  
  sum_x <- tapply(INDEX=precinct, X=x, FUN=sum)
  x_sum_p[asnum(names(sum_x))] <- sum_x
  
  return(
    list(
      n_obs_p=n_obs_p, 
      x_sum_p=x_sum_p
    )
  )
}

optimize_precinct_re_of_model <- function(model_fit){
  precinct_num <- model_fit@raw_data$precinct_num
  resid <- calc_resid(model_fit, winsorize=WINSORIZE_RESID)
  precinct_re_fit <- model_fit@precinct_re_fit
  precinct_num <- model_fit@raw_data$precinct_num
  n_precincts <- model_fit@params@n_precincts
  precinct_fe <- model_fit@params@precinct_fe$precinct_fe
  precinct_cov_inv <- model_fit@params@precinct_cov_inv
  sigma_noise <- model_fit@sigma_noise
  first_mat_stored <- model_fit@first_mat_stored
  first_mat_is_inv <- model_fit@first_mat_is_inv
  
  x_tab <- tabulate_x(
    precinct_num,
    resid + precinct_re_fit[precinct_num],
    n_precincts
  )
  n_obs_p <- x_tab$n_obs_p 
  x_sum_p <- x_tab$x_sum_p
  
  model_fit@precinct_re_fit <- optimize_precinct_re(
    mu=precinct_fe,
    sigma_inv=precinct_cov_inv,
    n_obs=n_obs_p,
    x_sum=x_sum_p,
    sigma_noise=sigma_noise,
    first_mat=first_mat_stored,
    first_mat_is_inv=first_mat_is_inv
  )
  
  return(model_fit)
}

optimize_precinct_re <- function(
  mu,
  sigma_inv,
  n_obs,
  x_sum,
  sigma_noise,
  first_mat,
  first_mat_is_inv = FALSE
){
  mu_plus_x <- sigma_inv %*% mu + x_sum / sigma_noise^2
  if(first_mat_is_inv){
    new_mu <- first_mat %*% mu_plus_x
  }else{
    new_mu <- solve(first_mat, mu_plus_x)
  }
  return(as.vector(new_mu))
}

precomp_re_first_mat_inv <- function(
  n_obs_p,
  params,
  sigma_noise
){
  ## Calculates (Sigma^-1 + N/s^2)^-1
  Sigma <- params@precinct_cov
  
  U <- Matrix::sparseMatrix(
    i = which(n_obs_p > 0),
    j = 1:sum(n_obs_p > 0),
    x = sqrt(n_obs_p[n_obs_p > 0]) / sigma_noise,
    dims = c(nrow(Sigma), sum(n_obs_p > 0))
  )
  V <- t(U)
  
  first_mat_stored <- Sigma - (
    Sigma %*% U %*% 
      solve(
        Diagonal(n = sum(n_obs_p > 0)) +
          V %*% Sigma %*% U
      ) %*% 
    V %*% Sigma
  )
  
  return(first_mat_stored)
}

precomp_re_first_mat <- function(
  n_obs_p,
  params,
  sigma_noise
){
  ## Calculates (Sigma^-1 + N/s^2)
  first_mat_stored <- 
    Diagonal(
      length(n_obs_p), 
      n_obs_p / sigma_noise^2 
    ) + 
    params@precinct_cov_inv

  return(first_mat_stored)
}

get_loess_params <- function(n_obs){
  if(n_obs < 50){
    loess_degree <- 1
    loess_span <- 1
  }
  if(n_obs >= 50 & n_obs < 500){
    loess_degree <- 1
    loess_span <- 0.3
  }
  if(n_obs >= 500){
    loess_degree <- 2
    loess_span <- 0.3
  }
  return(list(loess_degree=loess_degree, loess_span=loess_span))
}

optimize_loess <- function(
  model_fit, 
  loess_span, 
  loess_degree
){
  resid <- calc_resid(model_fit, winsorize=WINSORIZE_RESID)
  loess_pred_old <- predict_loess_obs(model_fit)
  minute_obs <- model_fit@raw_data$minute
  
  model_fit@loess_fit[[1]] <- loess(
    x ~ minute, 
    data.frame(
      x=resid + loess_pred_old,
      minute=minute_obs
    ),
    span = loess_span,
    degree = loess_degree
  )

  return(model_fit)
}

update_resid <- function(mean_abs_resid, current_fit, verbose){
  mean_old <- mean_abs_resid
  mean_new <- mean(abs(calc_resid(current_fit)))
  if(verbose){
    print(sprintf("old Mean Abs Resid: %s", mean_old))
    print(sprintf("new Mean Abs Resid: %s", mean_new))
  }
  return(mean_new)
}


fit_em_model <- function(
  raw_data, 
  params,
  election_config,
  sigma_noise=0.1, 
  tol=1e-10,
  verbose=TRUE,
  use_inverse=TRUE
){
  
  ## TODO: should this use mgcv::gam?
  validate_config(election_config)
  config <- extend_config(election_config)

  n_obs <- nrow(raw_data)
  loess_params <- get_loess_params(n_obs)
  loess_degree <- loess_params$loess_degree
  loess_span <- loess_params$loess_span
  
  ## Precomp values
  x_tab <- tabulate_x(
    raw_data$precinct_num,
    rep(0, n_obs),
    params@n_precincts
  )
  n_obs_p <- x_tab$n_obs_p 
  x_sum_p <- x_tab$x_sum_p
  
  if(verbose) print("Precomputing Matrix")
  if(use_inverse){
    first_mat_stored <- precomp_re_first_mat_inv(
      n_obs_p, params, sigma_noise
    )
  }else{
    first_mat_stored <- precomp_re_first_mat(
      n_obs_p, params, sigma_noise
    )
  }

  if(verbose) print("Sampling")
  ## initialize  
  precinct_re_fit <- rep(0, params@n_precincts)
  log_pattern_fit <- rep(0, config$n_minutes)
  
  ## TODO generalize beyond loess, create "predict_time()" function instead
  dummy_loess <- loess(x ~ minute, data.frame(x=0, minute=raw_data$minute))
  
  current_fit <- modelFit(
    precinct_re_fit=precinct_re_fit,
    loess_fit=dummy_loess,
    first_mat_stored=first_mat_stored,
    sigma_noise=sigma_noise,
    first_mat_is_inv=use_inverse,
    raw_data=raw_data,
    params=params
  )

  loess_predicted_eod <- 0
  mean_abs_resid <- update_resid(0, current_fit, verbose=F)

  continue <- TRUE
  loop_no <- 0
  
  while(continue){
    if(verbose) print(paste("Iter ", loop_no))
    loop_no <- loop_no + 1
    
    current_fit %<>% optimize_precinct_re_of_model()
    mean_abs_resid %<>% update_resid(current_fit, verbose)
      
    current_fit %<>% optimize_loess(
      loess_span,
      loess_degree
    )    
    mean_abs_resid %<>% update_resid(current_fit, verbose)
    
    prior_predicted_eod <- loess_predicted_eod
    loess_predicted_eod <- predict_loess_obs(current_fit, end_of_day=TRUE)
    
    if(verbose){
      print(paste0("Final Turnout: ", loess_predicted_eod))
      print(paste0("Abs Change: ", abs(loess_predicted_eod - prior_predicted_eod)))
    }
    
    continue <- abs(loess_predicted_eod - prior_predicted_eod) > (tol * prior_predicted_eod)
  }
  print(paste('n_iter =', loop_no))
  
  return(current_fit)
}  

process_results <- function(
  model_fit,
  election_config,
  plots = TRUE, 
  save_results = FALSE,
  calc_ses = TRUE,
  fake_data = NULL,
  save_dir = ".",
  verbose=TRUE,
  pause=TRUE
){

  raw_data <- model_fit@raw_data
  params <- model_fit@params
  
  validate_config(election_config)
  config <- extend_config(election_config)
  
  loess_fit <- model_fit@loess_fit[[1]]
  precinct_re_fit <- model_fit@precinct_re_fit
  
  if(pause){
    pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))
  } else pause <- function() return()
  
  printv <- function(x) if(verbose) print(x)
  
  printv("predicting loess")
  log_pattern_predicted <- predict(
    loess_fit, 
    newdata = data.frame(minute=1:config$n_minutes), 
    se = calc_ses
  )
  
  if(calc_ses){
    log_pattern_fit <- log_pattern_predicted$fit
    log_pattern_fit_se <- log_pattern_predicted$se.fit
  }else{
    log_pattern_fit <- log_pattern_predicted
    log_pattern_fit_se <- NA
  }  

  
  ## Diagnostics
  if(!is.null(fake_data) & plots){
    printv("plots")
    time_plot <- ggplot(
      data.frame(
        minute = 1:config$n_minutes, 
        true_pattern = fake_data$true_turnout * 
          fake_data$true_pattern(config$n_minutes)[1:config$n_minutes],
        fitted_pattern = exp(log_pattern_fit) * sum(exp(precinct_re_fit))
      ),
      aes(x=minute, y=true_pattern)
    ) + 
      geom_line() +
      geom_point(aes(y=fitted_pattern)) +
      ggtitle("Fitted Time Series")
    printv(time_plot)
    pause()
    
    re_plot <- ggplot(
      data.frame(
        fitted_re = precinct_re_fit - with(params@precinct_fe, precinct_fe),
        true_re = fake_data$precinct_re_samp - with(params@precinct_fe, precinct_fe)
      ),
      aes(x = true_re, y=fitted_re)
    ) + 
      geom_point() +
      geom_abline(slope=1, intercept=0, color='red') +
      coord_fixed() +
      ggtitle("Precinct Random Effects")
    
    printv(re_plot)
    pause()    
  }

  if(plots){

    resid <- calc_resid(model_fit)
    
    gg_fitted <- ggplot(
      data.frame(
        fitted=raw_data$log_obs - resid,
        resid=resid,
        row = as.character(raw_data$row_number)
      ),
      aes(x=fitted, y=resid)
    ) + geom_text(aes(label = row)) +
      geom_hline(yintercept = 0) +
      ggtitle("Fitted Values vs Residuals")
    printv(gg_fitted)
    pause()

    gg_residual <- ggplot(
      data.frame(
        minute = raw_data$minute,
        resid= resid,
        row = as.character(raw_data$row_number)
      ),
      aes(x=minute, y=resid)
    ) + geom_text(aes(label = row)) +
      geom_hline(yintercept = 0)+
      ggtitle("Residual vs time")
    printv(gg_residual)
    pause()
    
  }

  printv("div_turnout")

  precinct_df <- params@precinct_fe %>%
    group_by() %>%
    mutate(
      re_fit = precinct_re_fit
    )

  printv("time_df")

  time_df <- data.frame(
    minute = 1:config$n_minutes,
    log_fit = log_pattern_fit
  ) %>%
    filter(!is.na(log_fit)) %>%
    mutate(
      time_of_day = config$base_time + minutes(minute)
    )

  printv("full_predictions")
  full_predictions <- expand.grid(
    precinct=params@precinct_fe$precinct,
    time_of_day=time_df$time_of_day
  ) %>%
    left_join(precinct_df, by="precinct") %>%
    left_join(time_df, by="time_of_day") %>%
    mutate(
      prediction = exp(re_fit + log_fit)
    )

  model_predictions <- modelPredictions(
    precinct_df=precinct_df,
    time_df=time_df,
    full_predictions=full_predictions
  )
  
  if(save_results){
    printv('saving predictions')
    save_with_backup(
      model_predictions,
      "model_predictions",
      save_dir=save_dir
    )

  }

  return(model_predictions)
}


if(FALSE){
  ## NO LONGER WORKS WITH NEW TYPES
  ## Run Once:
  test_fit <- fit_em_model(
    raw_data, 
    params,
    election_config
  )
  
  process_results(
    test_fit$precinct_re_fit,
    test_fit$loess_fit,
    test_fit$first_mat_stored,
    raw_data,
    test_fit$resid,
    params,
    fake_data=fake_data
  )
}
