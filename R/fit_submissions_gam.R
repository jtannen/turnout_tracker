library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(Matrix)
library(magrittr)
select <- dplyr::select

# setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/R")
# source("../election_configs/phila_202011/config.R")

source("theme_sixtysix.R")
source('util.R')
source('types.R')

WINSORIZE_RESID <- TRUE

#######################
## Analyze
#######################


fit_gam <- function(
  raw_data, 
  params,
  election_config,
  sigma_noise=0.1, 
  tol=1e-10,
  verbose=TRUE
){
  # validate_config(election_config)
  K <- ncol(params@svd$u)
  config <- extend_config(election_config)
  cov_svd <- with(params@svd, u %*% diag(d[1:K]) %*% cov(v) %*% diag(d[1:K]) %*% t(u))
  
  ## Hacky way to reverse engineer what the division noise was
  sigma_div <- diag(params@precinct_cov[1:6, 1:6] - cov_svd[1:6, 1:6])
  if(max(abs(sigma_div) - sigma_div[1]) > 1e-10) stop("Noise expected to be all equal.")
  
  hourly_knots <- ceiling(max(raw_data$minute) / 60)
  ud_div <- with(params@svd, u %*% diag(d[1:K]))[raw_data$precinct_num,]
  colnames(ud_div) <- paste0("UD.", 1:3)
  raw_data <- cbind(raw_data, as.data.frame(ud_div))
  raw_data$precinct_fe <- params@precinct_fe$precinct_fe[raw_data$precinct_num]
  
  fit <- mgcv::gam(
    log_obs ~ 
      s(minute, bs="cr", k=ceiling(hourly_knots/2)) +
      offset(precinct_fe) +
      UD.1 + UD.2 + UD.3 +
      s(precinct, bs="re")
    , 
    data=raw_data %>% mutate(precinct = factor(precinct))
  )
  list(
    raw_data=raw_data,
    params=params,
    fit=fit
  )
}  

calc_resid_gam <- function(gam_fit, winsorize=FALSE){
  pred <- predict(gam_fit$fit, newdata=gam_fit$raw_data)
  resid <- gam_fit$raw_data$log_obs - pred
  if(winsorize) resid <- winsorize(resid)
  return(resid)
}


process_results_gam <- function(
  gam_fit,
  election_config,
  plots = TRUE, 
  save_results = FALSE,
  calc_ses = FALSE,
  fake_data = NULL,
  save_dir = ".",
  verbose=TRUE,
  pause=TRUE
){
  
  if(calc_ses) stop("Have not implemented calc_ses yet.")
  
  raw_data <- gam_fit$raw_data
  params <- gam_fit$params
  K <- ncol(params@svd$u)
  
  validate_config(election_config)
  config <- extend_config(election_config)
  
  if(pause){
    pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))
  } else pause <- function() return()
  
  printv <- function(x) if(verbose) print(x)
  
  printv("predicting loess")
  log_pattern_predicted <- predict(
    gam_fit$fit, 
    terms=c("s(minute)"),
    type="terms",
    newdata = data.frame(
      minute=1:max(raw_data$minute), 
      row_number=raw_data$row_number[1]
    ) %>%
      left_join(raw_data[1,] %>% select(-minute), by="row_number")
  )
  
  log_precinct_predicted <- data.frame(precinct=unique(raw_data$precinct)) %>%
    mutate(
      precinct_re = predict(
        gam_fit$fit, 
        terms=c("s(precinct)"),
        type="terms",
        newdata = data.frame(
          precinct=precinct, 
          row_number=raw_data$row_number[1]
        ) %>%
          left_join(raw_data[1,] %>% select(-precinct), by="row_number")
      ) %>% as.vector()
    )

  ud_coef <- coef(gam_fit$fit)[paste0("UD.", 1:K)]
  
  precinct_df <- params@precinct_fe %>%
    group_by() %>%
    mutate(ud_beta = with(params@svd, as.vector(u %*% diag(d[1:K]) %*% ud_coef))) %>%
    left_join(log_precinct_predicted) %>%
    mutate(re_fit = precinct_fe + ud_beta + ifelse(!is.na(precinct_re), precinct_re, 0))
  
  ## Diagnostics
  if(!is.null(fake_data) & plots){
    printv("plots")
    time_plot <- ggplot(
      data.frame(
        minute = 1:max(raw_data$minute), 
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
    
    resid <- calc_resid_gam(gam_fit)
    
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
  
  printv("time_df")
  
  time_df <- data.frame(
    minute=1:max(raw_data$minute), 
    log_fit=as.vector(log_pattern_predicted) + attr(log_pattern_predicted, "constant")
  ) %>%
    filter(!is.na(log_fit)) %>%
    mutate(
      time_of_day = config$base_time + minutes(minute)
    )
  
  raw_data$resid <- calc_resid_gam(gam_fit, winsorize=WINSORIZE_RESID)
  
  model_predictions <- modelPredictions(
    precinct_df=precinct_df,
    time_df=time_df,
    raw_data=raw_data
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


get_precinct_time_turnout <- function(precinct_df, time_df){
  ## preserves sim column if exists
  full_predictions <- expand.grid(
    precinct=unique(precinct_df$precinct),
    time_of_day=unique(time_df$time_of_day)
  ) %>%
    left_join(precinct_df) %>%
    left_join(time_df) %>%
    mutate(
      turnout = exp(re_fit + log_fit)
    )
  return(full_predictions)
}

if(FALSE){
  ## NO LONGER WORKS WITH NEW TYPES
  ## Run Once:
  test_fit <- fit_gam(
    raw_data, 
    params,
    election_config
  )
  
  process_results_gam(
    gam_fit,
    election_config
  )
}
