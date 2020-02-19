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
  Sigma_inv_plus_N <- model_fit@Sigma_inv_plus_N
  
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
    Sigma_inv_plus_N=Sigma_inv_plus_N
  )
  
  return(model_fit)
}

optimize_precinct_re <- function(
  mu,
  sigma_inv,
  n_obs,
  x_sum,
  sigma_noise,
  Sigma_inv_plus_N
){
  mu_plus_x <- sigma_inv %*% mu + x_sum / sigma_noise^2
  new_mu <- solve(Sigma_inv_plus_N, mu_plus_x)
  return(as.vector(new_mu))
}

precomp_Sigma_inv_plus_N_inv <- function(
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
  
  result <- Sigma - (
    Sigma %*% U %*% 
      solve(
        Diagonal(n = sum(n_obs_p > 0)) +
          V %*% Sigma %*% U
      ) %*% 
    V %*% Sigma
  )
  
  return(result)
}

calc_Sigma_inv_plus_N <- function(
  n_obs_p,
  params,
  sigma_noise
){
  ## Calculates (Sigma^-1 + N/s^2)
  Sigma_inv_plus_N <- 
    Diagonal(
      length(n_obs_p), 
      n_obs_p / sigma_noise^2 
    ) + 
    params@precinct_cov_inv

  return(Sigma_inv_plus_N)
}

get_loess_params <- function(n_obs){
  if(n_obs < 50){
    degree <- 1
    span <- 1
  }
  if(n_obs >= 50 & n_obs < 500){
    degree <- 1
    span <- 0.3
  }
  if(n_obs >= 500){
    degree <- 2
    span <- 0.3
  }
  return(list(degree=degree, span=span))
}

optimize_loess <- function(
  model_fit, 
  span, 
  degree
){
  resid <- calc_resid(model_fit, winsorize=WINSORIZE_RESID)
  loess_pred_old <- predict_loess_obs(model_fit)
  minute_obs <- model_fit@raw_data$minute
  
  params <- get_loess_params(length(minute_obs))

  model_fit@loess_fit[[1]] <- loess(
    x ~ minute, 
    data.frame(
      x=resid + loess_pred_old,
      minute=minute_obs
    ),
    span=params$span,
    degree=params$degree
  )

  return(model_fit)
}

mean_abs_resid <- function(current_fit){
  mean(abs(calc_resid(current_fit)))
}

initialize_model_fit <- function(
  raw_data, 
  params, 
  config, 
  sigma_noise,
  verbose
){
  n_obs <- nrow(raw_data)
  
  ## Precomp values
  x_tab <- tabulate_x(
    raw_data$precinct_num,
    rep(0, n_obs),
    params@n_precincts
  )
  n_obs_p <- x_tab$n_obs_p 
  x_sum_p <- x_tab$x_sum_p
  
  if(verbose) print("Precomputing Matrix")
  Sigma_inv_plus_N <- calc_Sigma_inv_plus_N(
    n_obs_p, params, sigma_noise
  )
  
  if(verbose) print("Sampling")
  ## initialize  
  precinct_re_fit <- rep(0, params@n_precincts)
  log_pattern_fit <- rep(0, config$n_minutes)
  
  ## TODO generalize beyond loess, create "predict_time()" function instead
  dummy_loess <- loess(x ~ minute, data.frame(x=0, minute=raw_data$minute))
  
  modelFit(
    precinct_re_fit=precinct_re_fit,
    loess_fit=dummy_loess,
    Sigma_inv_plus_N=Sigma_inv_plus_N,
    sigma_noise=sigma_noise,
    raw_data=raw_data,
    params=params
  )
}


fit_em_model <- function(
  raw_data, 
  params,
  election_config,
  sigma_noise=0.1, 
  tol=1e-10,
  verbose=TRUE
){
  
  ## TODO: should this use mgcv::gam?
  validate_config(election_config)
  config <- extend_config(election_config)
  
  current_fit <- initialize_model_fit(
    raw_data, params, config, sigma_noise, verbose
  )
  loess_predicted_eod <- 0
  
  maybe_print <- function(x) if(verbose) print(x)
  maybe_print_resid <- function(current_fit){
      maybe_print(sprintf("Mean Resid: %s", mean_abs_resid(current_fit)))
  }

  continue <- TRUE
  loop_no <- 0
  
  while(continue){
    maybe_print(paste("Iter ", loop_no))
    loop_no <- loop_no + 1
    
    current_fit %<>% optimize_precinct_re_of_model()
    maybe_print_resid(current_fit)
      
    current_fit %<>% optimize_loess(
      loess_span,
      loess_degree
    )    
    maybe_print_resid(current_fit)
    
    prior_predicted_eod <- loess_predicted_eod
    loess_predicted_eod <- predict_loess_obs(current_fit, end_of_day=TRUE)
    
    maybe_print(paste0("Final Turnout: ", loess_predicted_eod))
    maybe_print(paste0("Abs Change: ", abs(loess_predicted_eod - prior_predicted_eod)))
    
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
  
  raw_data$resid <- calc_resid(model_fit, winsorize=WINSORIZE_RESID)

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
  test_fit <- fit_em_model(
    raw_data, 
    params,
    election_config
  )
  
  process_results(
    test_fit$precinct_re_fit,
    test_fit$loess_fit,
    test_fit$Sigma_inv_plus_N,
    raw_data,
    test_fit$resid,
    params,
    fake_data=fake_data
  )
}
