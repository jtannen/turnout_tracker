library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(Matrix)
select <- dplyr::select

source("theme_sixtysix.R")
source('util.R')
source('types.R')


#######################
## SET ELECTION PARAMS
#######################

load_google_data <- function(
  election_config,
  rda="outputs/google_download.Rda"
){
  validate_config(election_config)
  config <- extend_config(election_config)
  
  raw_data <- safe_load(rda)
  
  raw_data <- raw_data %>% filter(
    !is.na(obs) & 
      !is.na(time) & 
      !is.na(precinct)
  )
  
  # filter obviously unreasonable submissions
  raw_data <- raw_data %>% filter(obs > 0 & obs <= 900) 
  raw_data <- raw_data %>% filter(
    time <= Sys.time() &
      time >= config$base_time &
      time <= config$base_time + hours(config$end_hour - config$start_hour)  
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
  
  year_fe_samp <- sample(params@year_fe$year_fe, 1)
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
      year_fe_samp + 
        precinct_re_samp[precinct_num] +
        noise +
        true_log_pattern[minute]
    )
  )  
  
  raw_data <- fake_data %>% 
    mutate(
      row_number = 1:n(),
      precinct = params@precinct_fe$precinct[precinct_num],
      time = as.character(config$base_time + minutes(minute))
    ) %>%
    dplyr::select(row_number, precinct, time, obs)
  
  true_turnout <- sum(exp(year_fe_samp + precinct_re_samp))
  
  return(
    list(
      raw_data=raw_data, 
      year_fe_samp=year_fe_samp, 
      precinct_re_samp=precinct_re_samp,
      true_pattern=true_pattern,
      true_turnout=true_turnout
    )
  )
}


load_data <- function(use_real_data, election_config, params, google_rda="outputs/google_download.Rda"){
  validate_config(election_config)
  if(use_real_data){
    fake_data <- NULL
    raw_data <- load_google_data(election_config, google_rda)
  } else {
    fake_data <- generate_fake_data(params, election_config)
    raw_data <- fake_data$raw_data
  }  

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
  
  sum_tab <- tapply(INDEX=precinct, X=x, FUN=sum)
  x_sum_p[asnum(names(sum_tab))] <- sum_tab
  
  return(
    list(
      n_obs_p=n_obs_p, 
      x_sum_p=x_sum_p
    )
  )
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
  if(first_mat_is_inv){
    new_mu <- first_mat %*%
      (sigma_inv %*% mu + x_sum / sigma_noise^2)
  }else{
    new_mu <- solve(first_mat, (sigma_inv %*% mu + x_sum / sigma_noise^2))
  }
  return(as.vector(new_mu))
}

precomp_re_first_mat_inv <- function(
  x_list,
  params,
  sigma_noise
){
  U <- Matrix::sparseMatrix(
    i = which(x_list$n_obs_p > 0),
    j = 1:sum(x_list$n_obs_p > 0),
    x = sqrt(x_list$n_obs_p[x_list$n_obs_p > 0]) / sigma_noise,
    dims = c(nrow(params@precinct_cov), sum(x_list$n_obs_p > 0))
  )
  V <- t(U)
  first_mat_stored <- params@precinct_cov - 
    params@precinct_cov %*% U %*% 
    solve(
      Diagonal(n = sum(x_list$n_obs_p > 0)) +
        V %*% params@precinct_cov %*% U
    ) %*% V %*% params@precinct_cov
  first_mat_is_inv <- TRUE
  return(list(first_mat_stored, first_mat_is_inv))
}

precomp_re_first_mat <- function(
  x_list,
  params,
  sigma_noise
){
  first_mat_stored <- 
    Diagonal(
      length(x_list$n_obs_p), 
      x_list$n_obs_p / sigma_noise^2 
    ) + 
    params@precinct_cov_inv
  first_mat_is_inv <- FALSE
  return(list(first_mat_stored, first_mat_is_inv))
}


calc_resid <- function(
  log_obs,
  obs_precinct_num,
  precinct_fit,
  loess_predicted
){
  return(
    log_obs +
      -precinct_fit[obs_precinct_num] +
      -loess_predicted
  )
}



get_loess_params <- function(n_obs){
  if(n_obs < 10){
    loess_degree <- 1
    loess_span <- 1
  }
  if(n_obs >= 10 & n_obs < 100){
    loess_degree <- 1
    loess_span <- 0.3
  }
  if(n_obs >= 100){
    loess_degree <- 2
    loess_span <- 0.2
  }
  return(list(loess_degree=loess_degree, loess_span=loess_span))
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
  n_precincts <- nrow(params@precinct_fe)
  
  ## Precomp values
  x_list <- tabulate_x(
    raw_data$precinct_num,
    rep(0, n_obs),
    n_precincts
  )
  
  if(verbose) print("Precomputing Matrix")
  if(use_inverse){
    precomp_mat <- precomp_re_first_mat_inv(x_list, params, sigma_noise)
  }else{
    precomp_mat <- precomp_re_first_mat(x_list, params, sigma_noise)
  }
  first_mat_stored <- precomp_mat[[1]]
  first_mat_is_inv <- precomp_mat[[2]]

  if(verbose) print("Sampling")
  ## initialize  
  precinct_re_fit <- rep(0, n_precincts)
  log_pattern_fit <- rep(0, config$n_minutes)
  loess_predicted <- 0
  loess_predicted_final <- 0
  
  resid <- calc_resid(
    raw_data$log_obs,
    raw_data$precinct_num,
    precinct_re_fit,
    loess_predicted
  )
  
  continue <- TRUE
  loop_no <- 0
  
  while(continue){
    if(verbose) print(paste("Iter ", loop_no))
    loop_no <- loop_no + 1
    
    x_list <- tabulate_x(
      raw_data$precinct_num,
      resid + precinct_re_fit[raw_data$precinct_num],
      n_precincts
    )
    
    precinct_re_fit <- optimize_precinct_re(
      mu=params@precinct_fe$precinct_fe,
      sigma_inv=params@precinct_cov_inv,
      n_obs = x_list$n_obs_p,
      x_sum = x_list$x_sum_p,
      sigma_noise=sigma_noise,
      first_mat=first_mat_stored,
      first_mat_is_inv=first_mat_is_inv
    )
    
    old_resid <- resid
    resid <- calc_resid(
      raw_data$log_obs,
      raw_data$precinct_num,
      precinct_re_fit,
      loess_predicted
    )
    
    if(verbose){
      print(paste0("Sum Sq Resid: ", sum(resid^2)))
      print(paste0("Change: ", sum(old_resid^2)- sum(resid^2)))
    }
    
    loess_fit <- loess(
      x ~ minute, 
      data.frame(
        x=resid + loess_predicted,
        minute=raw_data$minute
      ),
      span = loess_span,
      degree = loess_degree
    )
    loess_predicted <- predict(loess_fit)
    
    old_predicted_final <- loess_predicted_final
    loess_predicted_final <- predict(
      loess_fit, 
      newdata=data.frame(minute=max(raw_data$minute))
    )
    
    old_resid <- resid
    
    resid <- calc_resid(
      raw_data$log_obs,
      raw_data$precinct_num,
      precinct_re_fit,
      loess_predicted
    )
    
    if(verbose){
      print(paste0("Sum Sq Resid: ", sum(resid^2)))
      print(paste0("Change: ", sum(old_resid^2)- sum(resid^2)))
      print(paste0("Final Turnout: ", loess_predicted_final))
      print(paste0("Change: ", abs(loess_predicted_final - old_predicted_final)))
    }
    
    # continue <- abs(sum(resid^2) - sum(old_resid^2)) > (tol * sum(old_resid^2))
    continue <- abs(loess_predicted_final - old_predicted_final) > (tol * old_predicted_final)
  }
  print(paste('n_iter =', loop_no))
  
  return(list(
    precinct_re_fit=precinct_re_fit, 
    loess_fit=loess_fit, 
    first_mat_stored=first_mat_stored,
    first_mat_is_inv=first_mat_is_inv,
    resid=resid,
    sigma_noise=sigma_noise
  ))
}  

process_results <- function(
  precinct_re_fit, 
  loess_fit, 
  raw_data,
  resid,
  params,
  election_config,
  plots = TRUE, 
  save_results = FALSE,
  calc_ses = TRUE,
  fake_data = NULL,
  save_dir = ".",
  verbose=TRUE
){
  validate_config(election_config)
  config <- extend_config(election_config)
  
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
    left_join(precinct_df) %>%
    left_join(time_df) %>%
    mutate(
      prediction = exp(re_fit + log_fit)
    )


  if(save_results){
    printv('saving predictions')
    save_with_backup(
      precinct_df, time_df, full_predictions,
      save_dir=save_dir
    )

  }

  return(
    list(
      precinct_df=precinct_df,
      time_df=time_df,
      full_predictions=full_predictions
    )
  )
}


if(FALSE){
  ## Run Once:
  test_fit <- fit_em_model(
    raw_data, 
    params
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
