library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(sf)

###############################################
# This script calculates the fixed effects and covariance matrix
# using historic data.
###############################################

source("theme_sixtysix.R")
source("util.R")
source("types.R") # imports modelParams, validate_turnout_df, validate_precinct_sf

calc_params <- function(
  turnout_df,
  n_svd=3
){
  
  ########################################
  ## Given turnout (counts of total voters) for each precinct p in each election y,
  ## this calculates the historic fixed effects and correlations among precincts,
  ## on the log-scale.
  ##
  ## log(turnout[p, y] + 1) = 
  ##     election_fe[y] + precinct_fe[p] + e[p, y]                    
  ## 
  ## where cov(e[p, y], e[p', y]) is estimated using svd().
  ##
  ##
  ## INPUT:
  ##
  ## turnout_df: the input df, with columns precinct, election, turnout
  ##   
  ## OUTPUT: An object of class modelParams with the following:
  ##
  ## turnout_df: the input df with some new additional info
  ## precinct_fe: A data.frame with precinct fixed effects
  ## election_fe: a data.frame with election fixed effects.
  ## svd: the output of svd on the residuals
  ## precinct_cov: the precincts' estimated covariance matrix of log(turnout + 1)  
  ## precinct_cov_inv: the inverse of precinct_cov (it's useful to precompute)  
  ##
  ########################################
  
  validate_turnout_df(turnout_df)
  
  print("Fitting fixed effects")
  precincts <- sort(unique(turnout_df$precinct))
  dates <- sort(unique(turnout_df$election))

  turnout_df <- arrange(turnout_df, precinct, election)

  turnout_df <- turnout_df %>% 
    mutate(
      precinct = factor(precinct, levels = precincts),
      log_turnout = log(turnout + 1)
    ) %>%
    mutate(precinct_num = as.numeric(precinct))

  election_fe <- turnout_df %>% 
    group_by(election) %>%
    summarise(election_fe = mean(log_turnout))
  
  precinct_fe <- turnout_df %>%
    left_join(election_fe) %>%
    mutate(
      residual = log_turnout - election_fe
    ) %>%
    group_by(precinct) %>%
    summarise(precinct_fe = mean(residual))
  
  resid_mat <- turnout_df %>%
    group_by() %>%
    left_join(election_fe) %>%
    left_join(precinct_fe) %>%
    mutate(residual = log_turnout - election_fe - precinct_fe) %>%
    select(precinct, election, residual) %>%
    spread(key = election, value = residual) 
  
  mat <- resid_mat %>%
    select(-precinct) %>%
    as.matrix()

  print("Calculating svd")
  
  svd <- svd(mat, n_svd, n_svd)
  
  ## winsorize the svd
  for(i in 1:n_svd){
    threshold <- quantile(abs(svd$u[,i]), 0.9995)
    svd$u[,i] <- sign(svd$u[,i]) * pmin(abs(svd$u[,i]), threshold)
  }
  
  true_mat <- cbind(mat, svd$u %>% as.data.frame())
  
  fitted <- svd$u %*% diag(svd$d[1:n_svd]) %*% t(svd$v)
  
  print("Fitted vs True values, check for similarity:")
  print("Fitted:")
  print(fitted[1:6, 1:6])
  print("True:")
  print(true_mat[1:6, 1:6])

  print("Calculating covariances")
  
  precinct_cov <- svd$u %*% diag(svd$d[1:n_svd]) %*% cov(svd$v) %*% diag(svd$d[1:n_svd]) %*% t(svd$u)
  diag(precinct_cov) <- diag(precinct_cov) + var(as.vector(fitted - mat))
  precinct_cov_inv <- solve(precinct_cov)
  
  return(
    modelParams(
      turnout_df=turnout_df,
      election_fe=election_fe,
      precinct_fe=precinct_fe,
      svd=svd,
      precinct_cov=precinct_cov,
      precinct_cov_inv=precinct_cov_inv
    )
  )
}
 

map_precinct_fe <- function(params, precinct_sf){
  if(!is(params, "modelParams")) stop("params must be of class modelParams")
  validate_precinct_sf(precinct_sf, params)
  
  precinct_sf$area <- as.numeric(st_area(precinct_sf))
  
  ggplot(
    precinct_sf %>% 
      left_join(params@precinct_fe, by="precinct")
  ) +
    geom_sf(
      aes(fill = precinct_fe - log(area)),
      color= NA
    ) +
    scale_fill_viridis_c("Turnout Fixed Effect")+
    theme_map_sixtysix() +
    ggtitle("Precinct Fixed Effects [log(Votes per Mile)]")
} 

plot_election_fe <- function(params, config){
  if(!is(params, "modelParams")) stop("params must be of class modelParams")
  
  election_df <- params@election_fe %>%
    mutate(
      year = config$get_year_from_election(election),
      etype = config$get_etype_from_election(election)
    )
  
  ggplot(
    election_df,
    aes(x=year, y=election_fe)
  ) +
    geom_line(
      aes(group=asnum(substr(election, 1, 4)) %% 4),
      color= strong_green
    ) +
    geom_point(
      color = strong_green,
      size = 2
    ) +
    facet_grid(etype ~ .) +
    xlab("") +
    scale_fill_gradient2("Turnout Fixed Effect")+
    theme_sixtysix() +
    ggtitle("election Fixed Effects", "Grouped by 4 election cycle")
} 

map_svd_dim <- function(params, k, precinct_sf){
  if(!is(params, "modelParams")) stop("params must be of class modelParams")
  validate_precinct_sf(precinct_sf, params)
  
  u_df <- data.frame(
    score=as.vector(params@svd$u[,k]), 
    precinct=sort(unique(params@turnout_df$precinct))
  )

  ggplot(precinct_sf %>% left_join(u_df)) +
    geom_sf(
      aes(fill = score),
      color = NA
    ) +
    scale_fill_gradient2(
      paste("Score, Dimension", k), 
      midpoint = 0
    )+
    theme_map_sixtysix() +
    ggtitle(paste("Turnout Dimension", k))
} 

plot_election_svd_dim <- function(params, k, config){
  if(!is(params, "modelParams")) stop("params must be of class modelParams")
  
  v_df <- data.frame(
    score=as.vector(params@svd$v[,k]) * params@svd$d[k], 
    election=sort(unique(params@turnout_df$election))
  ) %>%
    mutate(
      year = config$get_year_from_election(election),
      etype = config$get_etype_from_election(election)
    )
  
  ggplot(
    v_df,
    aes(x = year, y = score)
  ) +
    geom_line(
      aes(group=year %% 4),
      color = strong_green,
      lwd = 1
    ) +
    geom_point(
      color = strong_green,
      size = 4
    ) +
    xlab("") +
    facet_grid(etype ~ .)+
    theme_sixtysix() +
    theme(
      panel.grid.minor.x = element_line(
        colour = 'grey90',
        linetype = 'dashed'
      )
    ) +
    scale_y_continuous("Score") + 
    ggtitle(paste("Dimension", k))
} 


diagnostics <- function(params, precinct_sf, config, pause=TRUE){
  if(!is(params, "modelParams")) stop("params must be of class modelParams")
  validate_precinct_sf(precinct_sf, params)
  
  print("Plotting Diagnostics...")
  
  if(pause){
    pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))
  } else pause <- function() return()
  
  map_precinct_fe(params, precinct_sf) %>% print
  pause()
  
  plot_election_fe(params, config) %>% print
  pause()
  
  for(k in 1:ncol(params@svd$u)){
    map_svd_dim(params, k, precinct_sf) %>% print()
    pause()
    
    plot_election_svd_dim(params, k, config) %>% print()
    pause()
  }
}
