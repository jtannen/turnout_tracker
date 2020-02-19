library(magrittr)

library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

source("fit_submissions.R")

setClass(
  "bootstrapResult",
  slots=c(
    raw_result='modelPredictions',
    bs_results='list'
  )
)

bootstrapResult <- function(raw_result, bs_results){
  new(
    "bootstrapResult", 
    raw_result=raw_result, 
    bs_results=bs_results
  )
}

fit_and_bootstrap <- function(
  raw_data,
  params,
  election_config,
  n_boot=40,
  verbose=TRUE
){
  diagnostic <- FALSE

  print("Raw Result")
  raw_result <- fit_data(raw_data, params, election_config)

  # bs_list <- lapply(as.list(1:n_boot), bootstrap_once)
  bs_list <- foreach(seed=1:n_boot) %do% 
    bootstrap_once(seed, raw_data, raw_result, params, config)

  eod_ci <- get_ci(
    predict_topline,
    raw_result, 
    bs_list, 
    keys=c("time_of_day"), 
    eod=TRUE
  )
  
  print("BS Turnout:")
  print(eod_ci)
  
  return(
    bootstrapResult(
      raw_result=raw_result,
      bs_results=bs_list
    )
  )
}

fit_data <- function(data, params, config, verbose=TRUE){
  single_fit <- fit_em_model(
    data, 
    params, 
    verbose=FALSE, 
    election_config=config
  )
  
  single_result <- process_results(
    single_fit, 
    election_config=config,
    plots = FALSE, 
    save_results = FALSE,
    verbose=FALSE
  )
  
  if(verbose){
    print(
      sprintf(
        "Result: %s", 
        scales::comma(predict_topline(single_result, eod=TRUE)$turnout)
      )
    )
  }
  
  return(single_result)
}

bootstrap_once <- function(seed, raw_data, raw_result, params, config){
  print(sprintf("Bootstrap %s", seed))
  set.seed(seed)
  bs_data <- raw_data %>% sample_frac(replace=TRUE) 
  bs_result <- fit_data(
    bs_data, params, config
  )
  
  bs_result %<>% extend_time_for_censored_sims(raw_result=raw_result)
  return(bs_result)
}

get_ci <- function(func, raw_result, bs_results, keys, ...){
  ### Get CI over the metric. Func should return a data.frame.
  ### CIs are calculated for all metrics but "keys".
  raw_metric <- func(raw_result, ...)
  bs_metrics <- bind_rows(
    lapply(bs_results, func, ...),
    .id="sim"
  )
  
  percentiles <- bs_metrics %>%
    select(-sim) %>%
    group_by(!!!syms(keys)) %>%
    summarise_all(
      list(
        p025 = ~quantile(., 0.025),
        p975 = ~quantile(., 0.975)
      )
    )
  
  bs_ci <- raw_metric %>%
    left_join(percentiles, by=keys)
    
  return(bs_ci)
}

get_ci_from_bs <- function(bs, func, keys, ...){
  get_ci(func, bs@raw_result, bs@bs_results, keys, ...)
}

extend_time_for_censored_sims <- function(bs_result, raw_result){
  max_time_raw <- raw_result@time_df %>% with(max(time_of_day))
  
  ## extend simulations in case the bootstrap cut off the times
  max_time <- max(bs_result@time_df$time_of_day)
  min_time <- min(bs_result@time_df$time_of_day)
  fit_at_max <- bs_result@time_df$log_fit[bs_result@time_df$time_of_day == max_time]

  all_times <- data.frame(
    time_of_day = raw_result@time_df$time_of_day
  )
  
  extended_time_df <- all_times %>%
    left_join(
      bs_result@time_df,
      by="time_of_day"
    ) %>%
    mutate(
      log_fit = ifelse(
        is.na(log_fit) & (time_of_day > max_time), 
        fit_at_max, 
        log_fit
      ),
      log_fit = ifelse(
        is.na(log_fit) & (time_of_day < min_time), 
        -100, 
        log_fit
      )
    )
  bs_result@time_df <- extended_time_df
  return(bs_result)
}

# add_sum_exp_re <- function(time_sim_df, ranef_df){
#   sum_exp_re_sim <- ranef_df %>% 
#     group_by(sim) %>% 
#     summarise(sum_exp_re = sum(exp(re_fit)))
#   
#   time_sim_df %>% 
#     left_join(sum_exp_re_sim) %>%
#     mutate(turnout = exp(log_fit) * sum_exp_re)
# }
# 
# get_time_ci_from_df <- function(time_sim_df){
#   time_sim_df %>% 
#     group_by(time_of_day) %>%
#     summarise(
#       turnout_025 = quantile(turnout, 0.025, names = FALSE),
#       turnout_500 = quantile(turnout, 0.5, names = FALSE),
#       turnout_975 = quantile(turnout, 0.975, names = FALSE)
#     )
# }
# 
# get_time_ci <- function(bs){
#   get_time_ci_from_df(bs@time_sim_df)
# }
#
# get_eod_ci <- function(bs){
#   bs@time_sim_df %>% 
#     get_time_ci_from_df() %>% 
#     filter_to_eod()
# }
# 
# get_precinct_eod_turnout <- function(bs){
#   get_precinct_time_turnout(
#     bs@precinct_sim_df,
#     bs@time_sim_df %>% filter_to_eod()
#   )
# }

hist_bootstrap <- function(bs){
  sum_exp_re <- sum(exp(bs@single_fit@precinct_re_fit))
  single_turnout <- bs@single_result@time_df %>% 
    tail(1) %>% 
    with(
      exp(log_fit) * sum_exp_re
    )
  
  ggplot(
    bs@bootstrap_topline ,
    aes(x = turnout)
  ) +
    geom_histogram() +
    geom_vline(
      xintercept = single_turnout,
      color = 'green', 
      size = 2
    ) +
    geom_vline(
      data = data.frame(
        x = unlist(tail(bs@bootstrap_ci, 1))[c(2,4)]
      ),
      aes(xintercept = x),           
      color = strong_blue,
      size = 2
    ) +
    scale_color_identity() +
    ggtitle("Bootstrap Results") +
    scale_x_continuous(
      "Predicted Turnout",
      labels = scales::comma                 
    ) +
    theme_sixtysix()
}


if(FALSE){
  bs <- fit_bootstrap(
    raw_data,
    params,
    n_boot=40
  )
  gg_bs_hist <- hist_bootstrap(bs)
  
  gg_turnout <- turnout_plot(
    bs,
    raw_data,
    ref_turnout=10000,
    ref_text="aaa"
  )
  
  save_with_backup(
    bootstrap_results, bootstrap_ci,
    "bootstrap",
    "."
  )  
  
  for(plotname in c("gg_turnout", "gg_bs_hist")){
    ggsave_with_backup(
      get(plotname), 
      filestem=plotname,
      plottype="png",
      width = 7,
      height=7,
      dir='outputs'
    )
  }
}
