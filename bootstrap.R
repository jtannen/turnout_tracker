source("fit_submissions.R")

fit_bootstrap <- function(
  raw_data,
  params,
  election_config,
  n_boot=40,
  use_inverse=FALSE,
  verbose=TRUE
){
  bootstrap_results <- vector(mode = "list", length = n_boot)
  ranef_results <- vector(mode = "list", length = n_boot)
  diagnostic <- FALSE

  print("Raw Result")
  single_fit <- fit_em_model(
    raw_data, params, verbose=FALSE, use_inverse=use_inverse, election_config=election_config
  )
  single_result <- process_results(
    single_fit$precinct_re_fit, 
    single_fit$loess_fit, 
    raw_data,
    single_fit$resid,
    params,
    election_config=election_config,
    plots = FALSE, 
    save_results = FALSE,
    verbose=FALSE
  )
  if(verbose){
    print(paste(
      "Raw Result:", 
      exp(tail(single_result$time_df$log_fit, n=1)) * 
        sum(exp(single_result$precinct_df$re_fit))
    ))
  }  
    
  for(i in 1:n_boot){
    print(paste0("Boot ", i))
    bs_data <- raw_data %>% sample_frac(replace = TRUE) 
    bs_params <- fit_em_model(
      bs_data, params, verbose=FALSE,use_inverse=use_inverse,election_config=election_config
    )
    bs_results <- process_results(
      bs_params$precinct_re_fit, 
      bs_params$loess_fit, 
      bs_data,
      bs_params$resid,
      params=params,
      election_config=election_config,
      plots = FALSE, 
      save_results = FALSE,
      verbose=FALSE
    )
    
    bootstrap_results[[i]] <- bs_results$time_df %>% 
      mutate(sim = i)
    
    ranef_results[[i]] <- bs_results$precinct_df %>% 
      select(precinct, re_fit) %>% mutate(sim=i)
    
    if(verbose){
      print(paste(
        i, 
        exp(tail(bs_results$time_df$log_fit, n=1)) * 
          sum(exp(bs_results$precinct_df$re_fit))
      ))
    }
  }
  ranef_df <- bind_rows(ranef_results)
  bootstrap_df <- bind_rows(bootstrap_results)
  
  max_time_of_day <- max(bootstrap_df$time_of_day)
  
  ## extend sims
  bootstrap_df <- bootstrap_df %>%
    full_join(
      bootstrap_df %>%
        group_by(sim) %>%
        mutate(min_time = min(time_of_day)) %>%
        filter(time_of_day == max(time_of_day)) %>%
        select(sim, log_fit, time_of_day, min_time) %>% 
        rename(max_log_fit = log_fit, max_time = time_of_day) %>%
        right_join(
          expand.grid(
            time_of_day = unique(bootstrap_df$time_of_day),
            sim = unique(bootstrap_df$sim)
          )
        )
    ) %>%
    mutate(
      log_fit = ifelse(is.na(log_fit) & (time_of_day > max_time), max_log_fit, log_fit),
      log_fit = ifelse(is.na(log_fit) & (time_of_day < min_time), -100, log_fit)
    ) %>%
    select(-max_log_fit)
  
  
  bootstrap_df <- bootstrap_df %>% 
    left_join(ranef_df %>% group_by(sim) %>% summarise(sum_exp_re = sum(exp(re_fit)))) %>%
    mutate(turnout = exp(log_fit) * sum_exp_re)
  
  bootstrap_topline <- bootstrap_df %>% group_by(sim) %>% filter(time_of_day == max(time_of_day))
  
  bootstrap_ci <- bootstrap_df %>% 
    group_by(time_of_day) %>%
    summarise(
      turnout_025 = quantile(turnout, 0.025, names = FALSE),
      turnout_500 = quantile(turnout, 0.5, names = FALSE),
      turnout_975 = quantile(turnout, 0.975, names = FALSE)
    )
  
  bs_ci <- quantile(bootstrap_topline$turnout, c(0.025, 0.500, 0.975))
  print(paste(c("BS Turnout:", round(bs_ci)), collapse = " "))
  
  return(
    list(
      single_result=single_result,
      bootstrap_df=bootstrap_df,
      bootstrap_topline=bootstrap_topline,
      bootstrap_ci=bootstrap_ci,
      ranef_df=ranef_df
    )
  )
}



hist_bootstrap <- function(bs){
  single_turnout <- bs$single_result$time_df %>% 
    tail(1) %>% 
    with(
      exp(log_fit) * sum(exp(fit$precinct_df$re_fit))
    )
  
  ggplot(
    bs$bootstrap_topline ,
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
        x = unlist(tail(bs$bootstrap_ci, 1))[c(2,4)]
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


turnout_plot <- function(
  bs,
  raw_data,
  election_config
){
  
  config <- extend_config(election_config)
  ref_turnout <- config$ref_turnout
  
  single_fit <- bs$single_result
  
  resid <- raw_data %>%
    mutate(time_of_day = config$base_time + minutes(minute)) %>%
    left_join(
      single_fit$time_df %>% 
        select(time_of_day, log_fit) %>%
        rename(time_fit = log_fit) %>%
        mutate(total_turnout = bs$bootstrap_ci$turnout_500)
    ) %>%
    mutate(
      re_fit = single_fit$precinct_df$re_fit[precinct_num],
      resid = log_obs - time_fit - re_fit
    )
  
  ggplot(
    bs$bootstrap_ci,
    aes(
      x = time_of_day,
      y = turnout_500
    )
  ) +
    geom_point(data = resid, aes(y=total_turnout * exp(resid))) +
    geom_ribbon(
      aes(ymin = turnout_025, ymax = pmin(turnout_975, 800e6)),
      alpha = 0.2,
      color = NA,
      fill = strong_purple
    ) +
    geom_line(size = 2, color = strong_purple) +
    scale_x_datetime("", date_labels = "%I", date_breaks = '1 hour') +
    scale_y_continuous("", labels = scales::comma) +
    geom_hline(
      yintercept = ref_turnout
    ) + 
    geom_text(
      data = data.frame( 
        turnout_500 = ref_turnout,
        time_of_day = rep(config$base_time + minutes(30), length(ref_turnout)),
        label = sprintf("%s Turnout = %s", names(ref_turnout), scales::comma(ref_turnout))
      ),
      aes(label = label),
      vjust = 1.2,
      hjust = 0
    ) +
    expand_limits(x = config$election_day + hours(config$end_hour), y=0) + 
    ggtitle("Estimated Election Turnout") +
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
