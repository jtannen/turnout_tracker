source("fit_submissions.R")

fit_and_bootstrap <- function(
  raw_data,
  params,
  election_config,
  n_boot=40,
  use_inverse=FALSE,
  verbose=TRUE
){
  bs_list <- vector(mode = "list", length = n_boot)
  
  diagnostic <- FALSE

  print("Raw Result")
  single_fit <- fit_em_model(
    raw_data, 
    params, 
    verbose=FALSE, 
    use_inverse=use_inverse, 
    election_config=election_config
  )
  
  single_result <- process_results(
    single_fit, 
    election_config=election_config,
    plots = FALSE, 
    save_results = FALSE,
    verbose=FALSE
  )
  
  if(verbose){
    print(sprintf("Raw Result: %s", predict_topline(single_result)))
  }  
    
  for(i in 1:n_boot){
    print(paste0("Boot ", i))
    bs_data <- raw_data %>% sample_frac(replace=TRUE) 
    bs_fit <- fit_em_model(
      bs_data, 
      params, 
      verbose=FALSE,
      use_inverse=use_inverse,
      election_config=election_config
    )
    
    bs_list[[i]] <- process_results(
      bs_fit,
      election_config=election_config,
      plots = FALSE, 
      save_results = FALSE,
      verbose=FALSE
    )
    
    if(verbose){
      print(sprintf(
        "%s: %s",
        i, 
        predict_topline(bs_list[[i]])
      ))
    }
  }
  
  bind_dfs <- function(slot_name){
    bind_rows(
      lapply(bs_list, slot, slot_name),
      .id="sim"
    )
  }
  
  bootstrap_df <- bind_dfs("time_df")
  ranef_df <- bind_dfs("precinct_df")

  max_time_of_day <- max(bootstrap_df$time_of_day)
  
  ## extend simulations in case the bootstrap cut off the times
  sim_times <- bootstrap_df %>%
    group_by(sim) %>%
    summarise(
      min_time = min(time_of_day),
      max_time = max(time_of_day),
      fit_at_max = log_fit[time_of_day == max_time]
    )
  
  bootstrap_df <- bootstrap_df %>% 
    right_join(
      data.frame(time_of_day = unique(bootstrap_df$time_of_day)),
      by="time_of_day"
    ) %>%
    left_join(sim_times, by="sim") %>%
    mutate(
      log_fit = ifelse(is.na(log_fit) & (time_of_day > max_time), fit_at_max, log_fit),
      log_fit = ifelse(is.na(log_fit) & (time_of_day < min_time), -100, log_fit)
    ) %>%
    select(-min_time, -max_time, -fit_at_max)
  
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
      single_fit=single_fit,
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
  election_config
){
  config <- extend_config(election_config)
  ref_turnout <- config$ref_turnout
  
  single_fit <- bs$single_fit
  single_result <- bs$single_result
  time_df <- single_result@time_df 
  precinct_df <- single_result@precinct_df
  
  resid <- data.frame(
    time_of_day = config$base_time + minutes(single_fit@raw_data$minute),
    resid = calc_resid(single_fit, winsorize = TRUE)
  ) %>%
    left_join(
      time_df %>% 
        select(time_of_day, log_fit) %>%
        rename(time_fit = log_fit) %>%
        mutate(total_turnout = bs$bootstrap_ci$turnout_500),
      by="time_of_day"
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
