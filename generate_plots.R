library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(stargazer)

source("theme_sixtysix.R", chdir=TRUE)
source("bootstrap.R", chdir=TRUE)
source("util.R")

strip_leading_zero <- function(x) gsub("^(0|\\s)+", "", x)
get_subtitle <- function(current_time){
  paste(
    "Results as of", 
    strip_leading_zero(
      format(current_time, "%I:%M %p")
    )
  )
}

div_to_ward <- function(divid){
  return(substr(divid, 1, 2))
}

turnout_plot <- function(
  bs,
  raw_data,
  election_config
){
  config <- extend_config(election_config)
  ref_turnout <- config$ref_turnout
  
  ci_df <- get_ci_from_bs(bs, predict_topline, keys="time_of_day", eod=FALSE)
  
  resid_data <- raw_data %>%
    mutate(
      pred = predict_turnout(
        bs@raw_result, 
        precinct=precinct, 
        time_of_day=time_of_day
      )$turnout,
      resid = winsorize(log_obs - log(pred))
    ) %>% 
    left_join(ci_df)
  
  ggplot(
    ci_df,
    aes(x=time_of_day, y=turnout)
  ) +
    geom_point(data=resid_data, aes(y=turnout * exp(resid))) +
    geom_ribbon(
      aes(ymin = p025, ymax = pmin(p975, 800e6)),
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
        turnout = ref_turnout,
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


gg_wards_relative <- function(wards, current_time, config){
  ggplot(wards) +
    geom_sf(aes(fill = re_over_fe), color = NA)+
    scale_fill_viridis_c(
      "Adjusted Turnout.\n 1 = Typical"
    ) +
    ggtitle(
      "Where turnout is disproportionately high", 
      paste(
        "Turnout adjusted for (1) typical Ward turnout and\n (2) current citywide turnout.",
        get_subtitle(current_time)
      )
    ) +
    theme_map_sixtysix() %+replace%
    theme(legend.position = config$map_legend_pos)
}

gg_wards_predicted <- function(wards, current_time, config) {
  ggplot(wards) +
    geom_sf(
      aes(fill = turnout),
      color = NA
    ) +
    scale_fill_viridis_c(
      "Estimated Votes",
      labels = scales::comma
    ) +
    ggtitle(
      "Estimated Votes", 
      get_subtitle(current_time)
    ) +
    theme_map_sixtysix() %+replace%
    theme(legend.position = config$map_legend_pos)
}

gg_wards_turnout_change <- function(wards, past_turnout, current_time, config){

  ggplot(wards) +
    geom_sf(
      aes(fill = pmin(turnout / turnout_2014, 2.25)),
      color = NA
    ) +
    scale_fill_viridis_c(
      "Turnout /\n 2014 Turnout",
      breaks = seq(0.5, 2.25, 0.25),
      labels = c(seq(0.5, 2.0, 0.25), "2.5+")
    ) +
    ggtitle("Turnout as a fraction of 2014",
            paste("Current estimated turnout vs. 2014 final turnout.",
                  get_subtitle(current_time))
    )+
    theme_map_sixtysix() %+replace%
    theme(legend.position = config$map_legend_pos)
}

gg_wards_labelled <- function(wards) {
  ward_centroids <- as.data.frame(
    st_coordinates(st_centroid(wards))
  ) %>% mutate(ward = wards$ward)
  
  ggplot(wards) +
    geom_sf(fill = light_blue, color = 'grey70') +
    geom_text(
      data = ward_centroids,
      aes(x=X,y=Y,label=ward)
    ) +
    theme_map_sixtysix() 
}

gg_wards_submissions <- function(wards, raw_data, current_time, config) {
  ggplot(
    wards %>%
      left_join(
        raw_data %>%
          group_by(ward) %>%
          count(),
        by = c('ward')
      )
  ) +
    geom_sf(
      aes(fill = n),
      color = NA
    ) +
    scale_fill_viridis_c(
      "Submissions",
      option = "plasma"
    ) +
    ggtitle("Data Submissions", 
            paste("Number of people who have submitted their voter number.\n", 
                  get_subtitle(current_time))
    ) +
    theme_map_sixtysix() %+replace%
    theme(legend.position = config$map_legend_pos)
}

# ggsave(wards_predicted, file = paste0("wards_predicted",Sys.time(),".png"))
# ggsave(turnout_plot, file = paste0("turnout_plot",Sys.time(),".png"))