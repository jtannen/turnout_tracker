library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(stargazer)

source("theme_sixtysix.R", chdir=TRUE)
source("bootstrap.R", chdir=TRUE) # turnout_plot defined here
source("util.R")

strip_leading_zero <- function(x) gsub("^(0|\\s)+", "", x)
get_subtitle <- function(current_time){
  paste(
    "Results as of", 
    strip_leading_zero(
      format(current_time, "%I:%M")
    )
  )
}

div_to_ward <- function(divid){
  return(substr(divid, 1, 2))
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