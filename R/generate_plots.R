library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(leaflet)
# library(stargazer)

source("theme_sixtysix.R", chdir=TRUE)
source("bootstrap.R", chdir=TRUE)
source("util.R")

get_subtitle <- function(current_time){
  sprintf(
    "Results as of %s", 
    pretty_time(current_time)
  )
}

div_to_ward <- function(divid){
  return(substr(divid, 1, 2))
}

turnout_plot <- function(
  bs,
  raw_data,
  election_config,
  mail_in=0
){
  config <- extend_config(election_config)
  ref_turnout <- config$ref_turnout
  
  ci_df <- get_ci_from_bs(bs, predict_topline, keys="time_of_day", eod=FALSE)
  
  winsorize <- function(x, t = 0.95){
    mean_x <- mean(x, na.rm=TRUE)
    x_demean <- x - mean_x
    cutoff <- quantile(abs(x_demean), probs=t, na.rm=TRUE)
    return(
      mean_x + sign(x_demean) * pmin(abs(x_demean), cutoff)
    )
  }
  
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
    aes(x=time_of_day, y=turnout + mail_in)
  ) +
    geom_point(data=resid_data, aes(y=turnout * exp(resid) + mail_in)) +
    geom_ribbon(
      aes(ymin = p025 + mail_in, ymax = pmin(p975, 800e6) + mail_in),
      alpha = 0.2,
      color = NA,
      fill = strong_purple
    ) +
    geom_line(size = 2, color = strong_purple) +
    scale_x_datetime("", date_labels = "%I", date_breaks = '1 hour') +
    scale_y_continuous("", labels = scales::comma) +
    geom_hline(yintercept = ref_turnout) + 
    geom_text(
      data = data.frame( 
        turnout = ref_turnout,
        time_of_day = rep(config$base_time + minutes(30), length(ref_turnout)),
        label = sprintf("%s turnout = %s", names(ref_turnout), scales::comma(ref_turnout))
      ),
      aes(label=label, y=turnout),
      vjust = 1.2,
      hjust = 0
    ) +
    geom_hline(yintercept = mail_in, color="grey50") + 
    geom_text(
      data = data.frame( 
        turnout = mail_in,
        time_of_day = rep(config$base_time + minutes(30), length(ref_turnout)),
        label = sprintf("%s mail-in ballots returned*", scales::comma(mail_in))
      ),
      aes(label = label, y=turnout),
      vjust = 1.2,
      hjust = 0
    ) +
    expand_limits(x = config$election_day + hours(config$end_hour), y=0) + 
    ggtitle("Estimated Election Turnout") +
    theme_sixtysix() 
}

guess_election_type <- function(ds){
  month <- asnum(substr(ds, 6, 7))
  if(month == 11){
    return("general")
  } else if(month %in% 4:6){
    return("primary")
  } else stop(sprintf("Don't know election_type of month %s", month))
}

load_ward_turnout <- function(config, lag=4){
  prior_turnout <- read_csv(
    add_election_path(config$turnout_df_path)
  )
  
  year <- asnum(substr(config$election_ds, 1, 4))
  election_type <- guess_election_type(config$election_ds)
  election <- sprintf("%s %s", year-lag, election_type)
  
  ward_turnout <- prior_turnout %>%
    filter(election == !!election) %>%
    mutate(ward = config$get_ward_from_precinct(precinct)) %>%
    group_by(ward) %>%
    summarise(turnout = sum(turnout))
  
  return(ward_turnout)
}

gg_wards_relative <- function(wards, current_time, config){
  lagged_turnout <- load_ward_turnout(config, lag=4) %>%
    rename(turnout.lag = turnout)
  
  wards <- wards %>% left_join(lagged_turnout, by='ward')
    
  ggplot(wards) +
    geom_sf(aes(fill = turnout / turnout.lag), color = NA)+
    scale_fill_viridis_c(
      sprintf("Current Turnout /\n Final Turnout %s", substr(current_time, 1, 4))
    ) +
    ggtitle(
      "Turnout vs four years ago",
        get_subtitle(current_time)
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


make_leaflet <- function(
  wards, 
  raw_data,
  mail_in,
  config,
  get_color, 
  title,
  is_percent=FALSE,
  pal="viridis"
){
  lagged_turnout <- load_ward_turnout(config, lag=4) %>%
    rename(turnout.lag = turnout)
  
  submissions <- raw_data %>% group_by(ward) %>% count(name="submissions")
  
  wards %<>% 
    left_join(lagged_turnout) %>%
    left_join(submissions) %>%
    left_join(mail_in) %>%
    mutate(
      popup = sprintf(
        paste(
          c(
            "<b>Ward %s</b>",
            "Current in-person turnout: %s",
            "Mail in ballots requested: %s",
            "Mail in ballots returned: %s",
            "Turnout in %s: %s",
            "Submissions: %s"
          ),
          collapse = "<br>"
        ),
        asnum(ward),
        scales::comma(round(turnout)),
        scales::comma(round(ballots)),
        scales::comma(round(returned)),
        asnum(substr(config$election_ds, 1, 4)) - 4,
        scales::comma(round(turnout.lag)),
        # round(100 * (turnout + returned) / turnout.lag),
        replace_na(submissions, 0)
      )
    )
  
  zoom <- 11

  wards$color <- get_color(wards)  
  
  min_value <- min(wards$color, na.rm=TRUE)
  max_value <- max(wards$color, na.rm=TRUE)
  
  sigfig <- round(log10(max_value-min_value)) - 1
  step_size <- 2 * 10^(sigfig)
  
  min_value <- step_size * (min_value %/% step_size)
  max_value <- step_size * (max_value %/% step_size + 1)
  
  pal <- colorNumeric(
    pal, 
    domain=c(min_value, max_value)
  )
  
  legend_values <- seq(min_value, max_value, step_size)
  legend_colors <- pal(legend_values)
  if(is_percent){
    legend_labels <- sprintf("%s%%", legend_values)
  } else{
    legend_labels <- scales::comma(legend_values)
  }
  
  bbox <- st_bbox(wards)
  
  leaflet(
    options=leafletOptions(
      minZoom=zoom, 
      maxZoom=zoom,
      zoomControl=FALSE,
      dragging=FALSE
      )
  )%>% 
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(
      data=wards$geometry,
      weight=0, 
      color="white", 
      opacity=1, 
      fillOpacity = 0.8, 
      smoothFactor = 0,
      fillColor = pal(wards$color),
      popup=wards$popup
    ) %>%
    # setView(
    #   lng=mean(bbox[c('xmin', 'xmax')]),
    #   lat=mean(bbox[c('ymin', 'ymax')]), 
    #   zoom=zoom
    # ) %>%
    setMaxBounds(
      lng1=bbox["xmin"],
      lng2=bbox["xmax"],
      lat1=bbox["ymin"],
      lat2=bbox["ymax"]
    ) %>%
    addControl(title, position="topright", layerId="map_title") %>% 
    addLegend(
      layerId="geom_legend",
      position="bottomright",
      colors=legend_colors,
      labels=legend_labels
    )
}

leaflet_wards_predicted <- function(wards, raw_data, mail_in, config, current_time){
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) wards$turnout,
    title=sprintf("Turnout, as of %s", pretty_time(current_time))
  )
}

leaflet_wards_total <- function(wards, raw_data, mail_in, config, current_time){
  make_leaflet(
    wards=wards %>% left_join(mail_in), 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) wards$turnout + wards$returned,
    title=sprintf("Turnout (in-person + mail-in), as of %s", pretty_time(current_time))
  )
}


leaflet_wards_change <- function(wards, raw_data, mail_in, config, current_time){
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) 100 * wards$turnout / wards$turnout.lag,
    title=sprintf("Turnout vs %s, as of %s", substr(config$election_ds, 1, 4), pretty_time(current_time)),
    is_percent=TRUE
  )
}

leaflet_submissions <- function(wards, raw_data, mail_in, config, current_time){
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) wards$submissions,
    title=sprintf("Number of submissions, as of %s", pretty_time(current_time)),
    pal="plasma"
  )
}

leaflet_mail_in_reqs <- function(wards, raw_data, mail_in, config, current_time){
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) wards$ballots,
    title="Mail In Requests",
    pal="plasma"
  )
}

leaflet_mail_in_returned <- function(wards, raw_data, mail_in, config, current_time){
  if(sum(mail_in$returned) == 0) return("")
  
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) wards$returned,
    title="Mail-In Ballots Returned so far",
    pal="plasma"
  )
}


leaflet_change <- function(wards, raw_data, mail_in, config, current_time){
  if(sum(mail_in$returned) == 0) return("")
  
  make_leaflet(
    wards=wards, 
    raw_data=raw_data,
    mail_in, 
    config=config, 
    get_color=function(wards) 100*(wards$returned + wards$turnout) / wards$turnout.lag ,
    title="Total current turnout / 2016 turnout (as %)",
    pal="plasma"
  )
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


generate_raw_plot <- function(raw_data, turnout_df, ref_year){
  
  winsorize <- function(x, t = 0.95){
    mean_x <- mean(x, na.rm=TRUE)
    x_demean <- x - mean_x
    cutoff <- quantile(abs(x_demean), probs=t, na.rm=TRUE)
    return(
      mean_x + sign(x_demean) * pmin(abs(x_demean), cutoff)
    )
  }
  
  ggdf <- raw_data %>% left_join(turnout_df) %>%
    mutate(y = winsorize(obs/turnout, t=0.99))
  ggplot(
    ggdf,
    aes(x = minute/60, y=y)
  ) +
    geom_point() +
    theme_sixtysix() +
    labs(
      title=sprintf("Raw responses normalized by %i turnout", ref_year),
      y=sprintf("Reported number divided by %s turnout", ref_year),
      x="Hour"
    ) +
    geom_hline(yintercept=1) +
    expand_limits(x=13)
}

# ggsave(wards_predicted, file = paste0("wards_predicted",Sys.time(),".png"))
# ggsave(turnout_plot, file = paste0("turnout_plot",Sys.time(),".png"))