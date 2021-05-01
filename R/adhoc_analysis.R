library(tidyverse)
source("../../../admin_scripts/util.R")
df <- read_csv("elections/phila_202006/outputs/google_download.csv") %>%
  mutate(ward = sprintf("%02d", asnum(ward)))

df_major <- readRDS("../../../data/processed_data/df_major_20191203.Rds")

turnout_12 <- df_major %>%
  filter(is_topline_office, election=="primary", year==2012) %>%
  group_by(warddiv) %>%
  summarise(votes = sum(votes))

mail_in <- read_csv("../../../../../../../Downloads/2020_Primary_Election_Mail_Ballot_Requests_Department_of_State (2).csv")

divs <- st_read("data/Political_Divisions.shp")
wards <- readRDS("data/wards.Rds")

polling_places <- read_csv("../../../../../data/miscellaneous/2020_Primary_Zones_and_Polling_Places_by_Division.csv")

turnout_places <- turnout_12 %>%
  mutate(warddiv=pretty_div(warddiv)) %>%
  left_join(polling_places, by=c("warddiv" = "Division")) %>%
  group_by(Zone, `Polling Place`, Address) %>% 
  summarise(
    turnout = sum(votes)
  ) 

ggplot(
  wards %>% left_join(df %>% group_by(ward) %>% count())
) +
  geom_sf(aes(fill = n), color="grey70") +
  scale_fill_viridis_c("submissions") +
  theme_map_sixtysix() +
  ggtitle("Submissions as of 8pm")
ggsave("submissions_800.png")

df %>% 
  filter(vote_method == "in person") %>%
  mutate(warddiv = sprintf("%02d%02d", asnum(ward), asnum(div))) %>%
  left_join(turnout_12)
  

df_places <- df %>% 
  filter(vote_method == "in person") %>%
  mutate(warddiv = sprintf("%02d-%02d", asnum(ward), asnum(div))) %>%
  left_join(polling_places, by=c("warddiv" = "Division")) %>%
  left_join(turnout_places)

ggplot(df_places, aes(x=time_of_day, y=obs / turnout)) + 
  geom_point() +
  theme_sixtysix() +
  labs(
    title="Raw Submissions, 7:00 PM",
    x="Time of Day",
    y="Voters / Turnout 2012"
  )
setwd("elections/phila_202006/")
ggsave("outputs/obs_7pm.png")

sth <- st_read("../../../../../data/gis/state_house/House2012Final.shp") %>%
  st_transform(st_crs(divs))

divs_to_sth <- st_covered_by(st_centroid(divs), sth) 
divs$sth <- as.character(sth$District_N)[unlist(divs_to_sth)]

ggplot(divs) + geom_sf(aes(fill=sth), color=NA) + scale_fill_viridis_d()


mail_sth <- mail_in %>% 
  rename(sth = `State House District`) %>%
  # filter(county == "PHILADELPHIA") %>%
  group_by(sth) %>%
  summarise(
    requests = n(),
    returned = sum(!is.na(`Ballot Returned Date`))
  ) %>%
  mutate(sth = gsub("^([0-9]+)[A-Z]+\\s+LEGISLATIVE DISTRICT$", "\\1", sth))

sth %<>% rename(sth = `District_N`) %>% left_join(mail_sth, by="sth")

library(leaflet)
make_leaflet <- function(
  sf, 
  color_col, 
  title,
  is_percent=FALSE,
  pal="viridis",
  zoom=11
){
  min_value <- min(sf[[color_col]], na.rm=TRUE)
  max_value <- max(sf[[color_col]], na.rm=TRUE)
  
  sigfig <- round(log10(max_value-min_value)) - 1
  step_size <- 2 * 10^(sigfig)
  
  min_value <- step_size * (min_value %/% step_size)
  max_value <- step_size * (max_value %/% step_size + 1)
  
  color_numeric <- colorNumeric(
    pal, 
    domain=c(min_value, max_value)
  )
  
  legend_values <- seq(min_value, max_value, step_size)
  legend_colors <- color_numeric(legend_values)
  if(is_percent){
    legend_labels <- sprintf("%s%%", legend_values)
  } else{
    legend_labels <- scales::comma(legend_values)
  }
  
  bbox <- st_bbox(sf)
  
  leaflet(
    options=leafletOptions(
      # minZoom=zoom, 
      # maxZoom=zoom,
      zoomControl=TRUE
      # dragging=FALSE
    )
  )%>% 
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(
      data=sf$geometry,
      weight=0, 
      color="white", 
      opacity=1, 
      fillOpacity = 0.8, 
      smoothFactor = 0,
      fillColor = color_numeric(sf[[color_col]]),
      popup=sf$popup
    ) %>%
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

sth %<>% mutate(
  popup = sprintf(
    paste(
      c(
        "<b>State House %s</b>",
        "Mail-In Requests: %s",
        "Returned as of 6/1: %s"
        # "Votes in the 2019 Primary: %s",
        # "Votes in the 2016 Primary: %s"
      ),
      collapse = "<br>"
    ),
    sth,
    scales::comma(requests),
    scales::comma(returned)
  )
)

lf_returned <- make_leaflet(sth, "returned", title="Mail-In Ballots Returned")  
htmlwidgets::saveWidget(lf_returned, file="lflt_returned.html", selfcontained = TRUE)

mail_in %>% 
  filter(`County Name` == "PHILADELPHIA") %>%
  summarise(
    requests = n(),
    returned = sum(!is.na(`Ballot Returned Date`))
  )
  
mail_in %>% 
  filter(`County Name` == "PHILADELPHIA", !is.na(`Ballot Returned Date`)) %>%
 with(table(`Ballot Returned Date`)) %>% sort

