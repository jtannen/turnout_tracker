setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/election_configs/phila_202011/")

library(dplyr)
library(stargazer)

source("config.R")
source("../../R/util.R", chdir=TRUE)
source("../../R/generate_plots.R", chdir=TRUE)
source("../../R/bootstrap.R", chdir=TRUE)

params <- readRDS("outputs/params.Rds")
bs <- readRDS("outputs/bootstrap.Rds")

get_ward <- config$get_ward_from_precinct

turnout_df <- get_ci_from_bs(bs, predict_topline, keys="time_of_day")

ward_turnout <- get_ci_from_bs(
  bs, 
  predict_ward_turnout, 
  get_ward=config$get_ward_from_precinct,
  keys="ward"
) %>%
  rename(proj_inperson = turnout)

true_turnout <- read_csv("../../../../election_night_2020/data/") %>%
  rename(
    ward=Division,
    true_inperson=PollingPlace
  )

turnout_16 <- read_csv("data/phila_turnout.csv") %>% filter(election=="2016 general")
raw_data <- readRDS("outputs/raw_data.Rds") %>% 
  mutate(
    time_of_day=extend_config(config)$base_time + minutes(minute)
  ) %>%
  left_join(turnout_16 %>% rename(turnout_16=turnout)) %>%
  left_join(turnout_df %>% mutate(time_trend = turnout / max(turnout)) %>% select(time_of_day, time_trend))

wards <- readRDS("data/wards.Rds")
wards %<>% left_join(ward_turnout) %>% 
  left_join(true_turnout %>% select(ward,true_inperson)) %>%
  left_join(turnout_16 %>% mutate(ward=substr(precinct,1,2)) %>% group_by(ward) %>% summarise(turnout_16=sum(turnout)))

ggplot(
  wards %>% 
    select(ward, geometry, true_inperson, proj_inperson, turnout_16) %>%
    pivot_longer(cols=c(true_inperson, proj_inperson)) %>%
    st_as_sf()
) + 
  geom_sf(aes(fill = value/turnout_16)) +
  scale_fill_viridis_c() +
  facet_grid(~name) +
  theme_map_sixtysix() %+replace% 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.justification="center")

scale_max <- with(wards, max(abs(100*(true_inperson - proj_inperson) / true_inperson)))
ggplot(wards) + 
  geom_sf(aes(fill = 100*(true_inperson - proj_inperson) / true_inperson)) +
  scale_fill_gradient2(low = strong_orange, mid="grey80", high=strong_purple, midpoint=0)+
  expand_limits(fill=c(-scale_max, scale_max)) +
  theme_map_sixtysix()

params@precinct_cov

is07 <- substr(params@precinct_fe$precinct, 1, 2) == "07"
# is07 <- params@precinct_fe$precinct == "2714"
sigma_12 <- params@precinct_cov[is07, !is07]
sigma_22 <- params@precinct_cov[!is07, !is07]

conditional_cov <- colSums(sigma_12 %*% solve(sigma_22))

divs <- readRDS("data/precincts.Rds")
conditional_cov_df <- data.frame(
  cond = conditional_cov,
  warddiv = params@precinct_fe$precinct[!is07]
)

divs %<>% left_join(conditional_cov_df, by=c("precinct"="warddiv"))

ggplot(divs) +
  geom_sf(aes(fill=cond), color=NA) +
  scale_fill_viridis_c() 

divs <- readRDS("data/precincts.Rds")

get_conditional_df <- function(ward){
  isward <- substr(params@precinct_fe$precinct, 1, 2) == ward
  # is07 <- params@precinct_fe$precinct == "2714"
  sigma_12 <- params@precinct_cov[isward, !isward]
  sigma_22 <- params@precinct_cov[!isward, !isward]
  
  conditional_cov <- colSums(sigma_12 %*% solve(sigma_22))
  data.frame(
    cond = conditional_cov,
    warddiv = params@precinct_fe$precinct[!isward]
  )
}

map_dependency <- function(ward){
  conditional_cov_df <- get_conditional_df(ward)  
  divs %<>% left_join(conditional_cov_df, by=c("precinct"="warddiv"))
  
  ggplot(divs) +
    geom_sf(aes(fill=cond), color=NA) +
    scale_fill_gradient2(
      na.value="black", 
      low=strong_orange, 
      mid="grey90", 
      high=strong_purple
    ) +
    theme_map_sixtysix()
}

map_dependency("66")

fe_2016 <- params@election_fe %>% filter(election == "2016 general") %>% with(election_fe)

raw_wards <- raw_data %>%
  left_join(params@precinct_fe) %>%
  group_by(ward) %>%
  summarise(
    mean_raw = exp(mean(log_obs - fe_2016 - precinct_fe - log(time_trend))),
    mean_raw_16 = exp(mean(log_obs - log(turnout_16) - log(time_trend)))
  )

ggplot(wards %>% left_join(raw_wards)) + 
  geom_sf(aes(fill=mean_raw)) +
  scale_fill_viridis_c(na.value=rgb(1,1,1,alpha=0)) +
  theme_map_sixtysix()

ggplot(wards %>% left_join(raw_wards)) + 
  geom_sf(aes(fill=mean_raw_16)) +
  scale_fill_viridis_c(na.value=rgb(1,1,1,alpha=0)) +
  theme_map_sixtysix()

raw_divs <- raw_data %>%
  group_by(precinct) %>%
  summarise(
    mean_raw = exp(mean(log_obs - log(turnout_16) - log(time_trend)))
  )

cond66 <- get_conditional_df("66")

raw_cond_66 <- raw_divs %>% 
  left_join(cond66, by=c("precinct"="warddiv"))

ggplot(divs %>% inner_join(raw_cond_66)) + 
  geom_sf(aes(fill=mean_raw * cond), color=NA) +
  # scale_fill_viridis_c(na.value=rgb(1,1,1,alpha=0)) +
  scale_fill_gradient2()+
  theme_map_sixtysix()


source("../../R/precalc_params.R", chdir=TRUE)
diagnostics(params, divs, config=config)
