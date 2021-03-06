library(tidyverse)
source("../../../../../admin_scripts/util.R")

folder <- "phila_201911"

setwd(sprintf("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/%s/outputs/"))
raw <- readRDS("raw_data.Rds")
turnout <- read.csv("../data/phila_turnout.csv") %>%
  mutate(precinct = sprintf("%04i", precinct)) %>%
  filter(election == "2015 general")

bs <- readRDS("backup/bootstrap_2019_11_05_12_58_44.Rds")

attr(bs$single_result, "precinct_df") %>% arrange(desc(re_fit))

generate_raw_plot <- function(raw_data, turnout_ref, ref_year){
  ggplot(
    raw %>%
      left_join(turnout),
    aes(x = minute, y=obs/turnout)
  ) +
    geom_point() +
    theme_sixtysix() +
    labs(
      title="Raw Responses normalized by 2016 turnout",
      y=sprintf("Reported Number divided by %s turnout", ref_year)
    ) +
    geom_hline(yintercept=1)
}

generate_raw_plot(raw_data, turnout, 2015)
ggsave(file="sanity_check.png")


raw %>%
  left_join(turnout) %>%
  filter(obs / turnout > 5)

raw %>%
  left_join(turnout) %>%
  filter(obs / turnout < 0.1, minute > 500)

