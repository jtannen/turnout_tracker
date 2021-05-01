library(dplyr)
library(tibble)
library(tidyr)

load("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data/processed_data/df_major_2019_05_14.Rda")
head(df_major)

turnout <- df_major %>% 
  filter(is_primary_office) %>%
  group_by(WARD19, DIV19, year, election) %>%
  summarise(turnout = sum(VOTES))


turnout <- turnout %>% 
  group_by() %>%
  mutate(
    precinct = paste0(WARD19, DIV19),
    election = paste(year, election)
  ) %>%
  select(precinct, election, turnout)

missing_rows <- tribble(
  ~precinct, ~election,
  '4902', '2009 general',
  '4903', '2009 general',
  '4906', '2009 general',
  '4902', '2009 primary',
  '4903', '2009 primary',
  '4906', '2009 primary',
  '0532', '2018 primary',
  '0533', '2018 primary',
  '0534', '2018 primary',
  '1818', '2018 primary'
) %>% mutate(turnout = 0)

turnout <- bind_rows(turnout, missing_rows)

write.csv(
  turnout, 
  file = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_day_tracker/tracker_v0/phila_201905/data/phila_turnout.csv",
  row.names=FALSE
)

turnout %>% filter(election == "2015 primary") %>% with(sum(turnout))
