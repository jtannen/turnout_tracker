library(dplyr)
library(tibble)
library(tidyr)

df_major <- readRDS("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data/processed_data/df_major_2019_09_19.Rds")
head(df_major)

turnout <- df_major %>% 
  filter(is_primary_office) %>%
  group_by(warddiv, year, election) %>%
  summarise(turnout = sum(votes))


turnout <- turnout %>% 
  group_by() %>%
  mutate(
    election = paste(year, election)
  ) %>%
  rename(precinct=warddiv) %>%
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
) %>% mutate(turnout=NA)

groups <- missing_rows %>% mutate(ward = substr(precinct, 1, 2)) %>% select(ward, election) %>% unique()


## Add votes to the missing precincts so they are non-zero
## Subtract votes from the rest of the ward to preserve turnout
for(i in 1:nrow(groups)){
  ward_i <- groups$ward[i]
  election_i <- groups$election[i]
  use_rows <- which(substr(missing_rows$precinct, 1, 2) == ward_i & missing_rows$election == election_i)
  precincts <- missing_rows$precinct[use_rows]
  
  df_ward <- turnout %>% 
    filter(
      substr(precinct, 1, 2)==ward_i
    )
  
  year_turnout <- df_ward %>% group_by(election) %>% summarise(turnout=sum(turnout))
    
  df_lm <- df_ward %>% 
    filter(election != election_i)
  
  fit <- lm(log(turnout) ~ election + precinct, data=df_lm)
  
  ## estimate the fixed effect from the observed divisions
  fe <- df_ward %>% 
    filter(election == election_i) %>%
    mutate(
      effect = coef(fit)[paste0("precinct", precinct)],
      effect=ifelse(is.na(effect), 0, effect),
      resid=log(turnout)-effect-coef(fit)["(Intercept)"]
    ) %>% with(mean(resid))
  
  div_turnout <- exp(coef(fit)[paste0("precinct", precincts)] + fe + coef(fit)["(Intercept)"])
  other_div_rows <- which(substr(turnout$precinct, 1, 2) == ward_i & turnout$election == election_i)
  ward_turnout <- turnout[
    other_div_rows,
    "turnout"
  ]
  
  prop <- sum(ward_turnout) / (sum(div_turnout) + sum(ward_turnout))
  missing_rows$turnout[use_rows] <- prop*div_turnout
  turnout$turnout[other_div_rows] <- prop*turnout$turnout[other_div_rows]
  turnout <- bind_rows(turnout, missing_rows[use_rows,])
}

# turnout <- bind_rows(turnout, missing_rows)

write.csv(
  turnout, 
  file = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/phila_201911/data/phila_turnout.csv",
  row.names=FALSE
)

turnout %>% filter(election == "2019 primary") %>% with(sum(turnout))
