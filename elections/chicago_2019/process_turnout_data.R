library(readr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_day_tracker/tracker_v0/chicago_2019/")
source("../util.R")


turnout_files <- grep("chicago_[a-z]+_[0-9]{6}\\.csv", list.files("data"), value = TRUE)

turnout_dfs <- vector(mode = "list", length = length(turnout_files))

for(i in seq_along(turnout_files)){
  file <- turnout_files[i]
  election_type <- gsub("chicago_([a-z]+)_[0-9]{6}\\.csv", "\\1", file)
  date <- gsub("chicago_[a-z]+_([0-9]{4})([0-9]{2})\\.csv", "\\1-\\2", file)
  turnout_dfs[[i]] <- 
    read_csv(
      paste0("data/", turnout_files[i])
    ) %>%
    filter(
      !is.na(as.numeric(as.character(Precinct)))
    ) %>%
    mutate(date=date)
  names(turnout_dfs[[i]]) <- c("ward", "precinct", "registered_voters", "ballots_cast", "turnout_percent", "date")
}

turnout_df <- do.call(rbind, turnout_dfs)

## Only use 2014 and later
turnout_df <- turnout_df %>% filter(date >= "2014-01")
turnout_df <- turnout_df %>%
  mutate(
    precinct = sprintf("%02d%02d", asnum(ward), asnum(precinct)),
    turnout = asnum(ballots_cast),
    year = date
  ) %>% select(precinct, year, turnout)

write.csv(turnout_df, "data/turnout_df.csv", row.names = FALSE)
