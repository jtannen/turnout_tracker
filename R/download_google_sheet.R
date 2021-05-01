library(gsheet)
library(dplyr)
library(lubridate)

asnum <- function(x) as.numeric(as.character(x))

download_google_sheet <- function(
  config,
  filestem="google_download", 
  save_dir="outputs",
  test_data=FALSE
){
  if(test_data){
    raw_url <- config$test_data
  } else {
    raw_url <- config$google_doc
  }
  url <- construct_download_url(raw_url, format = "csv")
  raw_data <- gsheet2tbl(url)
  names(raw_data) <- c("timestamp", "ward", "div", "time_of_day", "obs", "vote_method")
  
  write.csv(raw_data, file=paste0(save_dir,"/", filestem, ".csv"), row.names = FALSE)
  
  raw_data$time <- ymd(config$election_ds, tz=config$timezone) + hms(raw_data$time_of_day)

  if(test_data){
    raw_data$timestamp <- (
      ymd(config$election_ds, tz=config$timezone) 
      + hms(substr(raw_data$timestamp, 11, nchar(raw_data$timestamp)))
    )
  } else {
    raw_data$timestamp <- mdy_hms(raw_data$timestamp, tz=config$timezone) 
  }
  
  raw_data %<>% filter(vote_method == "in person")
  
  raw_data <- raw_data %>% 
    mutate(row_number = 1:n()) %>%
    mutate(
      ward = sprintf("%02d", asnum(ward)),
      div = sprintf("%02d", asnum(div)),
      precinct=paste0(ward,div),
      obs=asnum(obs)
    ) %>%
    select(timestamp, row_number, ward, div, precinct, time, obs)
  
  saveRDS(raw_data, file = paste0(save_dir, "/", filestem,".Rds"))
  return(raw_data)
}

get_mail_count <- function(save_dir){
  raw_data <- read.csv(file=paste0(save_dir,"/google_download.csv"))
  return(sum(raw_data$vote_method == "by mail"))
}
