library(gsheet)
library(dplyr)
library(lubridate)


download_google_sheet <- function(config, filestem="google_download", save_dir="outputs"){
  raw_url <- config$google_doc
  url <- construct_download_url(raw_url, format = "csv")
  raw_data <- gsheet2tbl(url)
  write.csv(raw_data, file=paste0(save_dir,"/google_download.csv"), row.names = FALSE)
  
  names(raw_data) <- c("timestamp", "ward", "div", "time_of_day", "obs")

  raw_data$time <- ymd(config$election_ds, tz=config$timezone) + hms(raw_data$time_of_day)
  
  raw_data <- raw_data %>% 
    filter(
      time <= mdy_hms(timestamp, tz=config$timezone)
    )
  
  # raw_data <- raw_data %>% 
  #   filter(
  #     mdy_hms(time_of_day, tz=config$timezone) <= (
  #       ymd(config$election_ds, tz=config$timezone) + hours(config$end_hour)
  #     )
  #   )
  
  raw_data <- raw_data %>% 
    mutate(row_number = 1:n()) %>%
    mutate(
      ward = sprintf("%02d", ward),
      div = sprintf("%02d", div),
      precinct=paste0(ward,div)
    ) %>%
    select(timestamp, row_number, ward, div, precinct, time, obs)
  
  save(raw_data, file = paste0(save_dir, "/", filestem,".Rda"))
  return(raw_data)
}
