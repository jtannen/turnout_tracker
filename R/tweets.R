library(rtweet)

appname <- "SixtySixWards Tweeter"

# not published to git
source("generate_twitter_token.R")


setClass(
  "Tweet",
  slots=list(
    time="character",
    id="numeric"
  )
)

Tweet <- function(time, id) new("Tweet", time=time, id=id)

get_status <- function(turnout_ci, current_time, config){
  sprintf(
    "As of %s, the Turnout Tracker estimates that %s Philadelphians have voted (95%% CI: %s, %s).\n\n https://bit.ly/turnouttracker",
    strip_leading_zero(format(current_time, "%I:%M")),
    scales::comma(round(turnout_ci$turnout_500)),
    scales::comma(round(turnout_ci$turnout_025)),
    scales::comma(round(turnout_ci$turnout_975))
  )
}

tweet_update <- function(reply_tweet_id, turnout_ci, current_time, config, media){
  if(is.na(reply_tweet_id)) stop("reply_tweet_id is NA")
  status <- get_status(turnout_ci, current_time, config)
  post_tweet(
    status,
    media=media,
    in_reply_to_status_id=reply_tweet_id
  )
  my_timeline <- get_timeline(rtweet:::home_user())
  last_id <- my_timeline$status_id[1]
  return(last_id)
}

tweet_if_time <- function(
  last_tweet, 
  current_time,
  turnout_ci,
  config,
  plot_files,
  minute_delay=20
){
  if(!is.na(last_tweet@time)){
    is_time_to_tweet <-
      (ymd_hms(current_time) - ymd_hms(time_of_last_tweet)) >= minutes(minute_delay)
  } else {
    is_time_to_tweet <- TRUE
  }

  if(is_time_to_tweet){
    reply_tweet_id <- tweet_update(
      last_tweet@id,
      turnout_ci,
      current_time,
      config,
      plot_files
    )
    return(Tweet(time=current_time, id=reply_tweet_id))
  } else {
    print(sprintf(
      "Not Tweeting: current time: %s, last tweet: %s", 
      current_time, 
      last_tweet_@time
    ))
    return(last_tweet)
  }
}