config <- list(
  city="Chicago",
  city_filename="chicago",
  timezone="America/Chicago",
  election_ds="2019-04-02",
  start_hour=6,
  end_hour=19,  #7pm
  # scale_up = 1.136, ## 12% mail-ins: 1 + 0.12 / 0.88
  mail_in_text = "The tracker only estimates in-person votes. In February, mail-in votes represented 12% of turnout.",
  precinct_shp_path = "data/Precincts (current).shp",
  get_precinct_id = function(df) sprintf("%02d%02d", df$ward, df$precinct),
  get_ward_from_precinct = function(precinct) substr(precinct, 1, 2),
  turnout_df_path="data/turnout_df.csv",
  submission_bitly="http://bit.ly/chicagoturnout",
  google_doc='https://docs.google.com/spreadsheets/d/1W8Y6kC4OctVv2xqLaixo_47-c0EbX8rGY-M6XIPHgIM',
  ref_turnout=c(`February`= 452529, `2015 Runoff`= 592524),
  site_name="Fifty Wards",
  site_link="https://sixtysixwards.com/home/the-turnout-tracker-is-open-sourced-and-going-to-chicago/",
  precinct_name="precinct",
  ward_name="ward",
  use_real_data=FALSE,
  map_legend_pos=c(0.01, 0.01)
)

