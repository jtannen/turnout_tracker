config <- list(
  city="Philadelphia",
  city_filename="philadelphia",
  timezone="America/New_York",
  election_ds="2018-11-06",
  start_hour=7,
  end_hour=20,  #8pm
  precinct_shp_path = "data/2016_Ward_Divisions.shp",
  get_precinct_id = function(df) df$WARD_DIVSN,
  get_ward_from_precinct = function(precinct) substr(precinct, 1, 2),
  turnout_df_path="data/phila_turnout.csv",
  submission_bitly="http://bit.ly/sixtysixturnout",
  google_doc='docs.google.com/spreadsheets/d/1GCPVCim0T5Kt4Qveotibx8pDyR2ZPVlCjpUFAMPy9F4',
  ref_turnout=c(`2014`=381503, `2016`=724394),
  site_name="Sixty-Six Wards",
  precinct_name="division",
  ward_name="ward",
  map_legend_pos=c(0.7, 0.1)
)

