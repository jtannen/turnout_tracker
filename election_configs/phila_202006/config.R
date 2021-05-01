config <- list(
  city="Philadelphia",
  city_filename="philadelphia",
  timezone="America/New_York",
  election_ds="2020-06-02",
  start_hour=7,
  end_hour=20,  #8pm
  precinct_shp_path = "data/Political_Divisions.shp",
  get_precinct_id = function(df) df$DIVISION_N,
  get_year_from_election = function(election) as.numeric(substr(election, 1, 4)),
  get_etype_from_election = function(election) substr(election,6, nchar(as.character(election))),
  get_ward_from_precinct = function(precinct) substr(precinct, 1, 2),
  turnout_df_path="data/phila_turnout.csv",
  submission_bitly="http://bit.ly/sixtysixturnout",
  google_doc='docs.google.com/spreadsheets/d/1GCPVCim0T5Kt4Qveotibx8pDyR2ZPVlCjpUFAMPy9F4',
  ## test data with 199K turnout
  test_data='docs.google.com/spreadsheets/d/11g0yRMF6VQqfZCHOC2S2jL62FgdiKVMq7KzUkcPhy1w',
  ref_turnout=c(`2016`=238664, `2019`=200834),
  site_name="Sixty-Six Wards",
  site_link="https://sixtysixwards.com/",
  precinct_name="division",
  ward_name="ward",
  map_legend_pos=c(0.7, 0.1)
)

