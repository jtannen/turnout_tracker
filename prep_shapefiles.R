library(dplyr)
library(sf)
library(sp)

source("util.R")

prep_shapefile <- function(
  shp_path,
  precinct_id_col,
  get_ward_from_precinct,
  save_dir = "data",
  save_precinct = "precincts.Rda",
  save_wards = "wards.Rda"
){
  precincts <- st_read(shp_path)

  precincts$precinct <- precincts[[precinct_id_col]]
  precincts$ward <- get_ward_from_precinct(precincts$precinct)

  wards <- precincts %>% 
    group_by(ward) %>% 
    summarise(geometry=st_union(geometry))
  wards <- remove_holes(wards)
  
  save(precincts, file = paste0(save_dir, '/', save_precinct))
  save(wards, file = paste0(save_dir, '/', save_wards))
}


remove_holes <- function(shp){
  sp <- as(shp, "Spatial")
  sp@polygons <- sapply(
    sp@polygons, 
    function(p){
      p_Polygons <- list()
      for(P in p@Polygons){
        if(!P@hole){
          p_Polygons <- c(p_Polygons, list(P))
        }
      }
      p@Polygons <- p_Polygons
      return(p)
    }
  )
  
  return(st_as_sf(sp))
}
  




