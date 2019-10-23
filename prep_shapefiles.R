library(dplyr)
library(sf)
library(sp)

source("util.R")

prep_shapefile <- function(
  shp_path,
  get_precinct_id,
  get_ward_from_precinct,
  save_dir = "data",
  save_precinct = "precincts.Rds",
  save_wards = "wards.Rds"
){
  precincts <- st_read(shp_path)

  precincts$precinct <- get_precinct_id(precincts)
  precincts$ward <- get_ward_from_precinct(precincts$precinct)

  wards <- precincts %>% 
    group_by(ward) %>% 
    summarise(geometry=st_union(geometry))
  wards <- remove_holes(wards)
  
  saveRDS(precincts, file = paste0(save_dir, '/', save_precinct))
  saveRDS(wards, file = paste0(save_dir, '/', save_wards))
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
  ## I don't know why this hack is necessary for Chicago...
  return(st_as_sf(sp[1:nrow(sp),]))
}
  




