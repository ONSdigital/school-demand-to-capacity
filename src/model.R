#install.packages("maptools")
library(maptools)

setwd("/Users/sunnyt/Google Drive/mc-root/ONS accelerator projects/MC-ONS Shared Folder/Project 1")


#### user parameters
geography = "ward"


#### model
# write a series of functions that correspond to the
# transformation boxes on the flowchart (green rectangles,
# https://docs.google.com/drawings/d/1SzfSVf9lA7B8NrprYyWyAurJUpAbXZujMLMTiKW5gYk/edit)
# then call the functions together at the end to run the model

### functions
filter_boundaries_by_geography <- function(geography){
  if (geography == "ward") {
    boundaries <- readShapePoly("Data/Generated datasets/Ward shapefile - GM/GM_wards.shp")
  } else {
    boundaries <- "NA"
  }
  return(boundaries)
}


### run the model
wards <- filter_boundaries_by_geography(geography)

