#install.packages("maptools")
library(maptools)

#### user parameters
geography = "ward"


#### model
# write a series of functions that correspond to the
# transformation boxes on the flowchart (green rectangles,
# https://docs.google.com/drawings/d/1SzfSVf9lA7B8NrprYyWyAurJUpAbXZujMLMTiKW5gYk/edit)
# then call the functions together at the end to run the model

### functions
select_boundaries_by_geography <- function(geography){
  if (geography == "ward") {
    boundaries <- readShapePoly("data/clean/Ward_shapefile_GM/GM_wards.shp")
  } else {
    if (geography == "LA") {
      boundaries <- readShapePoly("data/clean/LA_shapefile_GM/GM_LA.shp")
    }
  }
}

select_primary_school_population_by_geography <- function(geography){
  if (geography == "ward") {
    population <- read.csv("data/clean/ward_population.csv", stringsAsFactors = F)
  } else {
    if (geography == "LA") {
      population <- read.csv("data/clean/LA_Population.csv", stringsAsFactors = F)
    }
  }
}

### run the model
GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
#View(boundaries@data)
#plot(boundaries)
GM_Primary_school_population_by_area_and_age <- select_population_by_geography(geography)


