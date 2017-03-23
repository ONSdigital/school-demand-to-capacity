#install.packages("maptools")
library(maptools)
library(data.table)

#### user parameters
geography = "LA"


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

calculate_capacity_by_geography <- function(geography){
  capacity <- read.csv("data/clean/GM_schools_dataset.csv", stringsAsFactors = F)
  if (geography == "ward") {
    aggregated_capacity <- aggregate(capacity$Netcapacity..1, by=list(Geography=capacity$Ward.Code), FUN=sum)
    aggregated_capacity <- setnames(aggregated_capacity, "x", "aggregated net capacity")
  } else {
    if (geography == "LA") {
      aggregated_capacity <- aggregate(capacity$Netcapacity..1, by=list(Geography=capacity$Local.Authority.Code), FUN=sum)
      aggregated_capacity <- setnames(aggregated_capacity, "x", "aggregated net capacity")
    }
  }
  
  return(aggregated_capacity)
}


### run the model
GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
#View(boundaries@data)
#plot(boundaries)
GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)

GM_school_capacity_by_geography <- calculate_capacity_by_geography(geography)



