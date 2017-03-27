#install.packages("maptools")
#install.packages("testthat")
library(maptools)
library(data.table)
library(testthat)
library(dplyr)
library(shiny)

#### model
# write a series of functions that correspond to the
# transformation boxes on the flowchart (green rectangles,
# https://docs.google.com/drawings/d/1SzfSVf9lA7B8NrprYyWyAurJUpAbXZujMLMTiKW5gYk/edit)
# then call the functions together at the end to run the model
# geography is user defined (LA or Ward)

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

calculate_demand_by_geography <- function(geography, GM_Primary_school_population_by_area_and_age){
  
  #could do this using aggregate function as below, but have used dplyr instead
  #aggregated_population <- aggregate(GM_Primary_school_population_by_area_and_age$Population, by=list(Geography=GM_Primary_school_population_by_area_and_age$LA), FUN=sum)
  #aggregated_population <- setnames(aggregated_population,"x","Demand")
  if (geography == "ward") {
    aggregated_population <- GM_Primary_school_population_by_area_and_age %>% 
      group_by(Ward.Code, Ward.Name, Local.Authority) %>%
      summarise(demand=sum(Population))
  } else {
    if (geography == "LA") {
      aggregated_population <- GM_Primary_school_population_by_area_and_age %>% 
        group_by(LA) %>%
        summarise(demand=sum(Population))
    }
  }
}

#### user parameters
geography = "LA"

### run the model
GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
#View(GM_boundaries_by_geography@data)
#plot(GM_boundaries_by_geography)
GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
GM_school_capacity_by_geography <- calculate_capacity_by_geography(geography)
primary_school_demand_by_geography <- calculate_demand_by_geography(geography, GM_Primary_school_population_by_area_and_age)


## app

ui <- fluidPage(
  headerPanel('Where are primary state schools in relation to school age children in Greater Manchester?'),
  sidebarPanel(
    selectInput('geography', 'Please select geography', c('LA', 'ward'))),
  mainPanel(
    plotOutput("map")))

server <- function(input, output) {
  output$map <- renderPlot({
    plot(select_boundaries_by_geography(input$geography))}) ### Needs to take 'joined' dataset
}

shinyApp(ui = ui, server = server)

#Tests

(results <- test_that("Test that the number of wards in demand set is the number in GM (=215)",
                      {geography = "ward"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      primary_school_demand_by_geography <- calculate_demand_by_geography(geography, GM_Primary_school_population_by_area_and_age)
                        count_wards <- length (unique(primary_school_demand_by_geography$Ward.Code))
                      expect_equal(count_wards, 215)
                      }))

(results <- test_that("Test that the number of pupils per LA is the same in the LA data set and the ward data set",
                      {# set geography to LA, make test dataframe (rename of primary_school_demand_by_geography)
                      geography = "LA"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      test_demand_from_LA_data <- calculate_demand_by_geography(geography, GM_Primary_school_population_by_area_and_age)
                      # set geography to ward, make primary_school_demand_by_geography, sum by LA to make test dataframe, rename columns
                      geography = "ward"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      primary_school_demand_by_geography <- calculate_demand_by_geography(geography, GM_Primary_school_population_by_area_and_age)

                      test_demand_from_ward_data <- primary_school_demand_by_geography %>% 
                        group_by(Local.Authority) %>%
                        summarise(demand=sum(demand)) %>%
                        setnames("Local.Authority", "LA")
                      # compare test dataframes, should be the same
                      expect_equal(test_demand_from_LA_data, test_demand_from_ward_data)
                      }))

(results <- test_that("Test that the number of wards is 215",
                      {number_wards <-nrow(calculate_capacity_by_geography(geography = "ward"))
                      expect_equal(number_wards, 215)
                      }))
