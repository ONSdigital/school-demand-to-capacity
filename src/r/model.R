#install.packages("maptools")
#install.packages("data.table")
#install.packages("testthat")
#install.packages("dplyr")
#install.packages("shiny")
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
    boundaries <- readShapePoly("data/clean/Ward_shapefile_GM/GM_Ward.shp")
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
    aggregated_capacity <- setnames(aggregated_capacity, "x", "aggregated.net.capacity")
  } else {
    if (geography == "LA") {
      aggregated_capacity <- aggregate(capacity$Netcapacity..1, by=list(Geography=capacity$Local.Authority.Code), FUN=sum)
      aggregated_capacity <- setnames(aggregated_capacity, "x", "aggregated.net.capacity")
    }
  }
  
  return(aggregated_capacity)
}

calculate_demand_by_geography <- function(geography, 
                                          GM_Primary_school_population_by_area_and_age, 
                                          GM_boundaries_by_geography){
  
  if (geography == "ward") {
    aggregated_population <- GM_Primary_school_population_by_area_and_age %>% 
      group_by(Ward.Code, Ward.Name, Local.Authority) %>%
      summarise(demand=sum(Population)) %>%
      setnames("Ward.Code", "Geography")
    #alternative method without dplyr
    #aggregated_population <- aggregate(GM_Primary_school_population_by_area_and_age$Population, by=list(Geography=GM_Primary_school_population_by_area_and_age$LA), FUN=sum)
    #aggregated_population <- setnames(aggregated_population,"x","Demand")
    
  } else {
    if (geography == "LA") {
      aggregated_population_no.geo.code <- GM_Primary_school_population_by_area_and_age %>% 
        group_by(LA) %>%
        summarise(demand=sum(Population))
      
      # add LA codes to GM_Primary_school_population_by_area_and_age
      aggregated_population <- GM_boundaries_by_geography@data %>%
        select(lad15cd, lad15nm) %>%
        setnames("lad15cd","Geography") %>% 
        setnames("lad15nm","LA") %>%
        left_join(aggregated_population_no.geo.code, by="LA")
      # note - warning code expected from joining character and factor variable
  
    }
  }
}

calculate_difference_demand_capacity <- function(primary_school_demand_by_geography, GM_school_capacity_by_geography){
  
  join_demand_capacity <- primary_school_demand_by_geography %>% 
                            left_join(GM_school_capacity_by_geography, by='Geography')
  
  join_demand_capacity$aggregated.net.capacity[is.na(join_demand_capacity$aggregated.net.capacity)] <- 0
                           
  join_demand_capacity$difference=join_demand_capacity$aggregated.net.capacity - join_demand_capacity$demand
  
  return(join_demand_capacity)
}

join_difference_to_shapefile <- function(GM_primary_school_demand_capacity_diff,
                                         GM_boundaries_by_geography,
                                         geography){
  if (geography == "ward") {
    GM_primary_school_demand_capacity_diff <- setnames(GM_primary_school_demand_capacity_diff, "Geography", "wd15cd")

    GM_boundaries_by_geography@data <-  GM_boundaries_by_geography@data %>% left_join(GM_primary_school_demand_capacity_diff, by='wd15cd')

  } else {
    if (geography == "LA") {
    GM_primary_school_demand_capacity_diff <- setnames(GM_primary_school_demand_capacity_diff, "Geography", "lad15cd")
      
    GM_boundaries_by_geography@data <-  GM_boundaries_by_geography@data %>% left_join(GM_primary_school_demand_capacity_diff, by='lad15cd')
    
    }
  }
  return(GM_boundaries_by_geography)  
}



### run the model
run_model <- function(geography){
  GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
  #ways to look at the data:
  #View(GM_boundaries_by_geography@data)
  #plot(GM_boundaries_by_geography)
  GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
  GM_school_capacity_by_geography <- calculate_capacity_by_geography(geography)
  primary_school_demand_by_geography <- calculate_demand_by_geography(geography, 
                                                                      GM_Primary_school_population_by_area_and_age, 
                                                                      GM_boundaries_by_geography)
  GM_primary_school_demand_capacity_diff <- calculate_difference_demand_capacity(primary_school_demand_by_geography, GM_school_capacity_by_geography)
  GM_boundaries_with_school_data <- join_difference_to_shapefile(GM_primary_school_demand_capacity_diff,
                                                                 GM_boundaries_by_geography,
                                                                 geography)
  #useful for checking if join has occured:
  #head(GM_boundaries_with_school_data@data)
  
  return(GM_boundaries_with_school_data)
}

### plot with user parameter specified
plot(run_model(geography = "LA"))


#### choropleth map
# creating breaks to colour the choropleth
breaks <- c(min(GM_boundaries_with_school_data@data$difference),
            -0.5,
            0.5,
            max(GM_boundaries_with_school_data@data$difference)) # breaks between colours

# positive difference = excess capacity = good = green
# negative difference = unmet demand = bad = red
colours <- c("Red", "Black", "Green")

# plot the map
plot(GM_boundaries_with_school_data, col=colours[findInterval(GM_boundaries_with_school_data@data$difference,
                                                              breaks,
                                                              all.inside=T)])

## app

ui <- fluidPage(
  headerPanel('Where are primary state schools in relation to school age children in Greater Manchester?'),
  sidebarPanel(
    selectInput('geography', 'Please select geography', c('LA', 'ward'))),
  mainPanel(
    plotOutput("map")))

server <- function(input, output) {
  output$map <- renderPlot({
    plot(run_model(input$geography))}) ### Needs to take 'joined' dataset
}

shinyApp(ui = ui, server = server)

#Tests

(results <- test_that("Test that the number of wards in demand set is the number in GM (=215)",
                      {geography = "ward"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      primary_school_demand_by_geography <- calculate_demand_by_geography(geography, 
                                                                                          GM_Primary_school_population_by_area_and_age, 
                                                                                          GM_boundaries_by_geography)
                        count_wards <- length (unique(primary_school_demand_by_geography$Ward.Code))
                      expect_equal(count_wards, 215)
                      }))

(results <- test_that("Test that the number of pupils per LA is the same in the LA data set and the ward data set",
                      {# set geography to LA, make test dataframe (rename of primary_school_demand_by_geography)
                      geography = "LA"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      test_demand_from_LA_data <- calculate_demand_by_geography(geography, 
                                                                                GM_Primary_school_population_by_area_and_age, 
                                                                                GM_boundaries_by_geography) 
                    
                      test_demand_from_LA_data <- test_demand_from_LA_data %>% select(LA, demand) # drops LA geo code
                      # set geography to ward, make primary_school_demand_by_geography, sum by LA to make test dataframe, rename columns
                      geography = "ward"
                      GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
                      primary_school_demand_by_geography <- calculate_demand_by_geography(geography, 
                                                                                          GM_Primary_school_population_by_area_and_age,
                                                                                          GM_boundaries_by_geography)

                      test_demand_from_ward_data <- primary_school_demand_by_geography %>% 
                        group_by(Local.Authority) %>%
                        summarise(demand=sum(demand)) %>%
                        setnames("Local.Authority", "LA") %>%
                        as.data.frame() # to force into data frame only
                      # compare test dataframes, should be the same
                      
                      expect_equal(test_demand_from_LA_data, test_demand_from_ward_data)
                      }))

(results <- test_that("Test that the number of wards is 215",
                      {number_wards <-nrow(calculate_capacity_by_geography(geography = "ward"))
                      expect_equal(number_wards, 215)
                      }))
