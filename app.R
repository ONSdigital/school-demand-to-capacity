#install.packages("maptools")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("shiny")
#install.packages("testthat")
library(maptools)
library(data.table)
library(dplyr)
library(testthat)
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
    boundaries <- readShapePoly("data/Ward_shapefile_GM/GM_Ward.shp")
  } else {
    if (geography == "LA") {
      boundaries <- readShapePoly("data/LA_shapefile_GM/GM_LA.shp")
    }
  }
}

select_primary_school_population_by_geography <- function(geography){
  if (geography == "ward") {
    population <- read.csv("data/ward_population.csv", stringsAsFactors = F)
  } else {
    if (geography == "LA") {
      population <- read.csv("data/LA_Population.csv", stringsAsFactors = F)
    }
  }
}

calculate_capacity_by_geography <- function(geography){
  capacity <- read.csv("data/GM_schools_dataset.csv", stringsAsFactors = F)
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


plot_choropleth_map <- function(GM_boundaries_with_school_data){
  
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
}


### plot with user parameter specified from the command line
#plot_choropleth_map(run_model(geography = "ward"))

## Tests
test_results <- test_dir(".", reporter="summary")

## app

ui <- fluidPage(
  headerPanel('Where are primary state schools in relation to school age children in Greater Manchester?'),
  sidebarPanel(
    selectInput('geography', 'Please select geography', c('LA', 'ward'))),
  mainPanel(
    plotOutput("map")))

server <- function(input, output) {
  output$map <- renderPlot({
    plot_choropleth_map(run_model(input$geography))}) ### Needs to take 'joined' dataset
}

shinyApp(ui = ui, server = server)
