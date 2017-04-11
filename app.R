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

### Load model functions
source("src/r/model_functions.R")

#### app

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
