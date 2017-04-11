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


### Visual tests
# map the results of the model with specified user parameter
plot_choropleth_map(run_model(geography = "LA"))
plot_choropleth_map(run_model(geography = "ward"))


### Run the unit tests
# test_dir set at root in order to point at tests_model_functions.R
# which contains tests associated with the model functions
# the model functions rely on paths to data sets
# therefore the tests also rely on these paths
# beware moving the tests or changing the test_dir may break the tests
test_results <- test_dir(".", reporter="summary")
test_results
# not the most intuitive output
# as long as failed = 0 we are ok!

# sources used to help build unit tests:
# https://www.r-bloggers.com/unit-testing-with-r/
# https://github.com/hadley/testthat
