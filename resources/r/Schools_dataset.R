# libraries
library(dplyr)
library(data.table)

# set working directory
setwd("/Users/sunnyt/Google Drive/mc-root/ONS accelerator projects/MC-ONS Shared Folder/Project 1/Data")


#### data

## import dataset containing school capacity
capacity <- read.csv("2015_capacity_and_forecast_underlying_data_UD1.csv", stringsAsFactors = F)

## import school postcodes
postcodes <- read.csv("school postcode.csv", stringsAsFactors = F)

## import National Statistics postcode lookup UK
# big file ~700 MB, takes some time to open
postcode.lookup <- read.csv("National_Statistics_Postcode_Lookup_UK.csv", stringsAsFactors = F)

## Greater Manchester LAs, taken from wikipedia https://en.wikipedia.org/wiki/Greater_Manchester_Combined_Authority#Members
LAnames <- (c("Bolton",
              "Bury",
              "Manchester",
              "Oldham",
              "Rochdale",
              "Salford",
              "Stockport",
              "Tameside",
              "Trafford",
              "Wigan"))

#### model (in this case the model consists of just a data exploration and
#wrangling process)

generate_schools_dataset <- function(capacity, postcodes, postcode.lookup, LAnames){
  
  ## concatenate LA code and Establishment number in the postcodes dataset to
  ## get unique school ID 'LAEstab'
  postcodes.LAestab <- postcodes %>%
    mutate(LAEstab = as.numeric(paste(LA..code., EstablishmentNumber, sep="")))
  
  ## join postcodes to schools using inner_join in order to exclude NAs
  GM.schools.dataset <- capacity %>%
    filter(Phase == "PS") %>% #filter out primary schools
    filter(LAname %in% LAnames.GM) %>% #filter out GM LAs
    inner_join(postcodes.LAestab, by="LAEstab") %>% #join postcodes 
    select(c(LAEstab, Schoolname, Netcapacity..1., Postcode, LAname)) #select required columns
  
  ## join by Postcode.3 to get wards and LAs for schools
  # leaving in eastings and northings
  GM.schools.dataset.geo <- GM.schools.dataset %>%
    mutate(Postcode.3 = Postcode) %>%
    inner_join(postcode.lookup, by = "Postcode.3") %>%
    select(LAEstab, Schoolname, Netcapacity..1., Postcode,
           Easting, Northing,
           Local.Authority.Code, Local.Authority.Name,
           Ward.Code, Ward.Name)
  
  return(GM.schools.dataset.geo)
}

schools.dataset <- generate_schools_dataset(capacity, postcodes, postcode.lookup, LAnames.GM)
# expecting dataframe with 827 rows and 10 columns

#### output

## testing
#install.packages("testthat")
library(testthat)
(results <- test_that("Test Crumpsall Lane Primary School has a capacity of 420", {
  test_capacity <- schools.dataset %>%
    filter(Schoolname == "Crumpsall Lane Primary School") %>%
    select(Netcapacity..1.) %>%
    as.numeric()
  expect_equal(test_capacity, 420)
}))

## write to csv
write.csv(schools.dataset, "Generated datasets/GM_schools_dataset.csv", row.names = F)

