library(dplyr)
library(data.table)
library(sqldf)
library(testthat)

# set working directory
setwd("H:\\Desktop")

# import Ward dataset
wards <- read.csv("Ward_Ages_SQL_Friendly_Input.csv", stringsAsFactors = F)

ward_dataset <- sqldf(
                      "SELECT WardCode, WardName, LocalAuthority, AllAges, X5, X6, X7, X8, X9, X10, X11
                       FROM wards 
                       WHERE LocalAuthority 
                       IN 
                       ('Bolton','Bury','Manchester','Oldham','Rochdale','Salford','Stockport','Tameside','Trafford','Wigan') 
                       ORDER BY WardCode"
                      )

Ward_Final <- matrix(0,nrow=1505, ncol=5)                                                  # worked out how many rows in advance just to save writing code which adds rows each loop
colnames(Ward_Final) <- c("WardCode", "Ward Name", "Local Authority", "Age","Population")

l <- 0                                                                                     # set to zero outside the loop, this is the row counter for the output

for (j in 1:dim(ward_dataset)[1])                                                          # 1 to 215 rows in the subset
{
  for (i in 5:11)                                                                          # loop through columns 5 to 11 in each row of input loop j
  {
      l <- l + 1                                                                           # add 1 for the output row for each loop of i (columns)
      Ward_Final[l,1] <- toString(ward_dataset$WardCode[j])                                # repeat these vertically in the output
      Ward_Final[l,2] <- toString(ward_dataset$WardName[j])                                # repeat these vertically in the output
      Ward_Final[l,3] <- toString(ward_dataset$LocalAuthority[j])                          # repeat these vertically in the output
      Ward_Final[l,4] <- i                                                                 # use loop counter i as the age indicator in the output
      Ward_Final[l,5] <- ward_dataset[j,i]                                                 # had to use x,y coords as couldn't dynamically assemble column name X5, X6, etc as vector reference
  }
}

test_failed <- matrix(0,nrow=1505, ncol=5)
colnames(test_failed) <- c("WardCode", "Ward Name","Age","test_input1", "test_input2")

test_passed <- matrix(0,nrow=1505, ncol=5)
colnames(test_passed) <- c("WardCode", "Ward Name","Age","test_input1", "test_input2")

#start of the loop to test each age

n <- 0                                                                                             # This is the pointer for the output rows, starts at zero so n <- n + 1 on line 54 results in row one at the start of the main loop

for (k in 1:dim(ward_dataset)[1])                                                                  # Go through each row of the input .csv
{
  for (m in 5:11)                                                                                  # This is the pointer for the age columns
  {
    n <- n + 1                                                                                     # Add 1 to each row
    test_input1 <- ward_dataset %>%
      filter(WardCode == ward_dataset[k,1])                                                        # Build a subset of one Ward Code in the input csv for each loop
  
    test_input2 <- Ward_Final %>%
      as.data.frame(Ward_Final) %>%
      filter(WardCode == test_input1$WardCode)                                                     # Build a subset of the output csv using the input csv Ward Code
    
    test_results <- test_that("Test age by ward", {
    expect_equal(as.integer(test_input1[1,m]), as.integer(as.character(test_input2[m-4,5])))})     # Compare input ages with output ages
    
    if(test_results == F)                                                                          # If test_that returns false
    {
      test_failed[n,1] <- test_input1$WardCode                                                     # Dump the values into test_failed
      test_failed[n,2] <- test_input1$WardName         
      test_failed[n,3] <- m   
      test_failed[n,4] <- as.integer(test_input1[1,m])                                            
      test_failed[n,5] <- as.integer(as.character(test_input2[m-4,5]))        
    } 
    else                                                                                           # If test_that returns true
    {
      test_passed[n,1] <- test_input1$WardCode                                                     # Dump the values into test_passed        
      test_passed[n,2] <- test_input1$WardName         
      test_passed[n,3] <- m   
      test_passed[n,4] <- as.integer(test_input1[1,m])                                            
      test_passed[n,5] <- as.integer(as.character(test_input2[m-4,5])) 
    }
  }
} 

#end of the loop to test each age  

write.csv(Ward_Final, "Ward_Ages_Output.csv", row.names = F) 
