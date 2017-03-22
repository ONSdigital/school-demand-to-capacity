library(dplyr)
library(data.table)

# set working directory
setwd("c:/users/danielle cornish/documents")

# import Ward dataset
wards <- read.csv("Ward_Ages_Output.csv", stringsAsFactors = F)

# aggregate ward populations to LA level

LA <- aggregate(wards$Population, by=list(LA=wards$Local.Authority, Age=wards$Age), FUN=sum)
LA <- setnames(LA, "x", "Population")

#save as csv

write.csv(LA, "LA_Population.csv")
