library(dplyr)
library(data.table)


# import Ward dataset
wards <- read.csv("data/clean/ward_population.csv", stringsAsFactors = F)

# aggregate ward populations to LA level

LA <- aggregate(wards$Population, by=list(LA=wards$Local.Authority, Age=wards$Age), FUN=sum)
LA <- setnames(LA, "x", "Population")

#save as csv

write.csv(LA, "data/clean/LA_Population.csv")
