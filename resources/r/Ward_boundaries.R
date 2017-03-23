#install.packages("maptools")
library(maptools)


#### data

# ward boundaries
Ward_boundaries <- readShapePoly("data/raw/Wards_December_2015_Full_Extent_Boundaries_in_Great_Britain.shp")
class(Ward_boundaries) # the shapefile is read in as a SpatialPolygonsDataFrame
slotNames(Ward_boundaries) # which is actually made up of a number of parts which we access using @
head(Ward_boundaries@data) # the "data" is where we see the 'attributes' of the polygons e.g. the ward name
dim(Ward_boundaries@data) # 8668 wards - matches count on source website http://geoportal.statistics.gov.uk/datasets/afcc88affe5f450e9c03970b237a7999_1

Wardnames <- read.csv("data/raw/Ward_Ages_Output.csv", stringsAsFactors = F)

GM_Ward_boundaries <- Ward_boundaries[which(Ward_boundaries@data$wd15cd %in% Wardnames$Ward.Code),]

dim(GM_Ward_boundaries)

plot(GM_Ward_boundaries)

#### output
writePolyShape(GM_Ward_boundaries, "data/clean/Ward_shapefile_GM/GM_Ward.shp")


