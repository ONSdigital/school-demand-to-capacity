#install.packages("maptools")
library(maptools)

#### data

# ward boundaries
LA_boundaries <- readShapePoly("data/raw/Local_Authority_Districts_December_2015_Full_Extent_Boundaries_in_Great_Britain.shp")
class(LA_boundaries) # the shapefile is read in as a SpatialPolygonsDataFrame
slotNames(LA_boundaries) # which is actually made up of a number of parts which we access using @
head(LA_boundaries@data) # the "data" is where we see the 'attributes' of the polygons e.g. the ward name
dim(LA_boundaries@data) # 8668 wards - matches count on source website http://geoportal.statistics.gov.uk/datasets/afcc88affe5f450e9c03970b237a7999_1

#### model

### pull out just the area covered by GM
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

GM_LA_boundaries <- LA_boundaries[which(LA_boundaries@data$lad15nm %in% LAnames),]

# plot
plot(GM_LA_boundaries)

#### output
writePolyShape(GM_LA_boundaries, "data/clean/LA_shapefile_GM/GM_LA.shp")


