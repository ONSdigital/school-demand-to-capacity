# libraries
#install.packages("dplyr")
#install.packages("data.table")
library(dplyr)
library(data.table)



#### data

## import dataset containing school capacity
capacity <- read.csv("data/raw/2015_capacity_and_forecast_underlying_data_UD1.csv", stringsAsFactors = F)
summary(capacity)
dim(capacity)

## import school postcodes
postcodes <- read.csv("data/raw/school postcode.csv", stringsAsFactors = F)
summary(postcodes)
dim(postcodes)

## import National Statistics postcode lookup UK
# big file ~700 MB, takes some time to open
postcode.lookup <- read.csv("data/raw/National_Statistics_Postcode_Lookup_UK.csv", stringsAsFactors = F)
str(postcode.lookup)
dim(postcode.lookup)

#### model (in this case the model consists of just a data exploration and
#wrangling process). The code order follows a stream of consciousness working
#through the dataset and problems that arise - later we will extract the
#transformative parts of code to another script

generate_schools_dataset <- function(capacity, postcodes, postcode.lookup){
  
  ## filter out primary schools and select required columns
  netcapacity.PS <- capacity %>%
    filter(Phase == "PS") %>%
    select(c(estab1, Schoolname, Netcapacity..1.))
  summary(netcapacity.PS)
  # 16743 primary schools
  
  # join postcodes to schools
  netcapacity.PS.postcodes <- netcapacity.PS %>%
    setnames("estab1", "EstablishmentNumber") %>%
    left_join(postcodes, by="EstablishmentNumber")
  
  #!!PROBLEM 360485 rows in netcapacity.PS.postcodes, not 16743 as expected
  
  ### checks on capacity dataset
  
  ## check for duplicates
  any(duplicated(capacity)) #no duplicate rows
  any(duplicated(capacity$estab1)) # we thought estab1 was a unique identifier for a school
  any(duplicated(capacity$Schoolname)) # we have the same school appearing more than once, so each row is not a different school?
  #so what is unique about each row?
  any(duplicated(capacity$LAEstab)) #LAEstab, the first column, is unique
  
  ## what is LAEstab?
  capacity$LA1_estab1 <- as.numeric(paste(capacity$LA1, capacity$estab1, sep=""))
  summary(capacity)
  any(capacity$LAEstab - capacity$LA1_estab1 != 0) # no difference between my LAestab and the provided one
  # therefore comes from concatenating LA1 col with Estab1 col
  
  # conclusion: estab1 is not a unique School identifier. However the combination of the
  # LA1 and estab1 is unique, giving a unique School ID
  
  ## what is LA1?
  capacity[1,]
  # City of London GSS code = E02000001
  # LA1 = 201
  capacity[10000,]
  # Hamphire GSS code = E10000014
  # LA1 = 850
  
  # conclusion: LA1 is not a direct shortening of the GSS code for the LA
  
  # is LA1 == LA?
  nrow(unique(capacity[,c("LA1","LAname")]))
  length(unique(capacity$LA1))
  length(unique(capacity$LAname))
  # 152 is the correct amount of LAs for England (http://www.lgiu.org.uk/local-government-facts-and-figures/)
  
  # conclusion: LA1 = LA, however not clear where the coding comes from
  
  ### proceed using combination of LA and Establishment number as unique
  #identifier for schools. For the capacity dataset this is just LAEstab, whereas for
  #the postcodes dataset we need to create this
  
  ## concatenate LA code and Establishment number in the postcodes dataset
  postcodes.LAestab <- postcodes %>%
    mutate(LAEstab = as.numeric(paste(LA..code., EstablishmentNumber, sep="")))
  summary(postcodes.LAestab)
  
  ## Greater Manchester LAs, taken from wikipedia https://en.wikipedia.org/wiki/Greater_Manchester_Combined_Authority#Members
  LAnames.GM <- (c("Bolton",
                   "Bury",
                   "Manchester",
                   "Oldham",
                   "Rochdale",
                   "Salford",
                   "Stockport",
                   "Tameside",
                   "Trafford",
                   "Wigan"))
  
  ## create the schools dataset using a series of commands piped together
  GM.schools.dataset <- capacity %>%
    filter(Phase == "PS") %>% #filter out primary schools
    filter(LAname %in% LAnames.GM) %>% #filter out GM LAs
    left_join(postcodes.LAestab, by="LAEstab") %>% #join postcodes
    select(c(LAEstab, Schoolname, Netcapacity..1., Postcode, LAname)) #select required columns
  
  summary(GM.schools.dataset)
  table(GM.schools.dataset$LAname) #counts number of schools in each LA
  dim(GM.schools.dataset)#849 schools
  
  #!!PROBLEM - we have NAs - some schools are missing postcodes
  
  ## how many NAs?
  length(which(is.na(GM.schools.dataset$Postcode) == T))
  filter(GM.schools.dataset, is.na(Postcode))
  # 19 schools from 7 LAs
  
  ## compare postcodes dataset and capacity dataset separately for Oldham to investigate NAs
  oldham.cap <- filter(capacity, LAname == "Oldham")
  oldham.post <- filter(postcodes, LA..name. == "Oldham")
  # 99 in both datasets, but are they the same 99 schools?
  setdiff(capacity$estab1, postcodes$EstablishmentNumber)
  setdiff(postcodes$EstablishmentNumber, capacity$estab1)
  # doesnt look like it
  
  ### We decided to press on with the schools for which there is both capacity and postcodes
  
  ## re-create the schools dataset as above but using inner_join instead of left_join in order to exclude NAs
  GM.schools.dataset <- capacity %>%
    filter(Phase == "PS") %>% #filter out primary schools
    filter(LAname %in% LAnames.GM) %>% #filter out GM LAs
    inner_join(postcodes.LAestab, by="LAEstab") %>% #join postcodes 
    select(c(LAEstab, Schoolname, Netcapacity..1., Postcode, LAname)) #select required columns
  
  dim(GM.schools.dataset)
  # 830 GM schools in the dataset
  
  ### Next step - Joining ward and LA GSS codes using postcode - making use of the national postcode lookup
  
  ## First, explore the postcode lookup table
  str(postcode.lookup)
  # there are 3 postcode columns in the postcode.lookup to choose from
  # explore which type of postcode matches best
  # use set difference to see which postcodes don't match
  setdiff(GM.schools.dataset$Postcode, postcode.lookup$Postcode.1)
  setdiff(GM.schools.dataset$Postcode, postcode.lookup$Postcode.2)
  setdiff(GM.schools.dataset$Postcode, postcode.lookup$Postcode.3)
  # Postcode.3 matches best as only 3 unmatched
  # another way of doing this - intersect pulls out the matching postcodes, and then use length to count how many matched
  length(intersect(GM.schools.dataset$Postcode, postcode.lookup$Postcode.1))
  length(intersect(GM.schools.dataset$Postcode, postcode.lookup$Postcode.2))
  length(intersect(GM.schools.dataset$Postcode, postcode.lookup$Postcode.3))
  # matched out of how many? was assuming 830 schools so 830 postcodes
  length(unique(GM.schools.dataset$Postcode))
  # but actual number of unique GM school postcodes is 811
  
  # conclusion: best we can do by matching to one postcode is to use Postcode.3
  
  ## can we get match all postcode by using 2 postcode columns?
  unmatched.postcodes <- setdiff(GM.schools.dataset$Postcode, postcode.lookup$Postcode.3)
  length(intersect(unmatched.postcodes, postcode.lookup$Postcode.1))
  length(intersect(unmatched.postcodes, postcode.lookup$Postcode.2))
  # either Postcode.1 or Postcode.2 would work
  
  ### decided to press on with 
  
  ## join by Postcode.3 to get wards and LAs for schools
  # leaving in eastings and northings
  GM.schools.dataset.geo <- GM.schools.dataset %>%
    mutate(Postcode.3 = Postcode) %>%
    inner_join(postcode.lookup, by = "Postcode.3") %>%
    select(LAEstab, Schoolname, Netcapacity..1., Postcode,
           Easting, Northing,
           Local.Authority.Code, Local.Authority.Name,
           Ward.Code, Ward.Name)
  
  dim(GM.schools.dataset.geo)
  # 827 schools in the working version of the schools dataset
  
  
  return(GM.schools.dataset.geo)
}
# expecting dataframe with 827 rows and 10 columns

#### output

# testing
#install.packages("testthat")
library(testthat)
  
  # write to csv
write.csv(GM.schools.dataset.geo, "data/clean/GM_schools_dataset.csv", row.names = F)

