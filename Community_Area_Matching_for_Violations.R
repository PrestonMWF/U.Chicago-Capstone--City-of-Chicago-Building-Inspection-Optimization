library(RSocrata) #used to connect to Open Data API URLs
library(rjson) #used to download json from open data portal API
library(tidyverse) #used for manipulating data frames and mapping functions
library(sp) #used to match lat and long from violation to community area in city

#Script used to download community boundaries for chicago
#Other aggregated 311 sets will be joined to the building violations set using this field
#Set includes numeric marker for community boundary 
#General open data site url: https://data.cityofchicago.org/

#downloading community boundaries json and csv files from open data portal

url <- "https://data.cityofchicago.org/resource/igwz-8jzy.json"

community_json <- fromJSON(file = url)

url <- "https://data.cityofchicago.org/resource/igwz-8jzy.csv"

community_csv <- read.socrata(url, stringsAsFactors = F) %>%
  rename(community_area = area_numbe,
         community = community) %>%
  mutate(community = tolower(community)) %>%
  select(community_area, community)

#loading building violations set
building_violations <- read.csv("City_of_Chicago_building_violations.csv",
                                stringsAsFactors = F)

#Devleoping a function to extract the community area lat and long coorindates from json
#Function then matches the yes/no value to the community area number

community_matching <- function(comm_area){
  coords <- matrix(round(unlist(community_json[[comm_area]]$the_geom$coordinates), 20), 2)
  
  y_coord <- coords[1,]
  x_coord <- coords[2,]
  
  #point.in.polygon matches if a lat/long point is within a given coordinates polygon
  #this identifies which community boundary each violation is in
  
  matching_vector <- point.in.polygon(point.x = building_violations$latitude, 
                                      point.y = building_violations$longitude,  
                                      pol.x = x_coord, 
                                      pol.y = y_coord)
  
  matching_vector <- ifelse(matching_vector == 1, comm_area, NA)
}

#Using map to extract all 77 coordinate pairs using function from above
#Output is matrix with 77 columns and ~1.5million rows
#Each column has the appropriate community area number or an NA

matched_communities <- map(1:77, function(x) community_matching(x)) 

#Gathering all the actual community numbers
#Matrix from above is sparse so filtering out NAs and unifying all numbers into one column
#Adding in column that marks which row from original data set marker is for; used for join

matched_communities <- as.data.frame(matched_communities) %>%
  mutate(violation = seq(1, n(), 1)) %>%
  gather(key = "comm_area", value = "community_area", -violation) %>%
  filter(!is.na(community_area))

#Joining community markers with violations set and community names csv

building_violations <- building_violations %>%
  mutate(violation = seq(1, n(), 1)) %>%
  left_join(., matched_communities, by = "violation") %>%
  select(-violation, -comm_area) %>%
  left_join(., community_csv, by = "community_area") %>%
  select(address, community, community_area, everything())

write_csv(building_violations, "City_of_Chicago_building_violations.csv")
