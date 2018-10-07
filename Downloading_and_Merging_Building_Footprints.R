library(RSocrata) #used to connect to Open Data API URLs
library(rgdal) #used to map buildings to a community area in chicago using lat/lon
library(tidyverse) #used for manipulating data frames

#Script used to download all the necessary 311 and open data sets from portal
#General open data site url: https://data.cityofchicago.org/

#Downloading building footprints from open data portal
#Original download date on October 5, 2018

url <- "https://data.cityofchicago.org/resource/syp8-uezg.csv"

building_footprints <- read.socrata(url, stringsAsFactors = F) %>%
  mutate(building = seq(1, n(), 1))

building_footprints <- building_footprints %>%
  filter(qc_date != "") %>%
  unite(col = "address", t_add1, pre_dir1, st_name1, st_type1, sep = " ") %>%
  mutate_all(tolower)

write_csv(building_footprints, "City_of_Chicago_building_footprints.csv")

#Loading building violations set for merge

building_violations <- read.csv("City_of_Chicago_building_violations.csv", 
                                stringsAsFactors = F) %>%
  mutate(address = tolower(address))

#Aggregating the building violations by address and violation type

address_violations <- building_violations %>%
  mutate(addres = str_trim(address, side = "both")) %>%
  count(address, inspection_category) %>%
  arrange(desc(n))

#Spreading violations out by type (complaint, periodic, etc.)
#New columns have numeric value for how many violations by type took place at address

address_violations <- address_violations %>%
  spread(key = "inspection_category", value = n) %>%
  mutate_all(tolower) %>%
  mutate_at(vars(-address), as.integer) %>%
  rename_all(tolower) %>%
  rename_at(vars(-address), function(x) paste0(x, "_violations"))

#Merging newly created address violations set with building footprints by address
#Filling in NAs from violations with zeros
#Changing violation columns to integers

building_footprints <- building_footprints %>%
  filter(!duplicated(address)) %>%
  left_join(., address_violations, by = "address") %>%
  mutate_at(vars(complaint_violations, 
                 registration_violations, 
                 permit_violations, 
                 periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))

#With set merged, moving to add the community areas for each address
#These are needed to merge with other aggregated 311 sets
#To do so, downloading community boundaries json and csv files from open data portal
#Need to add actual lat and long into building footprints set as well

#Loading the community boundaries shape files from open data portal
#URL for site with download: 

community_shapes <- readOGR(dsn = getwd(), layer = "CommAreas")

building_coords <- building_footprints %>%
  select(x_coord, y_coord) %>%
  mutate_all(as.numeric)

coordinates(building_coords) <- ~ x_coord + y_coord

proj4string(building_coords) <- proj4string(community_shapes)

building_communities <- over(building_coords, community_shapes)

building_footprints <- building_communities %>%
  rename(community_area = AREA_NUMBE,
         community = COMMUNITY) %>%
  mutate(community = tolower(community)) %>%
  select(community_area, community) %>%
  bind_cols(building_footprints) %>%
  select(building, address, community_area, community, everything())
