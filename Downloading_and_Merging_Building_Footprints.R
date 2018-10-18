library(RSocrata) #used to connect to Open Data API URLs
library(rgdal) #used to map buildings to a community area in chicago using lat/lon
library(tidyverse) #used for manipulating data frames
library(sqldf)

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



# Loading ordinance violaions

ordinance_violations <- read.csv('City_of_Chicago_ordinance_violations.csv',
                                 stringsAsFactors = F) %>%
    mutate(ADDRESS = tolower(ADDRESS))


# spreading dispostion outcomes

address_ordinance_fines <- ordinance_violations %>%
    mutate(ADDRESS = str_trim(ADDRESS, side = "both")) %>%
    group_by(ADDRESS) %>%
    summarize(total_fines = sum(IMPOSED.FINE)) %>%
    rename_all(tolower)

address_ordinance_dispositions <- ordinance_violations %>%
    mutate(ADDRESS = str_trim(ADDRESS, side = "both")) %>%
    count(ADDRESS, CASE.DISPOSITION) %>%
    arrange(desc(n))

address_ordinance_dispositions <- address_ordinance_dispositions %>%
    spread(key = "CASE.DISPOSITION", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-ADDRESS), as.integer) %>%
    rename_all(tolower) %>%
    rename('null' = v1) %>%
    rename('other' = `adjudication performance / other`) %>%
    rename('dismissed' = `dismissed without prejudice`) %>%
    rename('vacated' = `judgment vacated`) %>%
    rename('not_liable' = `not liable`) %>%
    rename('non_suit' = `non-suit`) %>%
    rename_at(vars(-address), function(x) paste0(x, "_disposition"))

# Merging number of ordinance violations to data set

address_ordinance_dispositions <- address_ordinance_dispositions %>%
    filter(!duplicated(address)) %>%
    left_join(., address_ordinance_fines, by = "address")

building_footprints <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_ordinance_dispositions, by = "address") %>%
    mutate_at(vars(null_disposition, 
                   other_disposition,
                   continuance_disposition,
                   default_disposition,
                   dismissed_disposition,
                   vacated_disposition,
                   liable_disposition,
                   non_suit_disposition,
                   not_liable_disposition,
                   total_fines), function(x) as.integer(ifelse(is.na(x), 0, x)))

# Load and merge Scofflaw List

scofflaw_list <- read.csv('Scofflaw_List.csv',
                                 stringsAsFactors = F)

scofflaw_list <- scofflaw_list %>%
    mutate(listed_scofflaw = 1) %>%
    select(c(address, listed_scofflaw))

building_footprints <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., scofflaw_list, by = "address")

# Load and merge problem landlords (We may want to merge this with the scofflaw list, as the information is very similar)
# Need to investigate if the landlord address is the building they rent out or their business address

problem_landlords <- read.csv('Problem_Landlords.csv',
                              stringsAsFactors = F)

problem_landlords <- problem_landlords %>%
    mutate(listed_problem_landlord = 1) %>%
    select(c(address, listed_problem_landlord))

building_footprints <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., problem_landlords, by = "address")

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

write_csv(building_footprints, "City_of_Chicago_building_footprints.csv")
