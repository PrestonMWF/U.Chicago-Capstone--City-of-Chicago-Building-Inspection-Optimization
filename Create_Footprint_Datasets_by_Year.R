library(RSocrata) #used to connect to Open Data API URLs
library(rgdal) #used to map buildings to a community area in chicago using lat/lon
library(tidyverse) #used for manipulating data frames
library(lubridate)

#Loading building violations set for merge

building_violations <- read.csv("City_of_Chicago_building_violations.csv", 
                                stringsAsFactors = F) %>%
    mutate(address = tolower(address),
           violation_date = as.Date(violation_date, "%Y-%m-%d"))

# Loading building footprints

url <- "https://data.cityofchicago.org/resource/syp8-uezg.csv"

building_footprints <- read.socrata(url, stringsAsFactors = F) %>%
    mutate(building = seq(1, n(), 1))

building_footprints <- building_footprints %>%
    filter(st_name1 != "") %>%
    unite(col = "address", t_add1, pre_dir1, st_name1, st_type1, sep = " ") %>%
    mutate_all(tolower)

# Load address master

address_master_list <- read.csv('Address_Master_List.csv',
                                stringsAsFactors = F)

building_violations <- building_violations %>%
    left_join(., address_master_list, by = "address")

# Separate violations by year

building_violations_2018 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) == 2018)

building_violations_2017 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) == 2017)

building_violations_2016 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) == 2016)

# violations since 2011

building_violations_thru_2018 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) <= 2018,
           year(violation_date) >= 2013)

building_violations_thru_2017 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) <= 2017,
           year(violation_date) >= 2012)

building_violations_thru_2016 <- building_violations %>%
    mutate(violation_date == as.Date(violation_date, "%Y-%m-%d")) %>%
    filter(year(violation_date) <= 2016,
           year(violation_date) >= 2011)


# aggregate datasets

#Aggregating the building violations by address and violation type

address_violations_2017 <- building_violations_2017 %>%
    mutate(address = str_trim(address, side = "both")) %>%
    count(bldg_id, inspection_category) %>%
    arrange(desc(n))

address_violations_2016 <- building_violations_2016 %>%
    mutate(address = str_trim(address, side = "both")) %>%
    count(bldg_id, inspection_category) %>%
    arrange(desc(n))


address_violations_thru_2018 <- building_violations_thru_2018 %>%
    mutate(address = str_trim(address, side = "both")) %>%
    count(bldg_id, inspection_category) %>%
    arrange(desc(n))

address_violations_thru_2017 <- building_violations_thru_2017 %>%
    mutate(address = str_trim(address, side = "both")) %>%
    count(bldg_id, inspection_category) %>%
    arrange(desc(n))

address_violations_thru_2016 <- building_violations_thru_2016 %>%
    mutate(address = str_trim(address, side = "both")) %>%
    count(bldg_id, inspection_category) %>%
    arrange(desc(n))

#Spreading violations out by type (complaint, periodic, etc.)
#New columns have numeric value for how many violations by type took place at address

address_violations_2017 <- address_violations_2017 %>%
    spread(key = "inspection_category", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-bldg_id), as.integer) %>%
    rename_all(tolower) %>%
    rename_at(vars(-bldg_id), function(x) paste0(x, "_violations"))

address_violations_2016 <- address_violations_2016 %>%
    spread(key = "inspection_category", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-bldg_id), as.integer) %>%
    rename_all(tolower) %>%
    rename_at(vars(-bldg_id), function(x) paste0(x, "_violations"))


address_violations_thru_2018 <- address_violations_thru_2018 %>%
    spread(key = "inspection_category", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-bldg_id), as.integer) %>%
    rename_all(tolower) %>%
    rename_at(vars(-bldg_id), function(x) paste0(x, "_violations"))

address_violations_thru_2017 <- address_violations_thru_2017 %>%
    spread(key = "inspection_category", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-bldg_id), as.integer) %>%
    rename_all(tolower) %>%
    rename_at(vars(-bldg_id), function(x) paste0(x, "_violations"))

address_violations_thru_2016 <- address_violations_thru_2016 %>%
    spread(key = "inspection_category", value = n) %>%
    mutate_all(tolower) %>%
    mutate_at(vars(-bldg_id), as.integer) %>%
    rename_all(tolower) %>%
    rename_at(vars(-bldg_id), function(x) paste0(x, "_violations"))

# Identify complaints with missing addresses

#complaints_address_missing <- building_footprints %>%
#    right_join(., building_violations, by = 'address') %>%
#    filter(is.na(building)) %>%
#    select(building, address, id, latitude, longitude, location)

# write.csv(complaints_address_missing, 'violations_no_match.csv')

#Merging newly created address violations set with building footprints by address
#Filling in NAs from violations with zeros
#Changing violation columns to integers

building_footprints_2017 <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_violations_2017, by = "bldg_id") %>%
    mutate_at(vars(complaint_violations, 
                   registration_violations, 
                   permit_violations, 
                   periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))

building_footprints_2016 <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_violations_2016, by = "bldg_id") %>%
    mutate_at(vars(complaint_violations, 
                   registration_violations, 
                   permit_violations, 
                   periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))

building_footprints_thru_2018 <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_violations_thru_2018, by = "bldg_id") %>%
    mutate_at(vars(complaint_violations, 
                   registration_violations, 
                   permit_violations, 
                   periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))

building_footprints_thru_2017 <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_violations_thru_2017, by = "bldg_id") %>%
    mutate_at(vars(complaint_violations, 
                   registration_violations, 
                   permit_violations, 
                   periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))

building_footprints_thru_2016 <- building_footprints %>%
    filter(!duplicated(address)) %>%
    left_join(., address_violations_thru_2016, by = "bldg_id") %>%
    mutate_at(vars(complaint_violations, 
                   registration_violations, 
                   permit_violations, 
                   periodic_violations), function(x) as.integer(ifelse(is.na(x), 0, x)))



#Loading the community boundaries shape files from open data portal
#URL for site with download: 

community_shapes <- readOGR(dsn = getwd(), layer = "CommAreas")

building_coords <- building_footprints_2017 %>%
    select(x_coord, y_coord) %>%
    mutate_all(as.numeric)

coordinates(building_coords) <- ~ x_coord + y_coord

proj4string(building_coords) <- proj4string(community_shapes)

building_communities <- over(building_coords, community_shapes)



# add shape information to footprint datasets

building_footprints_2017 <- building_communities %>%
    rename(community_area = AREA_NUMBE,
           community = COMMUNITY) %>%
    mutate(community = tolower(community)) %>%
    select(community_area, community) %>%
    bind_cols(building_footprints_2017) %>%
    select(building, address, community_area, community, everything())%>%
    add_count(community_area) %>%
    rename(total_buildings = n)


building_footprints_2016 <- building_communities %>%
    rename(community_area = AREA_NUMBE,
           community = COMMUNITY) %>%
    mutate(community = tolower(community)) %>%
    select(community_area, community) %>%
    bind_cols(building_footprints_2016) %>%
    select(building, address, community_area, community, everything())%>%
    add_count(community_area) %>%
    rename(total_buildings = n)

building_footprints_thru_2018 <- building_communities %>%
    rename(community_area = AREA_NUMBE,
           community = COMMUNITY) %>%
    mutate(community = tolower(community)) %>%
    select(community_area, community) %>%
    bind_cols(building_footprints_thru_2017) %>%
    select(building, address, community_area, community, everything()) %>%
    add_count(community_area) %>%
    rename(total_buildings = n)

building_footprints_thru_2017 <- building_communities %>%
    rename(community_area = AREA_NUMBE,
           community = COMMUNITY) %>%
    mutate(community = tolower(community)) %>%
    select(community_area, community) %>%
    bind_cols(building_footprints_thru_2017) %>%
    select(building, address, community_area, community, everything()) %>%
    add_count(community_area) %>%
    rename(total_buildings = n)

building_footprints_thru_2016 <- building_communities %>%
    rename(community_area = AREA_NUMBE,
           community = COMMUNITY) %>%
    mutate(community = tolower(community)) %>%
    select(community_area, community) %>%
    bind_cols(building_footprints_thru_2016) %>%
    select(building, address, community_area, community, everything()) %>%
    add_count(community_area) %>%
    rename(total_buildings = n)


## socioeconomic data

socioeconomic_indicators <- read.csv('Socioeconomic_Indicators.csv',
                                     stringsAsFactors = F)

building_footprints_2017$community_area <- as.integer(as.character(building_footprints_2017$community_area))
building_footprints_2016$community_area <- as.integer(as.character(building_footprints_2016$community_area))
building_footprints_thru_2018$community_area <- as.integer(as.character(building_footprints_thru_2018$community_area))
building_footprints_thru_2017$community_area <- as.integer(as.character(building_footprints_thru_2017$community_area))
building_footprints_thru_2016$community_area <- as.integer(as.character(building_footprints_thru_2016$community_area))

building_footprints_2017$bldg_id <- as.integer(as.character(building_footprints_2017$bldg_id))
building_footprints_2016$bldg_id <- as.integer(as.character(building_footprints_2016$bldg_id))
building_footprints_thru_2018$bldg_id <- as.integer(as.character(building_footprints_thru_2018$bldg_id))
building_footprints_thru_2017$bldg_id <- as.integer(as.character(building_footprints_thru_2017$bldg_id))
building_footprints_thru_2016$bldg_id <- as.integer(as.character(building_footprints_thru_2016$bldg_id))


building_footprints_2017 <- building_footprints_2017 %>%
    filter(!duplicated(address)) %>%
    left_join(., socioeconomic_indicators,  by = c("community_area" = "ca"))

building_footprints_2016 <- building_footprints_2016 %>%
    filter(!duplicated(address)) %>%
    left_join(., socioeconomic_indicators,  by = c("community_area" = "ca"))

building_footprints_thru_2018 <- building_footprints_thru_2017 %>%
    filter(!duplicated(address)) %>%
    left_join(., socioeconomic_indicators,  by = c("community_area" = "ca"))

building_footprints_thru_2017 <- building_footprints_thru_2017 %>%
    filter(!duplicated(address)) %>%
    left_join(., socioeconomic_indicators,  by = c("community_area" = "ca"))

building_footprints_thru_2016 <- building_footprints_thru_2016 %>%
    filter(!duplicated(address)) %>%
    left_join(., socioeconomic_indicators,  by = c("community_area" = "ca"))



# add in 311 data


address_counts <- read.csv("Yearly_311_Service_Requests_for_Addresses.csv", 
                           stringsAsFactors = F) %>%
    rename_at(vars(-c(bldg_id, year)), function(x) paste0(x, "_address"))


community_area_counts <- read.csv("Yearly_311_Service_Requests_for_Community_Area.csv", 
                                  stringsAsFactors = F) %>%
    rename_at(vars(-c(community_area, year)), function(x) paste0(x, "_community"))


# load address aggregations by year


address_counts_2016 <- address_counts %>%
    filter(year == 2016) %>%
    select(-c(year)) 

address_counts_2017 <- address_counts %>%
    filter(year == 2017) %>%
    select(-c(year)) 

address_counts_2018 <- address_counts %>%
    filter(year == 2018) %>%
    select(-c(year))

# multi-year sets

address_counts_thru_2016 <- address_counts %>%
    filter(year >= 2011,
           year <= 2016) %>%
    select(-c(year)) %>%
    group_by(bldg_id) %>%
    summarize_all(funs(sum))

address_counts_thru_2017 <- address_counts %>%
    filter(year >= 2012,
           year <= 2017) %>%
    select(-c(year)) %>%
    group_by(bldg_id) %>%
    summarize_all(funs(sum))

address_counts_thru_2018 <- address_counts %>%
    filter(year >= 2013,
           year <= 2018) %>%
    select(-c(year)) %>%
    group_by(bldg_id) %>%
    summarize_all(funs(sum))



# load community area aggregations by year


community_area_counts_2016 <- community_area_counts %>%
    filter(year == 2016) %>%
    select(-c(year))

community_area_counts_2017 <- community_area_counts %>%
    filter(year == 2017) %>%
    select(-c(year))

community_area_counts_2018 <- community_area_counts %>%
    filter(year == 2018) %>%
    select(-c(year))

# multi-year sets


community_area_counts_thru_2016 <- community_area_counts %>%
    filter(year >= 2011,
           year <= 2016) %>%
    select(-c(year)) %>%
    group_by(community_area) %>%
    summarize_all(funs(sum))

community_area_counts_thru_2017 <- community_area_counts %>%
    filter(year >= 2012,
           year <= 2017) %>%
    select(-c(year)) %>%
    group_by(community_area) %>%
    summarize_all(funs(sum))

# Joining into both sets by address and community area

building_footprints_2017 <- building_footprints_2017 %>%
    left_join(., address_counts_2017, by = "bldg_id") %>%
    left_join(., community_area_counts_2017, by = "community_area")

building_footprints_2016 <- building_footprints_2016 %>%
    left_join(., address_counts_2016, by = "bldg_id") %>%
    left_join(., community_area_counts_2016, by = "community_area")

building_footprints_thru_2018 <- building_footprints_thru_2017 %>%
    left_join(., address_counts_thru_2018, by = "bldg_id") %>%
    left_join(., community_area_counts_thru_2017, by = "community_area")

building_footprints_thru_2017 <- building_footprints_thru_2017 %>%
    left_join(., address_counts_thru_2017, by = "bldg_id") %>%
    left_join(., community_area_counts_thru_2017, by = "community_area")

building_footprints_thru_2016 <- building_footprints_thru_2016 %>%
    left_join(., address_counts_thru_2016, by = "bldg_id") %>%
    left_join(., community_area_counts_thru_2016, by = "community_area")

# add next year dependent variables

all_violations_2018 <- building_violations_2018 %>%
    select(c(bldg_id)) %>%
    group_by(bldg_id) %>%
    count(bldg_id) %>%
    rename(any_violation = n)

complaint_violations_2018 <- building_violations_2018 %>%
    filter(inspection_category == 'complaint') %>%
    select(c(bldg_id)) %>%
    group_by(bldg_id) %>%
    count(bldg_id) %>%
    rename(complaint_violation = n)

all_violations_2017 <- building_violations_2017 %>%
    select(c(bldg_id)) %>%
    group_by(bldg_id) %>%
    count(bldg_id) %>%
    rename(any_violation = n)

complaint_violations_2017 <- building_violations_2017 %>%
    filter(inspection_category == 'complaint') %>%
    select(c(bldg_id)) %>%
    group_by(bldg_id) %>%
    count(bldg_id) %>%
    rename(complaint_violation = n)

# add to footprints datasets

building_footprints_2017 <- building_footprints_2017 %>%
    left_join(., all_violations_2018, by = "bldg_id") %>%
    left_join(., complaint_violations_2018, by = "bldg_id") %>%
    mutate(any_violation = if_else(is.na(any_violation), 0, 1),
           complaint_violation = if_else(is.na(complaint_violation), 0, 1))

building_footprints_2016 <- building_footprints_2016 %>%
    left_join(., all_violations_2017, by = "bldg_id") %>%
    left_join(., complaint_violations_2017, by = "bldg_id") %>%
    mutate(any_violation = if_else(is.na(any_violation), 0, 1),
           complaint_violation = if_else(is.na(complaint_violation), 0, 1))

building_footprints_thru_2017 <- building_footprints_thru_2017 %>%
    left_join(., all_violations_2018, by = "bldg_id") %>%
    left_join(., complaint_violations_2018, by = "bldg_id") %>%
    mutate(any_violation = if_else(is.na(any_violation), 0, 1),
           complaint_violation = if_else(is.na(complaint_violation), 0, 1))

building_footprints_thru_2016 <- building_footprints_thru_2016 %>%
    left_join(., all_violations_2017, by = "bldg_id") %>%
    left_join(., complaint_violations_2017, by = "bldg_id") %>%
    mutate(any_violation = if_else(is.na(any_violation), 0, 1),
           complaint_violation = if_else(is.na(complaint_violation), 0, 1))

# save as csv

#write.csv(building_footprints_2017, "building_footprints_2017.csv")
#write.csv(building_footprints_2016, "building_footprints_2016.csv")
write.csv(building_footprints_thru_2017, "building_footprints_2012-2017.csv", row.names = F)
write.csv(building_footprints_thru_2016, "building_footprints_2011-2016.csv", row.names = F)
