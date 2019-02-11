library(tidyverse) #used for manipulating data frames and map functions

#Script used to aggregate city of chicago 311 data sets
#Purpose is to have a count of how many incidents took place in various parts of the city
#Set will be merged into the final city of chicago building inspections set

#Rproj directory has 14 csv files in it
#Only taking first 12 which are all 311 sets

files <- list.files(pattern = "*.csv")[4:15]

data_list <- files %>%
  map(., function(x) read.csv(x, stringsAsFactors = F))

# load address master list

address_master_list <- read.csv('Address_Master_List.csv',
                                stringsAsFactors = F)

# Load new 311 dataset with matching sections after 12/18/2018

new_311 <- read.csv('311_Service_Requests.csv') %>%
    mutate_all(tolower) %>%
    rename_all(tolower) %>%
    mutate(created_date = as.Date(substr(created_date, 1, 10), format='%m/%d/%Y')) %>%
    filter(created_date >= '2018-12-19',
           sr_type %in% c('vacant/abandoned building complaint', 'graffiti removal request',
                          'rodent baiting/rat complaint', 'alley light out complaint', 
                          'street light out complaint', 'sanitation code violation', 'pothole in street complaint',
                          'tree debris clean-up request', 'garbage cart maintenance', 'abandoned vehicle complaint',
                          'tree trim request'))

new_311 <- new_311 %>%
    mutate(year = year(created_date)) %>%
    select(address = street_address, service_set = sr_type, community_area, year) %>%
    filter(!is.na(community_area)) %>%
    mutate(service_set = str_replace(service_set, "vacant/abandoned building complaint", "abandoned_buildings"),
           service_set = str_replace(service_set, "graffiti removal request", "graffiti"),
           service_set = str_replace(service_set, "rodent baiting/rat complaint", "rodent_baiting"),
           service_set = str_replace(service_set, "alley light out complaint", "alley_lights"),
           service_set = str_replace(service_set, "street light out complaint", "one_light"),
           service_set = str_replace(service_set, "sanitation code violation", "sanitation_code"),
           service_set = str_replace(service_set, "pothole in street complaint", "pot_holes"),
           service_set = str_replace(service_set, "tree debris clean-up request", "tree_debris"),
           service_set = str_replace(service_set, "garbage cart maintenance", "garbage_carts"),
           service_set = str_replace(service_set, "abandoned vehicle complaint", "abandoned_vehicles"),
           service_set = str_replace(service_set, "tree trim request", "tree_trims"),
           community_area = as.integer(community_area),
           year = as.character(year))


#Devleoping a function to extract only needed variables from each 311 set
#Function also adds in a new column for year which is derived from completion date

city_data_prep <- function(data){
  data %>%
    mutate(year = str_sub(string = creation_date, start = 1, end = 4)) %>%
    rename(address = street_address) %>%
    mutate(address = tolower(address)) %>%
    select(address, service_set, community_area, year) %>%
    filter(!is.na(community_area)) 
}

#Using map_df to extract all 12 data frames from data list while combining them
#Applying city_data_prep function to retain only 5 needed columns

service_requests <- map_df(1:11, function(x) as.data.frame(data_list[x])) %>%
    city_data_prep()
    

#Processing file 12 (abandoned buildings) as individual df due to different format
#Does not have completion date- instead using date received
#Binding it to service_request df found above in the same process

service_requests <- as.data.frame(data_list[12]) %>%
    mutate(year = str_sub(string = date_service_request_was_received, 
                        start = 1, end = 4)) %>%
    unite(col = "address", "address_street_number", "address_street_direction", 
        "address_street_name", "address_street_suffix", sep = " ") %>%
    mutate(address = tolower(address)) %>%
    select(address, service_set, community_area, year) %>%
    filter(!is.na(community_area)) %>%
    bind_rows(service_requests) %>%
    bind_rows(new_311) %>%
    left_join(., address_master_list, by = "address")

#Counting the number of 311 requests by city community_area and service type

community_area_counts <- service_requests %>%
  count(community_area, service_set)

#Spreading data so each community_area is individual row
#Counts respective incidents for each service

community_area_counts <- community_area_counts %>%
  spread(key = "service_set", value = "n")

write.csv(community_area_counts, "Aggregated_311_Service_Requests_for_Community_Area.csv", row.names = F)

#Making set with 311 counts by community_area per year

yearly_community_area_counts <- service_requests %>%
  count(year, community_area, service_set) %>%
  filter(!is.na(year)) %>%
  arrange(year)

yearly_community_area_counts <- yearly_community_area_counts %>%
  spread(key = "service_set", value = "n") %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

write.csv(yearly_community_area_counts, 
          "Yearly_311_Service_Requests_for_Community_Area.csv", row.names = F)

#Counting the number of 311 requests by address and service type

address_counts <- service_requests %>%
  count(bldg_id, service_set)

#Spreading data so each address is individual row
#Counts respective incidents for each service

address_counts <- address_counts %>%
  spread(key = "service_set", value = "n") %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

write.csv(address_counts, "Aggregated_311_Service_Requests_for_Addresses.csv", row.names = F)

#Making set with 311 counts by address per year

yearly_address_counts <- service_requests %>%
  count(year, bldg_id, service_set) %>%
  filter(!is.na(year)) %>%
  arrange(year)

yearly_address_counts <- yearly_address_counts %>%
  spread(key = "service_set", value = "n") %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

write.csv(yearly_address_counts, 
          "Yearly_311_Service_Requests_for_Addresses.csv", row.names = F)

