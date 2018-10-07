library(tidyverse) #used for manipulating data frames and map functions

#Script used to aggregate city of chicago 311 data sets
#Purpose is to have a count of how many incidents took place in various parts of the city
#Set will be merged into the final city of chicago building inspections set

#Rproj directory has 14 csv files in it
#Only taking first 12 which are all 311 sets

files <- list.files(pattern = "*.csv")[1:12]

data_list <- files %>%
  map(., function(x) read.csv(x, stringsAsFactors = F))

#Devleoping a function to extract only needed variables from each 311 set
#Function also adds in a new column for year which is derived from completion date

city_data_prep <- function(data){
  data %>%
    mutate(year = str_sub(string = completion_date, start = 1, end = 4)) %>%
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
  bind_rows(service_requests)

#Counting the number of 311 requests by city community_area and service type

community_area_counts <- service_requests %>%
  count(community_area, service_set)

#Spreading data so each community_area is individual row
#Counts respective incidents for each service

community_area_counts <- community_area_counts %>%
  spread(key = "service_set", value = "n")

#Making set with 311 counts by community_area per year

yearly_community_area_counts <- service_requests %>%
  count(year, community_area, service_set) %>%
  filter(!is.na(year)) %>%
  arrange(year)

yearly_community_area_counts <- yearly_community_area_counts %>%
  spread(key = "service_set", value = "n")







#Counting the number of 311 requests by address and service type

address_counts <- service_requests %>%
  count(address, service_set)

#Spreading data so each address is individual row
#Counts respective incidents for each service

address_counts <- address_counts %>%
  spread(key = "service_set", value = "n") %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

#Making set with 311 counts by address per year

yearly_address_counts <- service_requests %>%
  count(year, address, service_set) %>%
  filter(!is.na(year)) %>%
  arrange(year)

yearly_address_counts <- yearly_address_counts %>%
  spread(key = "service_set", value = "n") %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))
