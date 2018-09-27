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
    select(service_set, community_area, ward, police_district, year) %>%
    filter(!is.na(ward) | !is.na(community_area) | !is.na(police_district)) 
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
  select(service_set, community_area, ward, police_district, year) %>%
  filter(!is.na(ward) & !is.na(community_area) & !is.na(police_district)) %>%
  bind_rows(service_requests)

#Counting the number of 311 requests by city ward and service type

ward_service_counts <- service_requests %>%
  count(ward, service_set)

#Spreading data so each ward is individual row with respective count for each service

ward_service_counts <- ward_service_counts %>%
  spread(key = "service_set", value = "n")

#Making set with 311 counts by ward per year

yearly_ward_counts <- service_requests %>%
  count(year, ward, service_set) %>%
  filter(!is.na(ward) & (!is.na(year))) %>%
  arrange(year)

yearly_ward_counts <- yearly_ward_counts %>%
  spread(key = "service_set", value = "n")
