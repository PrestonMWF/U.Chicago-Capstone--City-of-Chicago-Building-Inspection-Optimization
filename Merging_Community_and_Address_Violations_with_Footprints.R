library(dplyr) #used to manipulate data frames

#Script used to merge in aggregated city of chicago 311 data sets
#Purpose is to have a count of how many incidents took place in various parts of the city
#Also merging in address specific violations from different 311 sources


#loading both building footprints and 311 counts by community and address
building_footprints <- read.csv("City_of_Chicago_building_footprints.csv", 
                                stringsAsFactors = F)

address_counts <- read.csv("Aggregated_311_Service_Requests_for_Addresses.csv", 
                           stringsAsFactors = F) %>%
  rename_at(vars(-address), function(x) paste0(x, "_address"))
  

community_area_counts <- read.csv("Aggregated_311_Service_Requests_for_Community_Area.csv", 
                           stringsAsFactors = F) %>%
  rename_at(vars(-community_area), function(x) paste0(x, "_community"))


#Joining into both sets by address and community area

building_footprints <- building_footprints %>%
  left_join(., address_counts, by = "address") %>%
  left_join(., community_area_counts, by = "community_area")

write_csv(building_footprints, "City_of_Chicago_building_footprints.csv")

