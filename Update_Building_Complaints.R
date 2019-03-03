library(tidyverse)


footprints <- read.csv('footprint_set_2018.csv')
address <- read.csv('Address_Master_List.csv', stringsAsFactors = F)
complaints <- read.csv('311_building_complaints.csv', stringsAsFactors = F)

complaints <- complaints %>%
    mutate(street_address = str_trim(street_address, side = "both")) %>%
    left_join(., address,  by = c("street_address" = "address")) %>%
    left_join(., footprints, by = 'bldg_id')


write.csv(complaints, 'Building_Complaints_Full.csv')
