library(tidyverse)

# Load new 311 dataset with matching sections after 12/18/2018

building_complaints <- read.csv('311_Service_Requests.csv') %>%
    mutate_all(tolower) %>%
    rename_all(tolower) %>%
    mutate(created_date = as.Date(substr(created_date, 1, 10), format='%m/%d/%Y')) %>%
    filter(created_date >= '2018-12-19',
           sr_type %in% c('building violation',
                          'no building permit and construction violation'))

# write to csv

write_csv(building_complaints, '311_building_complaints.csv')
