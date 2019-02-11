library(tidyverse)
library(RSocrata) #used to connect to Open Data API URLs
library(geosphere)
library(data.table)
library(rgeos)
options(digits = 15)

address_violations <- read.csv('Building_Violation_Address_with_bldg_id.csv')

building_complaints <- read.csv('311_building_complaints_with_bldg_id.csv')

service_requests <- read.csv('Yearly_311_Service_Requests_for_Addresses_with_bldg_id.csv')

# address violations

violations_address <- address_violations %>%
    select(bldg_id, address) %>%
    count(bldg_id, address)

# service requests

sr_count <- service_requests %>%
    select(bldg_id, address) %>%
    count(bldg_id, address)

# building complaints

complaint_count <- building_complaints %>%
    select(bldg_id, address=street_address) %>%
    count(bldg_id, address)

# create master list

address_violations <- address_violations %>%
    select(bldg_id, address, latitude, longitude)

building_complaints <- building_complaints %>%
    select(bldg_id, address = street_address, latitude, longitude)

service_requests <- service_requests %>%
    select(bldg_id, address, latitude, longitude)

address_master_list <- bind_rows(address_violations, building_complaints, service_requests)
    
address_master_list <- address_master_list[!duplicated(address_master_list$address), ]

# Loading building footprints

url <- "https://data.cityofchicago.org/resource/syp8-uezg.csv"

building_footprints <- read.socrata(url, stringsAsFactors = F) %>%
    mutate(building = seq(1, n(), 1))

building_footprints <- building_footprints %>%
    filter(st_name1 != "") %>%
    unite(col = "address", t_add1, pre_dir1, st_name1, st_type1, sep = " ") %>%
    mutate_all(tolower)

# compute building footprint lat long

find_coords <- function(input, type='longitude') {
    temp <- substr(input, 17, nchar(input)-3)
    temp <- gsub('(', '', temp, fixed = T)
    temp <- gsub(')', '', temp, fixed = T)
    temp <- strsplit(temp, ', ')
    temp <- strsplit(temp[[1]], ' ')
    temp <- lapply(temp, as.numeric)
    temp <- do.call(rbind, temp)
    if (type == 'longitude') {
        temp <- mean(temp[, 1])
    } else {
        temp <- mean(temp[, 2])
    }
    return(temp)
}

building_footprints$longitude <- sapply(building_footprints$the_geom, find_coords)
building_footprints$latitude <- sapply(building_footprints$the_geom, type='latitude', find_coords)

# create long/lat subsets

buffer <- 0.0005

max_lat <- max(building_footprints$latitude)
min_lat <- min(building_footprints$latitude)
max_long <- max(building_footprints$longitude)
min_long <- min(building_footprints$longitude)

lat_seg <- ((max(building_footprints$latitude) - min(building_footprints$latitude)) / 200)

long_seg <- ((max(building_footprints$longitude) - min(building_footprints$longitude)) / 200)

address_master_no_match <- address_master_list[is.na(address_master_list$bldg_id), ]

for (x in 101:200) {
    for (y in 1:200) {

        no_match_seg <- address_master_no_match %>%
            filter(latitude <= max_lat - (x-1)*lat_seg,
                   latitude >= max_lat - x*lat_seg,
                   longitude <= max_long - (y-1)*long_seg,
                   longitude >= max_long - y*long_seg)

        footprint_seg <- building_footprints %>%
            filter(latitude <= max_lat - (x-1)*lat_seg + buffer,
                   latitude >= max_lat - x*lat_seg - buffer,
                   longitude <= max_long - (y-1)*long_seg  + buffer,
                   longitude >= max_long - y*long_seg  - buffer)
        

         # loop to find closest building id
        
        if (nrow(no_match_seg) == 0) {
            next
        }
        if (nrow(footprint_seg) == 0) {
            next
        }
        
        print(x)
        print(y)
        print('missing count')
        print(nrow(no_match_seg))
        print('footprint count')
        print(nrow(footprint_seg))
        
        for (i in 1:nrow(no_match_seg)) {
            best_dist <- 100000
            
            for (j in 1:nrow(footprint_seg)) {
                dist <- distm(no_match_seg[i, c('longitude','latitude')], footprint_seg[j, c('longitude','latitude')])
                if (dist < best_dist) {
                    best_dist <- dist
                    best_id <- footprint_seg$bldg_id[j]
                }
            }
            no_match_seg$bldg_id[i] <- best_id
        }


    address_master_no_match[(address_master_no_match$latitude <= max_lat - (x-1)*lat_seg) &
                                (address_master_no_match$latitude >= max_lat - x*lat_seg) &
                                (address_master_no_match$longitude <= max_long - (y-1)*long_seg) &
                                (address_master_no_match$longitude >= max_long - y*long_seg), ] <- no_match_seg
    }
}

 # test

address_master_list[is.na(address_master_list$bldg_id), ] <- address_master_no_match

# write to csv

write.csv(address_master_list, 'Address_Master_List.csv', row.names=FALSE)


sum(is.na(address_master_list$bldg_id))


# look into violations with no matching address

violations_no_match <- read.csv('violations_no_match.csv')

violations_no_match <- violations_no_match %>%
    left_join(., address_master_list, by = "address")

sum(is.na(violations_no_match$bldg_id))


    