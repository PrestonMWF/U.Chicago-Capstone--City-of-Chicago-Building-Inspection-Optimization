library(RSocrata) #used to connect to Open Data API URLs
library(dplyr) #used for manipulating data frames; filter being used to remove duplicates

#Script used to download all the necessary 311 and open data sets from portal
#All these sets will be used to enrich the main City of Chicago buildings violation set
#Initial download date on September 27, 2018 from City of Chicago Open Data Portal
#Utilizes Open Data API url and downloads file using read.socrata from Open Data Portal
#General open data site url: https://data.cityofchicago.org/

#0. Load data from new system for records after 12/18/2018

new_311 <- read.csv('311_Service_Requests.csv') %>%
    mutate_all(tolower) %>%
    rename_all(tolower) %>%
    mutate(created_date = as.Date(substr(created_date, 1, 10), format='%m/%d/%Y')) %>%
    filter(created_date >= '2018-12-19')


#1. Downloading data for 311 building reports

url <- "https://data.cityofchicago.org/resource/yama-9had.csv"

abandoned_buildings <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "abandoned_buildings") %>%
  select(service_set, everything())

abandoned_buildings_new <- new_311 %>%
    filter(SR_TYPE == 'Vacant/Abandoned Building Complaint')

write_csv(abandoned_buildings, 
          "311_Service_Requests_Vacant_and_abandoned_buildings.csv")

#2. Downloading data for 311 graffiti removal requests

url <- "https://data.cityofchicago.org/resource/cdmx-wzbz.csv"

graffiti <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "graffiti") %>%
  select(service_set, everything())

new_graffiti <- new_311 %>%
    filter(SR_TYPE == 'Graffiti Removal Request')

write_csv(graffiti, "311_Service_Requests_graffiti.csv")

#3. Downloading data for 311 tree trim requests

url <- "https://data.cityofchicago.org/resource/yvxb-fxjz.csv"

tree_trims <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "tree_trims") %>%
  select(service_set, everything())

write_csv(tree_trims, "311_Service_Requests_tree_trims.csv")

#4. Downloading data for 311 abandoned vehicles requests

url <- "https://data.cityofchicago.org/resource/suj7-cg3j.csv"

abandoned_vehicles <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "abandoned_vehicles") %>%
  select(service_set, everything())

write_csv(abandoned_vehicles, "311_Service_Requests_abandoned_vehicles.csv")

#5. Downloading data for 311 garbage cart requests

url <- "https://data.cityofchicago.org/resource/a9br-8sqt.csv"

garbage_carts <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "garbage_carts") %>%
  select(service_set, everything())

write_csv(garbage_carts, "311_Service_Requests_garbage_carts.csv")

#6. Downloading data for 311 tree debris requests

url <- "https://data.cityofchicago.org/resource/ee5t-jv3t.csv"

tree_debris <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "tree_debris") %>%
  select(service_set, everything())

write_csv(tree_debris, "311_Service_Requests_tree_debris.csv")

#7. Downloading data for 311 pot hole requests

url <- "https://data.cityofchicago.org/resource/787j-mys9.csv"

pot_holes <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "pot_holes") %>%
  select(service_set, everything())

write_csv(pot_holes, "311_Service_Requests_pot_holes.csv")

#8. Downloading data for 311 sanitation code requests

url <- "https://data.cityofchicago.org/resource/kcdz-f29q.csv"

sanitation_code <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "sanitation_code") %>%
  select(service_set, everything())

write_csv(sanitation_code, "311_Service_Requests_sanitation_code.csv")

#9. Downloading data for 311 all lights out requests

url <- "https://data.cityofchicago.org/resource/h5ea-dn36.csv"

all_lights <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "all_lights") %>%
  select(service_set, everything())

write_csv(all_lights, "311_Service_Requests_all_lights.csv")

#10. Downloading data for 311 one light out requests

url <- "https://data.cityofchicago.org/resource/h555-t6kz.csv"

one_light <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "one_light") %>%
  select(service_set, everything())

write_csv(one_light, "311_Service_Requests_one_light.csv")

#11. Downloading data for 311 alley lights requests

url <- "https://data.cityofchicago.org/resource/j9pw-ad5p.csv"

alley_lights <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "alley_lights") %>%
  select(service_set, everything())

write_csv(alley_lights, "311_Service_Requests_alley_lights.csv")

#12. Downloading data for 311 rodent baiting requests

url <- "https://data.cityofchicago.org/resource/dvua-vftq.csv"

rodent_baiting <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "rodent_baiting") %>%
  select(service_set, everything())

write_csv(rodent_baiting, "311_Service_Requests_rodent_baiting.csv")

#13. Downloading data for Chicago Building Violations- will act as main data set
#General site url: https://data.cityofchicago.org/Buildings/Building-Violations/22u3-xenr
#Page includes overview of what data is captured in set for; each row is a violation

url <- "https://data.cityofchicago.org/resource/ucdv-yd74.csv"

building_violations <- read.socrata(url, stringsAsFactors = F) %>%
  mutate_all(tolower)

write_csv(building_violations, "City_of_Chicago_building_violations.csv")

building_violation_address <- building_violations %>%
    select(address, id, latitude, longitude)

write_csv(building_violation_address, "Building_Violation_Address.csv")

#14. Downloading data for Building Ordinance Violations 
#Reflects building code violations brought before the Chicago DOA Hearings
#Doesn't reflect code violations brought before the Circuit Court of Cook County

url <- "https://data.cityofchicago.org/resource/awqx-tuwv.csv"

ordinance_violations <- read.socrata(url, stringsAsFactors = F) %>%
  mutate_all(tolower)

write_csv(ordinance_violations, "City_of_Chicago_ordinance_violations.csv")

#15. Downloading building scofflaw list. These are properties where the landlord has multiple unnaddressed court cases. 
#Updated yearly.

url <- 'https://data.cityofchicago.org/resource/ve4p-h7ak.csv'

scofflaw_list <- read.socrata(url, stringsAsFactors = F) %>%
    mutate_all(tolower)

write.csv(scofflaw_list, 'Scofflaw_List.csv')

#16. Downloading problem landlord list. This is a list of landlords that have received two or more administrative hearings.

url <- 'https://data.cityofchicago.org/resource/qjqj-a2d9.csv'

problem_landlord <- read.socrata(url, stringsAsFactors = F) %>%
    mutate_all(tolower)

write.csv(problem_landlord, 'Problem_Landlords.csv')

#17. Downloading socioeconomic indicators

url <- 'https://data.cityofchicago.org/resource/jcxq-k9xf.csv'

socioeconomic_indicators <- read.socrata(url, stringsAsFactors = F) %>%
    mutate_all(tolower)

write.csv(socioeconomic_indicators, 'Socioeconomic_Indicators.csv')

#Checking that all files have been downloaded into Rproj directory
#Downloads bring in 14 data sets from Chicago Open Data Portal

list.files(pattern = ".csv")
