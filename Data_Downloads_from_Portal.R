library(RSocrata) #used to connect to Open Data API URLs
library(dplyr) #used for manipulating data frames; filter being used to remove duplicates

#Script used to download all the necessary 311 and open data sets from portal
#All these sets will be used to enrich the main City of Chicago buildings violation set
#Initial download date on September 27, 2018 from City of Chicago Open Data Portal
#Utilizes Open Data API url and downloads file using read.socrata from Open Data Portal
#General open data site url: https://data.cityofchicago.org/

#1. Downloading data for 311 building reports

url <- "https://data.cityofchicago.org/resource/yama-9had.csv"

abandoned_buildings <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "abandoned_buildings") %>%
  select(service_set, everything())

write_csv(abandoned_buildings, 
          "311_Service_Requests_Vacant_and_abandoned_buildings.csv")

#2. Downloading data for 311 graffiti removal requests

url <- "https://data.cityofchicago.org/resource/cdmx-wzbz.csv"

graffiti <- read.socrata(url, stringsAsFactors = F) %>%
  filter(duplicated(service_request_number) == F) %>%
  mutate(service_set = "graffiti") %>%
  select(service_set, everything())

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

#14. Downloading data for Building Ordinance Violations 
#Reflects building code violations brought before the Chicago DOA Hearings
#Doesn't reflect code violations brought before the Circuit Court of Cook County

url <- "https://data.cityofchicago.org/resource/awqx-tuwv.csv"

ordinance_violations <- read.socrata(url, stringsAsFactors = F) %>%
  mutate_all(tolower)

write_csv(ordinance_violations, "City_of_Chicago_ordinance_violations.csv")

#Checking that all files have been downloaded into Rproj directory
#Downloads bring in 14 data sets from Chicago Open Data Portal

list.files(pattern = ".csv")
