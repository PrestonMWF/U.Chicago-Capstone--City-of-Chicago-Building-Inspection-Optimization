library(tidyverse) #used for manipulating data frames and ggplot2

#Script used to map building violations by community area in Chicago

#load community shapes- these are the spatial polygons that outline communities
community_shapes <- readOGR(dsn = getwd(), layer = "CommAreas")

community_shapes@data$id <- rownames(community_shapes@data)

fortify_shapes <- fortify(community_shapes, region = "id")

#joining shapes with community area data
community_area_map <- inner_join(x = fortify_shapes, y = community_shapes@data, 
                                 by = "id") %>%
  rename(community_area = AREA_NUMBE)

#developing simple map without any community area details added
community_area_map %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  ggtitle("Map for 77 community areas in Chicago")

#loading building footprints set to aggregate violations by community area
building_footprints <- read.csv("City_of_Chicago_building_footprints.csv", 
                                stringsAsFactors = F)

community_buildings <- building_footprints %>%
  filter(!is.na(community)) %>%
  count(community) %>%
  rename(total_buildings = n)

community_buildings <- building_footprints %>%
  filter(!is.na(community)) %>%
  group_by(community, community_area) %>%
  summarise(total_complaint_violations = sum(complaint_violations)) %>%
  inner_join(x = .,y = community_buildings, by = "community")

community_buildings <- community_buildings %>%  
  mutate(violations_per_building = round(total_complaint_violations / 
                                           total_buildings, 3))

#joining both footprints and shapes set together
community_area_map <- community_area_map %>%
  mutate(community_area = as.numeric(community_area)) %>%
  left_join(x = ., y = community_buildings, by = "community_area")

#developing maps for violations per building and total violations
community_area_map %>%
  ggplot(aes(long, lat, group = group, fill = total_complaint_violations)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange") +
  labs(title = "Map of total complaint violations by community area in Chicago",
       subtitle = "Neighbourhoods on Chicago's Sotuh and West side show large incidences of violations",
       fill = "Total Violations")

community_area_map %>%
  ggplot(aes(long, lat, group = group, fill = violations_per_building)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange") +
  labs(title = "Map of violations per building by community area in Chicago",
       subtitle = "Neighbourhoods on Chicago's Sotuh and West side show large incidences of violations",
       fill = "Violations per Building")


