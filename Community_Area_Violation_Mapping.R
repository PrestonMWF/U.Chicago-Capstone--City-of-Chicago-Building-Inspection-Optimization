library(tidyverse) #used for manipulating data frames and ggplot2
library(rgdal) #used to map buildings to a community area in chicago using lat/lon

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
  ungroup() %>%
  inner_join(x = .,y = community_buildings, by = "community")

community_buildings <- community_buildings %>%  
  mutate(violations_per_building = round(total_complaint_violations / 
                                           total_buildings, 3))

community_buildings %>%
  top_n(25, violations_per_building) %>%
  filter(!is.na(community)) %>%
  mutate(community = reorder(community, violations_per_building)) %>%
  ggplot(aes(community, violations_per_building)) +
  geom_col(fill = "dodgerblue2") +
  coord_flip() +
  labs(title = "Top 25 community areas by building complaint violations per building",
       subtitle = "West Garfield Park has most violations per building of any community area in Chicago (violations / # buildings)", 
       x = NULL)

#joining both footprints and shapes set together
community_area_map <- community_area_map %>%
  mutate(community_area = as.numeric(community_area)) %>%
  left_join(x = ., y = community_buildings, by = "community_area")

#developing maps for violations per building, total violations, and total buildings
community_area_map %>%
  ggplot(aes(long, lat, group = group, fill = total_complaint_violations)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange") +
  labs(title = "Map of total complaint violations by community area in Chicago",
       subtitle = "Neighbourhoods on Chicago's South and West side show large incidences of violations",
       fill = "Total Violations")

community_area_map %>%
  ggplot(aes(long, lat, group = group, fill = total_buildings)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange") +
  labs(title = "Map of total number of building by community area in Chicago",
       subtitle = "Neighbourhoods on Chicago's South and West side show high building concentrations",
       fill = "Total Buildings")

community_area_map %>%
  ggplot(aes(long, lat, group = group, fill = violations_per_building)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme_void() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange") +
  labs(title = "Map of violations per building by community area in Chicago",
       subtitle = "Neighbourhoods on Chicago's South and West side show large incidences of violations",
       fill = "Violations per Building")

#total violations and per building violations
community_area_map %>%
  select(long, lat, group, violations_per_building, total_complaint_violations) %>%
  mutate(violations_per_building = scale(violations_per_building, 
                                         center = T, scale = T),
         total_complaint_violations = scale(total_complaint_violations, 
                                            center = T, scale = T)) %>%
  gather(key = "violation_type", value = "value", -long, -lat, -group) %>%
  ggplot(aes(long, lat, group = group, fill = value)) +
  geom_polygon() +
  geom_path(color = "white") +
  facet_wrap(facets = "violation_type") +
  coord_equal() +
  theme_minimal() +
  scale_fill_gradient(low = "dodgerblue2", high = "darkorange", 
                      labels = c("High", "Medium", "Low"), 
                      breaks = c(3.2, 1.2, -.75)) +
  labs(title = "Map of violations per building and total violations (converted to z-scores) by community area",
       subtitle = "Neighbourhoods on Chicago's South and West side show large incidences of violations",
       fill = "Scaled Values")
