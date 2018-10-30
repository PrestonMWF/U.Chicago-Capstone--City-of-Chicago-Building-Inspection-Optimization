building_footprints %>%
  filter(total_fines == 0) %>%
  summarise(percent_not_fined = n() / nrow(building_footprints) * 100)

building_footprints %>%
  select(permit_violations, periodic_violations, 
         complaint_violations, registration_violations) %>%
  gather(key = "violation_type", value = "count") %>%
  filter(count == 0) %>%
  group_by(violation_type) %>%
  count(count) %>%
  mutate(percent_wo_violation = n / nrow(building_footprints) * 100)

building_footprints %>%
  filter(total_fines > 0) %>%
  arrange(desc(total_fines)) %>%
  select(address, community_area, total_fines) %>%
  top_n(10, total_fines)

building_footprints %>%
  select(other_disposition, 
         dismissed_disposition, 
         continuance_disposition, 
         null_disposition, 
         liable_disposition, 
         default_disposition, 
         non_suit_disposition, 
         vacated_disposition) %>%
  gather(key = "disposition_type", value = "count") %>%
  filter(count == 0) %>%
  group_by(disposition_type) %>%
  count(count) %>%
  mutate(percent_wo_disposition = n / nrow(building_footprints) * 100)

building_footprints %>%
  select(graffiti_address, one_light_address, pot_holes_address, 
         all_lights_address, tree_trims_address, tree_debris_address, 
         alley_lights_address, garbage_carts_address, sanitation_code_address, 
         rodent_baiting_address, abandoned_vehicles_address) %>%
  gather(key = "complaint_type", value = "count") %>%
  filter(count == 0) %>%
  group_by(complaint_type) %>%
  count(count) %>%
  mutate(percent_wo_complaint = n / nrow(building_footprints) * 100)
