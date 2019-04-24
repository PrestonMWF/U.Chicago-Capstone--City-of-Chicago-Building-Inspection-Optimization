### This script is being used to prepare the data set for open building complaints
### Set is the primary source for Shiny app housing predictive modelling results

library(tidyverse)
library(lubridate)

city_complaints <- read.csv("Building_Complaints_Full.csv", stringsAsFactors = F)

open_building_complaints <- city_complaints %>%
  filter(!is.na(x_coordinate) & zip_code != "" & !is.na(logistic_preds)) %>%
  mutate(days_open = Sys.Date() - as.Date(created_date),
         days_open = as.numeric(days_open) - 40,
         past_21_days = ifelse(days_open > 21, 1, 0),
         complaint_type = case_when(
           past_21_days == 0 & logistic_preds == 0 ~ "Under 21 days with no violation prediction",
           past_21_days == 1 & logistic_preds == 0 ~ "Over 21 days with no violation prediction", 
           past_21_days == 0 & logistic_preds == 1 ~ "Under 21 days with violation predicted", 
           past_21_days == 1 & logistic_preds == 1 ~ "Over 21 days with violation predicted"
         ),
         complaint_type = factor(complaint_type, levels = c(
           "Under 21 days with no violation prediction", 
           "Over 21 days with no violation prediction", 
           "Under 21 days with violation predicted", 
           "Over 21 days with violation predicted"
          )
         ),
         complaint_pred = ifelse(logistic_preds == 1, "Showing violation prediction",
                                 "Not showing violation prediction"),
         past_21_days = case_when(
           past_21_days == 1 & status == "open" ~ "Past 21 day timeline",
           past_21_days == 1 | past_21_days == 0 & status == "completed" ~ "Completed",
           past_21_days == 0 & status == "open" ~ "Before 21 day timeline"
        ),
        risk_category = case_when(
          log_rank < .20 ~ "0-19th Risk Percentile",
          log_rank >= .20 & log_rank < .40 ~ "20-39th Risk Percentile",
          log_rank >= .40 & log_rank < .60 ~ "40-59th Risk Percentile",
          log_rank >= .60 & log_rank < .80 ~ "60-79th Risk Percentile",
          log_rank >= .80 ~ "80-100th Risk Percentile"
        ),
        model_probs = case_when(
          cut(log(logistic_probs), breaks = 5) == "(-4.99,-4.01]" ~ "0-19th Model Probability Percentile",
          cut(log(logistic_probs), breaks = 5) == "(-4.01,-3.03]" ~ "20-39th Model Probability",
          cut(log(logistic_probs), breaks = 5) == "(-3.03,-2.04]" ~ "40-59th Model Probability",
          cut(log(logistic_probs), breaks = 5) == "(-2.04,-1.06]" ~ "60-79th Model Probability",
          cut(log(logistic_probs), breaks = 5) == "(-1.06,-0.0767]" ~ "80-100th Model Probability"
        ),
        year = year(as_date(created_date)),
        month = month(as_date(created_date), label = T),
        day = day(as_date(created_date))
  ) %>%
  select(sr_number, status, created_date, days_open, past_21_days, complaint_pred, 
         risk_percentile = "log_rank", risk_category, logistic_probs, complaint_type, 
         model_probs, street_address, zip_code, community_area, latitude = "latitude.x", 
         longitude = "longitude.x", year, month, day)

saveRDS(object = open_building_complaints, 
        file = "open_building_complaints.rds")

### Creating address level coefficient effects for inferential tab

glm_coefs <- read.csv("logistic_2018_coefficients.csv", stringsAsFactors = F) %>%
  rename(variable = "X",
         coef = x) %>%
  mutate(variable = ifelse(variable == "(Intercept)", "bldg_condisound", variable))

validation_set <- city_complaints %>%
  filter(!is.na(x_coordinate) & zip_code != "" & !is.na(logistic_preds)) %>%
  select(bldg_sq_fo, shape_area, street_address,
         stories, year_built, bldg_condi, abandoned_buildings_address,
         graffiti_address, one_light_address, pot_holes_address, all_lights_address,
         tree_trims_address, tree_debris_address, alley_lights_address,
         garbage_carts_address, sanitation_code_address, rodent_baiting_address,
         abandoned_vehicles_address, graffiti_community, one_light_community, pot_holes_community,
         all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community,
         garbage_carts_community, sanitation_code_community, rodent_baiting_community,
         abandoned_vehicles_community, hardship_index,
         complaint_violations, permit_violations, periodic_violations,
         registration_violations)

spread_bldg_conition <- validation_set %>%
  select(bldg_condi) %>%
  model.matrix(~bldg_condi - 1, data = .) %>%
  as.data.frame()

validation_set <- validation_set %>%
  select(-bldg_condi) %>%
  bind_cols(spread_bldg_conition) %>%
  filter(!duplicated(street_address)) %>%
  select(-street_address)

top_coef_effect <- function(building){
  building_coef_effect <- validation_set %>%
    slice(building) %>%
    gather(key = "variable", value = "value") %>%
    inner_join(x = ., y = glm_coefs, by = "variable") %>%
    mutate(effect = value * coef %>% round(5)) %>%
    arrange(desc(effect)) %>%
    mutate(building_n = building) %>%
    select(building_n, everything())
  
  return(building_coef_effect)
}

total_buildings <- seq(1, nrow(validation_set), 1)

building_top_effects <- map_df(
  total_buildings, function(x) top_coef_effect(building = x)
) %>%
  group_by(variable) %>%
  mutate(y_density = density(x = effect, n = length(total_buildings))$y,
         x_density = density(x = effect, n = length(total_buildings))$x)
  
building_top_effects %>%
  group_by(building_n) %>%
  top_n(n = 6, wt = effect) %>%
  mutate(rank = seq(1:6)) %>%
  ungroup() %>%
  count(variable, rank, sort = T) %>%
  filter(rank == 1)

average_glm_effect <- building_top_effects %>%
  group_by(variable) %>%
  summarise(avg_var_effect = mean(effect))

effects_per_address <- building_top_effects %>%
  filter(variable != "bldg_condisound") %>%
  left_join(x = ., y = average_glm_effect, by = "variable") %>%
  mutate(change_from_avg = effect - avg_var_effect,
         avg_comparison = ifelse(effect > avg_var_effect, 
                                 "higher than average", "lower than average"),
         change_from_avg = abs(change_from_avg)) %>%
  group_by(building_n) %>%
  top_n(n = 6, wt = change_from_avg) %>%
  arrange(desc(change_from_avg)) %>%
  mutate_if(is.numeric, function(x) round(x, 4)) %>%
  mutate(rank = seq(1:6)) %>%
  ungroup()

effects_per_address %>%
  group_by(rank) %>%
  count(variable, sort = T) %>%
  top_n(n = 6, wt = n) %>%
  arrange(rank)
  
### checking one building to see top five effects

effects_per_address %>%
  filter(building_n == 1)

effects_per_address <- open_building_complaints %>%
  filter(!duplicated(street_address)) %>%
  mutate(building_n = row_number()) %>%
  select(building_n, street_address, model_probs) %>%
  left_join(x = ., y = effects_per_address, by = "building_n") %>%
  select(-building_n)

saveRDS(object = effects_per_address, 
        file = "address_coefficient_effects.rds")

saveRDS(object = average_glm_effect, 
        file = "average_glm_effect.rds")

saveRDS(object = building_top_effects,
        file = "building_top_effects.rds")

### testing faceted panel with top 6 effects

theme_set(
  theme_minimal()
)

set.seed(1017)
sample_address <- sample(x = effects_per_address$street_address, size = 1)

building_example <- effects_per_address %>%
  filter(street_address == sample_address) %>%
  arrange(variable)

avg_line <- average_glm_effect %>%
  filter(variable %in% building_example$variable)

building_top_effects %>%
  filter(variable %in% building_example$variable) %>%
  ggplot(aes(x_density, y_density)) +
  geom_line(size = 1.3, colour = "dodgerblue2", alpha = .5) +
  geom_vline(aes(xintercept = building_example$effect, colour = "Effect"), 
             data = avg_line, size = 1.3) +
  geom_vline(aes(xintercept = avg_var_effect, colour = "Average Value"), 
             data = avg_line, size = 1.3) +
  scale_colour_manual(values = c("darkorchid", "darkorange")) +
  facet_wrap(facets = "variable", scales = "free") +
  labs(title = paste("Top 6 coefficient effects for", building_example$street_address),
       subtitle = paste("Model Percentile for building:", building_example$model_probs),
       x = "Effect",
       y = "Density",
       colour = "Variable")
