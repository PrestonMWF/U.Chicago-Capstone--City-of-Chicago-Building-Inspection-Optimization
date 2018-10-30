library(randomForest)
library(caret)
library(tidyverse)
library(ResourceSelection)

building_footprints <- read.csv("City_of_Chicago_building_footprints.csv", 
                                stringsAsFactors = F)

#modelling all the violation counts and aggregating would be good

#only complaints used- other three violations column have too many 0s

#clustering all 311s
clustering_set <- building_footprints %>%
  select(graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
         tree_trims_address, tree_debris_address, alley_lights_address, 
         garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
         abandoned_vehicles_address, complaint_violations, graffiti_community, 
         one_light_community, pot_holes_community, all_lights_community, 
         tree_trims_community, tree_debris_community, alley_lights_community, 
         garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
         abandoned_vehicles_community, complaint_violations) %>%
  scale(center = T, scale = T) %>%
  as.data.frame() %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

cluster_collect <- function(return, training_data, cluster_n, n_start, seed){
  set.seed(seed = seed)
  train_cluster <- kmeans(training_data, centers = cluster_n, nstart = n_start)
  
  vaf <- data.frame(cluster = cluster_n,
                    train_VAF = 1 - train_cluster$tot.withinss / train_cluster$totss)
  
  train_size <- train_cluster$cluster
  
  train_means <- train_cluster$centers
  
  if (return == "vaf") {return(vaf)}
  if (return == "test") {return(test_size)}
  if (return == "train") {return(train_size)}
  if (return != "vaf" | return != "test" | return != "train") {
    stop("Value specified is not part of function- Call one of vaf, train, or test")}
}

vaf_compare <- map_df(4:12, function(x) cluster_collect(seed = 1017,
                                             return = "vaf",
                                             training_data = clustering_set, 
                                             cluster_n = x, 
                                             n_start = 1))
theme_set(
  theme_minimal()
)

vaf_compare %>%
  ggplot(aes(cluster, train_VAF)) +
  geom_line(size = 1.3, colour = "dodgerblue2") +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  scale_x_continuous(breaks = seq(4, 12, 1)) +
  geom_vline(xintercept = 5, alpha = .3, colour = "blueviolet", size = 1.3) +
  geom_vline(xintercept = 8, alpha = .3, colour = "blueviolet", size = 1.3) +
  annotate("rect", xmin = 5, xmax = 8,
           ymin = .3, ymax = .475, 
           alpha = .1, fill = "blueviolet") +
  labs(title = "Variance Accounted For (VAF) Screeplot for Building Footprint data",
       subtitle = "Selecting 5 to 8 clusters seems appropriate",
       y = "VAF")

building_kmeans <- kmeans(x = clustering_set, centers = 8)

cluster_means <- as.data.frame(building_kmeans$centers) %>%
  mutate(Group = 1:8) %>%
  select(Group, everything())

cluster_means %>%
  gather(key = "variable", value = "means", -Group) %>%
  mutate(Group = as.factor(Group)) %>%
  ggplot(aes(variable, means, fill = Group, group = Group)) +
  geom_col(size = 1.3, show.legend = F) +
  geom_hline(yintercept = 0, size = 1.3, alpha = .5, colour = "darkgray") +
  facet_wrap(facets = "Group", nrow = 2) +
  coord_flip() +
  theme_bw() +
  labs(title = "Faceted lines plot for variable means by group for eight cluster k-means model",
       y = "group means",
       x = NULL)

modelling_set <- building_footprints %>%
  mutate(complaint_marker = ifelse(complaint_violations > 0, 1, 0)) %>%
  select(complaint_violations, bldg_sq_fo, no_of_unit, shape_area, shape_len, stories, year_built, 
         graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
         tree_trims_address, tree_debris_address, alley_lights_address, 
         garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
         abandoned_vehicles_address, complaint_violations, graffiti_community, 
         one_light_community, pot_holes_community, all_lights_community, 
         tree_trims_community, tree_debris_community, alley_lights_community, 
         garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
         abandoned_vehicles_community, total_fines, complaint_marker) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(cluster = as.character(building_kmeans$cluster))

set.seed(1017)
data_split <- createDataPartition(y = modelling_set$complaint_marker, 
                                  p = .7, 
                                  list = F)

building_train <- modelling_set %>%
  slice(data_split)

building_test <- modelling_set %>%
  slice(-data_split)

footprint_logistic <- glm(complaint_marker ~ ., 
                          data = building_train[,-1], 
                          family = binomial("logit"))

summary(footprint_logistic)

train_pred <- ifelse(footprint_logistic$fitted.values > .5, 1, 0) %>%
  as.numeric()

confusionMatrix(building_train$complaint_marker, train_pred)

violation_review <- building_train %>%
  select(complaint_marker, complaint_violations) %>%
  mutate(fitted_probs = footprint_logistic$fitted.values)

building_test <- building_test %>%
  mutate(logistic_probs = predict(footprint_logistic, 
                          newdata = building_test),
         logistic_pred = ifelse(logistic_probs > .5, 1, 0))

confusionMatrix(building_test$complaint_marker, building_test$logistic_pred)

step_glm <- step(footprint_logistic)

#used for glm goodness of fit- very small p-value indicates poor model fit
hoslem.test(building_train$complaint_marker, step_glm$fitted.values)

decision_boundary <- function(model, prob, data, y){
  suppressWarnings(
    results <- data %>%
      mutate(Class = y,
             class_prob = predict.glm(model, newdata = data, type = "response"),
             class_pred = ifelse(class_prob > prob, 1, 0))
  )
  
  conf_mat <- confusionMatrix(results$Class, results$class_pred)
  accuracy <- conf_mat$overall[1]
  
  sens <- sensitivity(as.factor(results$Class), as.factor(results$class_pred))
  spec <- specificity(as.factor(results$Class), as.factor(results$class_pred))
  balance <- (sens + spec) / 2
  
  neg_pred <- negPredValue(as.factor(results$Class), as.factor(results$class_pred))
  pos_pred <- posPredValue(as.factor(results$Class), as.factor(results$class_pred))
  
  return(list(accuracy = accuracy,
              sensitivity = sens, 
              specificity = spec,
              balanced = balance,
              neg_pred = neg_pred,
              pos_pred = pos_pred))
}

decision_probs <- map_df(seq(0.1, .9, .01), 
                      function(x) decision_boundary(model = footprint_logistic, 
                                                    prob = x,
                                                    data = building_test,
                                                    y = building_test$complaint_marker))

decision_probs <- decision_probs %>%
  select(-neg_pred, -pos_pred) %>%
  mutate(probability = (seq(0.1, .9, .01))) %>%
  select(probability, everything())

decision_probs %>%
  gather(key = "accuracy_metric", value = "values", -probability) %>%
  ggplot(aes(probability, values, colour = accuracy_metric)) +
  geom_line(size = 1.3, alpha = .45) +
  geom_vline(xintercept = 0.5, size = 1.3, alpha = .45, colour = "darkgray") +
  scale_x_continuous(breaks = seq(0, 1, .05), expand = c(.02, 0, 0.02, 0)) +
  scale_y_continuous(breaks = seq(0, 1, .1), expand = c(0, 0, 0, 0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Classification test accuracy metrics based on differing probability decision boundary",
       subtitle = "Highest accuracy (~83.6%) comes from boundary cut off at .41; Best balanced result (~89) at .9",
       y = NULL)

building_test <- building_test %>%
  mutate(step_probs = predict.glm(step_glm, 
                                  newdata = building_test, type = "response"),
         step_pred = ifelse(step_probs > .9, 1, 0))

confusionMatrix(building_test$complaint_marker, building_test$step_pred)

#pca regression
pca_training <- building_train %>%
  select(-complaint_violations, -complaint_marker) %>%
  mutate_if(is.character, as.numeric)

building_pca <- princomp(x = pca_training)$scores %>%
  as.data.frame()

names(building_pca) <- names(pca_training)

building_pca <- building_pca %>%
  mutate(complaint_marker = building_train$complaint_marker,
         cluster = as.character(cluster))

footprint_pca_logistic <- glm(complaint_marker ~ ., 
                          data = building_pca, 
                          family = binomial("logit"))

summary(footprint_pca_logistic)

#tree attempts
tree_set <- modelling_set %>%
  select(-cluster) %>%
  bind_cols(
    model.matrix(~ cluster + 0, data = modelling_set) %>% as.data.frame()
  )

tree_train <- tree_set %>%
  slice(data_split)

tree_test <- tree_set %>%
  slice(-data_split)

footprint_forest <- randomForest(complaint_violations ~ ., data = tree_train)


         