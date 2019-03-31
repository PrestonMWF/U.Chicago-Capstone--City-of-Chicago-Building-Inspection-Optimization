library(MASS)
library(caret)
library(gains)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(data.table)
library(gbm)
library(AUC)
library(rms)
library(tidyverse)

#setwd('~/MS Analytics/Capstone')


# create function to impute data

impute_footprints <- function(dataset) {
    
    year_impute_median <- dataset %>%
        filter(year_built != 0) %>%
        group_by(community_area) %>%
        summarise(median_year = median(year_built))
    
    year_impute_set <- dataset %>%
        filter(year_built == 0) %>%
        select(-year_built) %>%
        left_join(x = ., y = year_impute_median, by = "community_area") %>%
        rename(year_built = median_year)
    
    sqft_impute_set <- dataset %>%
        filter(bldg_sq_fo != 0) %>%
        group_by(community_area) %>%
        summarise(median_sqft = median(bldg_sq_fo))
    
    sqft_impute_set <- dataset %>%
        filter(bldg_sq_fo == 0) %>%
        select(-bldg_sq_fo) %>%
        left_join(x = ., y = sqft_impute_set, by = "community_area") %>%
        rename(bldg_sq_fo = median_sqft) %>%
        select(building, bldg_sq_fo)
    
    dataset <- dataset %>%
        filter(year_built != 0) %>%
        bind_rows(year_impute_set) %>%
        arrange(building)
    
    sqft_impute_set <- dataset %>%
        select(-bldg_sq_fo) %>%
        right_join(x = ., y = sqft_impute_set, by = "building")
    
    dataset <- dataset %>%
        filter(!building  %in% sqft_impute_set$building) %>%
        bind_rows(sqft_impute_set) %>%
        filter(community_area != 0)
    
    dataset <- dataset %>%
        mutate(bldg_condi = ifelse(bldg_condi == "", "unknown", bldg_condi),
               bldg_condi = ifelse(bldg_condi == "unnhabitable", 
                                   "uninhabitable", bldg_condi))
    
    dataset$bldg_condi <- factor(dataset$bldg_condi, ordered = FALSE )
    dataset <- within(dataset, bldg_condi <- relevel(bldg_condi, ref = 3))
    
    dataset[is.na(dataset)] <- 0
    
    dataset$bldg_condi <- as.factor(dataset$bldg_condi)
    
    return(dataset)
}

# convert to numeric as necessary

building_footprints_2011_2016 <- read.csv("building_footprints_2011-2016.csv", stringsAsFactors = F)

building_footprints_2011_2016_clean <- impute_footprints(building_footprints_2011_2016)

# create modeling set
"
model_set <- building_footprints_2011_2016_clean %>%
    select(complaint_violation, bldg_sq_fo, shape_area, 
           stories, year_built, bldg_condi, 
           graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
           tree_trims_address, tree_debris_address, alley_lights_address, 
           garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
           abandoned_vehicles_address, abandoned_buildings_address, graffiti_community, one_light_community, pot_holes_community, 
           all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community, 
           garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
           abandoned_vehicles_community, abandoned_buildings_community, percent_aged_16_unemployed, 
           percent_aged_25_without_high_school_diploma, per_capita_income_,
           percent_households_below_poverty, percent_of_housing_crowded,
           percent_aged_under_18_or_over_64, complaint_violations, permit_violations, periodic_violations,
           registration_violations, total_buildings)
"

model_set <- building_footprints_2011_2016_clean %>%
    select(complaint_violation, bldg_sq_fo, shape_area, 
           stories, year_built, bldg_condi, 
           graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
           tree_trims_address, tree_debris_address, alley_lights_address, 
           garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
           abandoned_vehicles_address, abandoned_buildings_address, graffiti_community, one_light_community, pot_holes_community, 
           all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community, 
           garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
           abandoned_vehicles_community, abandoned_buildings_community, hardship_index, complaint_violations, permit_violations, periodic_violations,
           registration_violations, total_buildings)

model_set[, 19:30] <- model_set[, 19:30] / model_set$total_buildings
model_set$total_buildings <- NULL

model_set <- model_set %>%
    mutate(bldg_sq_fo = log(bldg_sq_fo),
           shape_area = log(shape_area),
           complaint_violations = log(complaint_violations+1),
           periodic_violations = log(periodic_violations+1),
           permit_violations = log(permit_violations+1),
           registration_violations = log(registration_violations+1),
           graffiti_address = log(graffiti_address+1),
           one_light_address = log(one_light_address+1),
           pot_holes_address = log(pot_holes_address+1),
           all_lights_address = log(all_lights_address+1), 
           tree_trims_address = log(tree_trims_address+1),
           tree_debris_address = log(tree_debris_address+1), 
           alley_lights_address = log(alley_lights_address+1), 
           garbage_carts_address = log(garbage_carts_address+1),
           sanitation_code_address = log(sanitation_code_address+1), 
           rodent_baiting_address = log(rodent_baiting_address+1), 
           abandoned_vehicles_address = log(abandoned_vehicles_address+1),
           abandoned_buildings_address = log(abandoned_buildings_address+1))

preprocessor <- preProcess(model_set, method='center')
model_centered <- predict(preprocessor, newdata = model_set)
model_set <- model_set %>%
    mutate(bldg_sq_fo = model_centered$bldg_sq_fo,
           shape_area = model_centered$shape_area,
           stories = model_centered$stories, 
           year_built = model_centered$year_built, 
           hardship_index = model_centered$hardship_index)
           


# build model

set.seed(1027)
data_split <- createDataPartition(y = model_set$complaint_violation, 
                                  p = .7, 
                                  list = F)

building_train <- model_set %>%
    dplyr::slice(data_split)

building_train <- as.data.table(building_train)

building_test <- model_set %>%
    dplyr::slice(-data_split)

building_test <- as.data.table(building_test)

building_logistic <- glm(complaint_violation ~ ., 
                         data = building_train, 
                         family = binomial("logit"))

summary(building_logistic)

building_results <- building_train %>%
    select(complaint_violation) %>%
    mutate(train_pred = ifelse(building_logistic$fitted.values > .027, 1, 0) %>%
               as.numeric())

confusionMatrix(as.factor(building_results$train_pred), 
                as.factor(building_train$complaint_violation), 
                positive = "1")

# transforming variables

ggplot(building_footprints_2011_2016_clean, aes(x=complaint_violations)) + geom_density()
ggplot(building_footprints_2011_2016_clean, aes(x=log(complaint_violations+1))) + geom_density()

ggplot(building_footprints_2011_2016_clean, aes(x=bldg_sq_fo)) + geom_density()
ggplot(building_footprints_2011_2016_clean, aes(x=log(bldg_sq_fo))) + geom_density()

ggplot(building_footprints_2011_2016_clean, aes(x=shape_area)) + geom_density()
ggplot(building_footprints_2011_2016_clean, aes(x=log(shape_area))) + geom_density()

ggplot(building_footprints_2011_2016_clean, aes(x=shape_len)) + geom_density()
ggplot(building_footprints_2011_2016_clean, aes(x=log(shape_len))) + geom_density()

ggplot(building_footprints_2011_2016_clean, aes(x=stories)) + geom_density()
ggplot(building_footprints_2011_2016_clean, aes(x=log(stories))) + geom_density()


# find better decision boundary

decision_boundary <- function(model, prob, data, y){
    suppressWarnings(
        results <- data %>%
            mutate(Class = y,
                   class_prob = predict.glm(model, newdata = data, type = "response"),
                   class_pred = ifelse(class_prob > prob, 1, 0))
    )
    results$class_pred <- as.factor(results$class_pred)
    results$Class <- as.factor(results$Class)
    
    conf_mat <- confusionMatrix(results$class_pred, results$Class)
    accuracy <- conf_mat$overall[1]
    
    #these get done in reverse order because pred is included first as opposed to class
    sens <- specificity(results$class_pred, results$Class)
    spec <- sensitivity(results$class_pred, results$Class)
    balance <- (sens + spec) / 2
    
    return(list(accuracy = accuracy,
                sensitivity = sens, 
                specificity = spec,
                balanced = balance))
}

decision_probs <- map_df(seq(0.01, .9, .01), 
                         function(x) decision_boundary(model = building_logistic, 
                                                       prob = x,
                                                       data = building_test,
                                                       y = building_test$complaint_violation))

decision_probs <- decision_probs %>%
    mutate(probability = (seq(0.01, .9, .01))) %>%
    select(probability, everything())

decision_probs %>%
    gather(key = "accuracy_metric", value = "values", -probability) %>%
    ggplot(aes(probability, values, colour = accuracy_metric)) +
    geom_line(size = 1.3, alpha = .45) +
    geom_vline(xintercept = 0.5, size = 1.3, alpha = .45, colour = "darkgray") +
    scale_x_continuous(breaks = seq(0, 1, .05), expand = c(.02, 0, 0.02, 0)) +
    scale_y_continuous(breaks = seq(0, 1, .1), expand = c(0, 0, 0, 0)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "Classification test accuracy metrics based on differing probability decision boundary for building logistic",
         y = NULL)

# building test

building_test <- building_test %>%
    mutate(model_probs = predict.glm(building_logistic, 
                                     newdata = building_test, type = "response"),
           preds = ifelse(model_probs > .027, 1, 0))

confusionMatrix(as.factor(building_test$preds), 
                as.factor(building_test$complaint_violation), 
                positive = "1")

# the test set appears to be robust to new data


# validate on 2018 data
# create the full model

building_logistic_full <- glm(complaint_violation ~ ., 
                              data = model_set, 
                              family = binomial("logit"))

summary(building_logistic_full)

model_set_output <- model_set %>%
    mutate(model_probs = predict.glm(building_logistic_full, 
                                     newdata = model_set, type = "response"),
           preds = ifelse(model_probs > .026, 1, 0))

confusionMatrix(as.factor(model_set_output$preds), 
                as.factor(model_set_output$complaint_violation), 
                positive = "1")

# convert to numeric as necessary

building_footprints_2012_2017 <- read.csv("building_footprints_2012-2017.csv", stringsAsFactors = F)

building_footprints_2012_2017_clean <- impute_footprints(building_footprints_2012_2017)

# create validation set

validation_set <- building_footprints_2012_2017_clean %>%
    select(complaint_violation, bldg_sq_fo, shape_area, 
           stories, year_built, bldg_condi, 
           graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
           tree_trims_address, tree_debris_address, alley_lights_address, 
           garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
           abandoned_vehicles_address, abandoned_buildings_address, graffiti_community, one_light_community, pot_holes_community, 
           all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community, 
           garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
           abandoned_vehicles_community, abandoned_buildings_community, hardship_index, complaint_violations, permit_violations, periodic_violations,
           registration_violations, total_buildings)

validation_set[, 19:30] <- validation_set[, 19:30] / validation_set$total_buildings
validation_set$total_buildings <- NULL

validation_set <- validation_set %>%
    mutate(bldg_sq_fo = log(bldg_sq_fo),
           shape_area = log(shape_area),
           complaint_violations = log(complaint_violations+1),
           periodic_violations = log(periodic_violations+1),
           permit_violations = log(permit_violations+1),
           registration_violations = log(registration_violations+1),
           graffiti_address = log(graffiti_address+1),
           one_light_address = log(one_light_address+1),
           pot_holes_address = log(pot_holes_address+1),
           all_lights_address = log(all_lights_address+1), 
           tree_trims_address = log(tree_trims_address+1),
           tree_debris_address = log(tree_debris_address+1), 
           alley_lights_address = log(alley_lights_address+1), 
           garbage_carts_address = log(garbage_carts_address+1),
           sanitation_code_address = log(sanitation_code_address+1), 
           rodent_baiting_address = log(rodent_baiting_address+1), 
           abandoned_vehicles_address = log(abandoned_vehicles_address+1),
           abandoned_buildings_address = log(abandoned_buildings_address+1))

validation_centered <- predict(preprocessor, newdata = validation_set)
validation_set <- validation_set %>%
    mutate(bldg_sq_fo = model_centered$bldg_sq_fo,
           shape_area = model_centered$shape_area,
           stories = model_centered$stories, 
           year_built = model_centered$year_built, 
           hardship_index = model_centered$hardship_index)


# building validation


validation_set_output <- validation_set %>%
    mutate(model_probs = predict.glm(building_logistic_full, 
                                     newdata = validation_set, type = "response"),
           preds = ifelse(model_probs > .026, 1, 0))

confusionMatrix(as.factor(validation_set_output$preds), 
                as.factor(validation_set_output$complaint_violation), 
                positive = "1")


# the accuracy is actually slightly higher than in the original model

# evaluate model

model_set_analysis <- model_set %>% 
    mutate(model_probs = predict(building_logistic_full, type = "response"),
           logit = log(model_probs/(1-model_probs)))


ggplot(model_set_analysis, aes(x=complaint_violations, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=bldg_sq_fo, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=shape_len, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=shape_area, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=year_built, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=stories, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=periodic_violations, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)

ggplot(model_set_analysis, aes(x=hardship_index, y=logit)) + geom_point(shape=1) + theme_minimal() + 
    geom_smooth(method = "auto", size = 1.5)


# linear discriminant analysis model

building_lda_full <- lda(complaint_violation ~ ., data = model_set)

lda_pred <- predict(building_lda_full, 
                    newdata = model_set, type = "response")

model_set_output <- model_set_output %>%
    mutate(lda_probs = predict(building_lda_full, 
                               newdata = model_set, type = "response")$posterior[, 2],
           lda_preds = ifelse(lda_probs > .015, 1, 0))

confusionMatrix(as.factor(model_set_output$lda_preds), 
                as.factor(model_set_output$complaint_violation), 
                positive = "1")

validation_set_output <- validation_set_output %>%
    mutate(lda_probs = predict(building_lda_full, 
                               newdata = validation_set, type = "response")$posterior[, 2],
           lda_preds = ifelse(lda_probs > .015, 1, 0))

confusionMatrix(as.factor(validation_set_output$lda_preds), 
                as.factor(validation_set_output$complaint_violation), 
                positive = "1")


# gradient boosting model

set.seed(1027)
building_gbm_full <- gbm(complaint_violation ~ ., distribution = 'bernoulli', data = model_set, n.trees = 200)

model_set_output <- model_set_output %>%
    mutate(gbm_probs = predict.gbm(building_gbm_full, 
                                   newdata = model_set, type = "response", n.trees = 200),
           gbm_preds = ifelse(gbm_probs > .023, 1, 0))

confusionMatrix(as.factor(model_set_output$gbm_preds), 
                as.factor(model_set_output$complaint_violation), 
                positive = "1")

validation_set_output <- validation_set_output %>%
    mutate(gbm_probs = predict.gbm(building_gbm_full, 
                                   newdata = model_set, type = "response", n.trees = 200),
           gbm_preds = ifelse(gbm_probs > .023, 1, 0))

confusionMatrix(as.factor(validation_set_output$gbm_preds), 
                as.factor(validation_set_output$complaint_violation),
                positive = '1')


# compare spearman correlations

cor(model_set_output$model_probs, model_set_output$lda_probs, method = 'spearman')
cor(model_set_output$model_probs, model_set_output$gbm_probs, method = 'spearman')
cor(model_set_output$lda_probs, model_set_output$gbm_probs, method = 'spearman')

cor(validation_set_output$model_probs, validation_set_output$lda_probs, method = 'spearman')
cor(validation_set_output$model_probs, validation_set_output$gbm_probs, method = 'spearman')
cor(validation_set_output$lda_probs, validation_set_output$gbm_probs, method = 'spearman')

# compare empiracl copulas

ggplot(validation_set_output, aes(x = rank(model_probs)/nrow(validation_set_output),
                                  y=rank(lda_probs)/nrow(validation_set_output),
                                  color=as.factor(complaint_violation))) + geom_point(shape=1) + theme_minimal() +
    scale_color_manual(values = c('royalblue', 'orange')) + xlab('logistic regression rank') + ylab('LDA rank') +
    ggtitle('Empirical copula between logistic regression and LDA',
            subtitle = 'Results show high rank correlation between outputs') +
    labs(color = "Complaint Status\n")
    

ggplot(validation_set_output, aes(x = rank(model_probs)/nrow(validation_set_output),
                                  y=rank(lda_probs)/nrow(validation_set_output))) + geom_hex() + theme_minimal()

ggplot(validation_set_output, aes(x = rank(model_probs)/nrow(validation_set_output),
                                  y=rank(gbm_probs)/nrow(validation_set_output),
                                  color=as.factor(complaint_violation))) + geom_point(shape = 1) + theme_minimal()

ggplot(validation_set_output, aes(x = rank(lda_probs)/nrow(validation_set_output),
                                  y=rank(gbm_probs)/nrow(validation_set_output),
                                  color=as.factor(complaint_violation))) + geom_point(shape= 1) + theme_minimal()


# gains chart log reg

building_lift <- gains(actual =  validation_set_output$complaint_violation,
                       predicted = validation_set_output$model_probs,
                       groups = 40)


lift_table <- as.data.frame(unlist(building_lift)) %>%
    rownames_to_column(var = "element") %>%
    slice(-441:-444)  %>%
    rename(values = "unlist(building_lift)") %>%
    mutate(row = rep(1:40, 11),
           values = as.character(values),
           values = round(as.numeric(values), 2),
           element = gsub(pattern = "([0-9]+)", replacement = "", x = element),
           element = factor(element, levels = names(building_lift)[1:11])) %>%
    spread(key = "element", value = "values") %>%
    select(-row, -min.prediction, -max.prediction)

ggplot(lift_table, aes(x=depth, y=lift)) + geom_line(col='dodgerblue2', size = 1.5) + geom_point(size=2) + 
    theme_minimal() + geom_line(aes(x=depth, y=mean(lift)))


# gains chart lda

building_lift_lda <- gains(actual =  model_set_output$complaint_violation,
                           predicted = model_set_output$lda_probs,
                           groups = 40)

lift_table_lda <- as.data.frame(unlist(building_lift_lda)) %>%
    rownames_to_column(var = "element") %>%
    slice(-441:-444)  %>%
    rename(values = "unlist(building_lift_lda)") %>%
    mutate(row = rep(1:40, 11),
           values = as.character(values),
           values = round(as.numeric(values), 2),
           element = gsub(pattern = "([0-9]+)", replacement = "", x = element),
           element = factor(element, levels = names(building_lift_lda)[1:11])) %>%
    spread(key = "element", value = "values") %>%
    select(-row, -min.prediction, -max.prediction)

ggplot(lift_table_lda, aes(x=depth, y=lift)) + geom_line() + theme_minimal()

# gains chart gbm

building_lift_gbm <- gains(actual =  model_set_output$complaint_violation,
                           predicted = model_set_output$gbm_probs,
                           groups = 40)

lift_table_gbm <- as.data.frame(unlist(building_lift_gbm)) %>%
    rownames_to_column(var = "element") %>%
    slice(-441:-444)  %>%
    rename(values = "unlist(building_lift_gbm)") %>%
    mutate(row = rep(1:40, 11),
           values = as.character(values),
           values = round(as.numeric(values), 2),
           element = gsub(pattern = "([0-9]+)", replacement = "", x = element),
           element = factor(element, levels = names(building_lift_gbm)[1:11])) %>%
    spread(key = "element", value = "values") %>%
    select(-row, -min.prediction, -max.prediction)

ggplot(lift_table_gbm, aes(x=depth, y=lift)) + geom_line() + theme_minimal() + geom_line(aes(x=depth, y=mean(lift)))

# AUC log reg


roc_df <- data.frame(roc(predictions = validation_set_output$model_probs,
                         labels = as.factor(validation_set_output$complaint_violation))[2],
                     roc(predictions = validation_set_output$model_probs,
                         labels = as.factor(validation_set_output$complaint_violation))[3])

building_auc <- round(auc(roc(validation_set_output$model_probs,
                              as.factor(validation_set_output$complaint_violation))), 3)


roc_df %>%
    ggplot(aes(fpr, tpr)) +
    geom_line(size = 1.3, colour = "dodgerblue2") +
    geom_abline(intercept = 0, slope = 1, size = 1.3,
                alpha = .5, colour = "darkorchid3") +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    labs(y = "sensitivity (true positive rate)",
         x = "1 - specificity (false positive rate)") +
    theme_minimal()


# AUC LDA

roc_df_lda <- data.frame(roc(predictions = validation_set_output$lda_probs,
                             labels = as.factor(validation_set_output$complaint_violation))[2],
                         roc(predictions = validation_set_output$lda_probs,
                             labels = as.factor(validation_set_output$complaint_violation))[3])

building_auc_lda <- round(auc(roc(model_set_output$lda_probs,
                                  as.factor(model_set_output$complaint_violation))), 3)

roc_df_lda %>%
    ggplot(aes(fpr, tpr)) +
    geom_line(size = 1.3, colour = "dodgerblue2") +
    geom_abline(intercept = 0, slope = 1, size = 1.3,
                alpha = .5, colour = "darkorchid3") +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    labs(title = paste0("ROC curve for Building Predictions LDA in Validation (2018): AUC = ", building_auc_lda),
         y = "sensitivity (true positive rate)",
         x = "1 - specificity (false positive rate)") +
    theme_minimal()

# AUC GBM

roc_df_gbm <- data.frame(roc(predictions = validation_set_output$gbm_probs,
                             labels = as.factor(validation_set_output$complaint_violation))[2],
                         roc(predictions = validation_set_output$gbm_probs,
                             labels = as.factor(validation_set_output$complaint_violation))[3])

building_auc_gbm <- round(auc(roc(model_set_output$gbm_probs,
                                  as.factor(model_set_output$complaint_violation))), 3)

roc_df_gbm %>%
    ggplot(aes(fpr, tpr)) +
    geom_line(size = 1.3, colour = "dodgerblue2") +
    geom_abline(intercept = 0, slope = 1, size = 1.3,
                alpha = .5, colour = "darkorchid3") +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    labs(title = paste0("ROC curve for Building Predictions GBM in Validation (2018): AUC = ", building_auc_gbm),
         y = "sensitivity (true positive rate)",
         x = "1 - specificity (false positive rate)") +
    theme_minimal()

# update new model set

building_logistic_2018 <- glm(complaint_violation ~ .,
                              data = validation_set,
                              family = binomial("logit"))

summary(building_logistic_2018)

output_2018 <- validation_set %>%
    mutate(logistic_probs = predict.glm(building_logistic_2018, 
                                        newdata = validation_set, type = "response"),
           logistic_preds = ifelse(logistic_probs > .027, 1, 0))

confusionMatrix(as.factor(output_2018$logistic_preds), 
                as.factor(output_2018$complaint_violation), 
                positive = "1")

# LDA

building_lda_2018 <- lda(complaint_violation ~ ., data = validation_set)


output_2018 <- output_2018 %>%
    mutate(lda_probs = predict(building_lda_2018, 
                               newdata = validation_set, type = "response")$posterior[, 2],
           lda_preds = ifelse(lda_probs > .015, 1, 0))

confusionMatrix(as.factor(output_2018$lda_preds), 
                as.factor(output_2018$complaint_violation), 
                positive = "1")
# gbm

building_gbm_2018 <- gbm(complaint_violation ~ ., distribution = 'bernoulli',
                         data = validation_set, n.trees = 200)

output_2018 <- output_2018 %>%
    mutate(gbm_probs = predict.gbm(building_gbm_full, 
                                   newdata = validation_set, type = "response", n.trees = 200),
           gbm_preds = ifelse(gbm_probs > .023, 1, 0))

confusionMatrix(as.factor(output_2018$gbm_preds), 
                as.factor(output_2018$complaint_violation), 
                positive = "1")

# save full data set

output_2018$bldg_id <- building_footprints_2012_2017_clean$bldg_id
output_2018$log_rank <- rank(output_2018$logistic_probs)/(length(output_2018$logistic_probs)+1)
output_2018$lda_rank <- rank(output_2018$lda_probs)/(length(output_2018$lda_probs)+1)
output_2018$gbm_rank <- rank(output_2018$gbm_probs)/(length(output_2018$gbm_probs)+1)

write.csv(output_2018, file='footprint_set_2018.csv', row.names = F)

write.csv(building_logistic_2018$coefficients, 'coefficients.csv')

# decision tree

building_tree <- rpart(complaint_violation ~ .,
                       control = rpart.control(minsplit = 10, cp = 0.001), 
                       data = model_set)

fancyRpartPlot(building_tree)


### penalization and addition of splines

options(prType="plain")
dd <- datadist(model_set)
options(datadist="dd")

building_logistic_enhanced <- lrm(complaint_violation ~ rcs(bldg_sq_fo, 3) + rcs(shape_area, 3) + 
                                  rcs(shape_len, 3) + rcs(stories, 3) + rcs(year_built, 3) + bldg_condi + 
                                  graffiti_address + one_light_address + pot_holes_address + all_lights_address + 
                                  tree_trims_address + tree_debris_address + alley_lights_address + 
                                  garbage_carts_address + sanitation_code_address + rodent_baiting_address + 
                                  abandoned_vehicles_address + graffiti_community + one_light_community + pot_holes_community + 
                                  all_lights_community + tree_trims_community + tree_debris_community + alley_lights_community + 
                                  garbage_carts_community + sanitation_code_community + rodent_baiting_community + 
                                  abandoned_vehicles_community + rcs(hardship_index, 4) + per_capita_income_ +
                                  percent_households_below_poverty + percent_of_housing_crowded +
                                  percent_aged_under_18_or_over_64 + rcs(complaint_violations, 3) + 
                                  rcs(permit_violations, 3) + rcs(periodic_violations, 3) +
                                  rcs(registration_violations, 3), data = building_train_tranformed)

summary(building_logistic_enhanced)

print(building_logistic_enhanced)

anova(building_logistic_enhanced)


# penalizing the fit is shown to not be effective
pentrace(building_logistic_enhanced, seq(.2, 1, by=0.05))



building_results_enhanced <- building_train_tranformed %>%
    select(complaint_violation) %>%
    mutate(train_pred = ifelse(predict(building_logistic_enhanced, type = 'fitted') > .027, 1, 0) %>%
               as.numeric())

confusionMatrix(as.factor(building_results_enhanced$train_pred), 
                as.factor(building_results_enhanced$complaint_violation), 
                positive = "1")

building_test_enhanced <- building_test_tranformed %>%
    mutate(model_probs = predict(building_logistic_enhanced, building_test_tranformed, type = "fitted"),
           preds = ifelse(model_probs > .027, 1, 0))

confusionMatrix(as.factor(building_test_enhanced$preds), 
                as.factor(building_test_enhanced$complaint_violation), 
                positive = "1")


# full model

building_logistic_enhanced <- lrm(complaint_violation ~ rcs(bldg_sq_fo, 3) + rcs(shape_area, 3) + 
                                      rcs(shape_len, 3) + rcs(stories, 3) + rcs(year_built, 3) + bldg_condi + 
                                      graffiti_address + one_light_address + pot_holes_address + all_lights_address + 
                                      tree_trims_address + tree_debris_address + alley_lights_address + 
                                      garbage_carts_address + sanitation_code_address + rodent_baiting_address + 
                                      abandoned_vehicles_address + graffiti_community + one_light_community + pot_holes_community + 
                                      all_lights_community + tree_trims_community + tree_debris_community + alley_lights_community + 
                                      garbage_carts_community + sanitation_code_community + rodent_baiting_community + 
                                      abandoned_vehicles_community + rcs(hardship_index, 4) + per_capita_income_ +
                                      percent_households_below_poverty + percent_of_housing_crowded +
                                      percent_aged_under_18_or_over_64 + rcs(complaint_violations, 3) + 
                                      rcs(permit_violations, 3) + rcs(periodic_violations, 3) +
                                      rcs(registration_violations, 3), data = model_set)

validation_set_output <- validation_set_output %>%
    mutate(model_probs_enhanced = predict(building_logistic_enhanced, validation_set, type = "fitted"),
           preds_enhanced = ifelse(model_probs_enhanced > .026, 1, 0))

# old

confusionMatrix(as.factor(validation_set_output$preds), 
                as.factor(validation_set_output$complaint_violation), 
                positive = "1")

# new

confusionMatrix(as.factor(validation_set_output$preds_enhanced), 
                as.factor(validation_set_output$complaint_violation), 
                positive = "1")

roc_df <- data.frame(roc(predictions = validation_set_output$model_probs_enhanced,
                         labels = as.factor(validation_set_output$complaint_violation))[2],
                     roc(predictions = validation_set_output$model_probs_enhanced,
                         labels = as.factor(validation_set_output$complaint_violation))[3])

building_auc <- round(auc(roc(validation_set_output$model_probs_enhanced,
                              as.factor(validation_set_output$complaint_violation))), 3)

roc_df %>%
    ggplot(aes(fpr, tpr)) +
    geom_line(size = 1.3, colour = "dodgerblue2") +
    geom_abline(intercept = 0, slope = 1, size = 1.3,
                alpha = .5, colour = "darkorchid3") +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    labs(title = paste0("ROC curve for Building Predictions GLM in Validation (2017): AUC = ", building_auc),
         y = "1 - sensitivity (true positive rate)",
         x = "1 - specificity (false positive rate)") +
    theme_minimal()

building_lift <- gains(actual =  validation_set_output$complaint_violation,
                       predicted = validation_set_output$model_probs_enhanced,
                       groups = 40)


lift_table <- as.data.frame(unlist(building_lift)) %>%
    rownames_to_column(var = "element") %>%
    slice(-441:-444)  %>%
    rename(values = "unlist(building_lift)") %>%
    mutate(row = rep(1:40, 11),
           values = as.character(values),
           values = round(as.numeric(values), 2),
           element = gsub(pattern = "([0-9]+)", replacement = "", x = element),
           element = factor(element, levels = names(building_lift)[1:11])) %>%
    spread(key = "element", value = "values") %>%
    select(-row, -min.prediction, -max.prediction)

ggplot(lift_table, aes(x=depth, y=lift)) + geom_line() + theme_minimal()


ggplot(validation_set_output) + geom_density(aes(x=model_probs), col = 'blue') +
    geom_density(aes(x=model_probs_enhanced), col = 'yellow') + theme_minimal()

cor(validation_set_output$model_probs, validation_set_output$model_probs_enhanced, method = 'spearman')

# VIF score

car::vif(building_logistic_2018)


