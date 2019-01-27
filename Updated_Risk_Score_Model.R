library(tidyverse)
library(data.table)
library(caret)
library(reticulate)
library(GGally)
library(gains)

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
        mutate(bldg_condi = ifelse(bldg_condi == "unnhabitable", 
                                   "uninhabitable", bldg_condi))
    dataset[is.na(dataset)] <- 0
    
    dataset$bldg_condi <- as.factor(dataset$bldg_condi)
    
    return(dataset)
}

# convert to numeric as necessary

building_footprints_2011_2016 <- read.csv("building_footprints_2011-2016.csv", stringsAsFactors = F)

building_footprints_2011_2016_clean <- impute_footprints(building_footprints_2011_2016)

# create modeling set

model_set <- building_footprints_2011_2016_clean %>%
    select(complaint_violation, bldg_sq_fo, shape_area, 
           shape_len, stories, year_built, bldg_condi, 
           graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
           tree_trims_address, tree_debris_address, alley_lights_address, 
           garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
           abandoned_vehicles_address, graffiti_community, one_light_community, pot_holes_community, 
           all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community, 
           garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
           abandoned_vehicles_community,hardship_index, per_capita_income_,
           percent_households_below_poverty, percent_of_housing_crowded,
           percent_aged_under_18_or_over_64, complaint_violations, permit_violations, periodic_violations,
           registration_violations, total_buildings)

model_set[, 19:29] <- model_set[, 19:29] / model_set$total_buildings
model_set$total_buildings <- NULL

# build model

set.seed(1027)
data_split <- createDataPartition(y = model_set$complaint_violation, 
                                  p = .7, 
                                  list = F)

building_train <- model_set %>%
    slice(data_split)

building_train <- as.data.table(building_train)

building_test <- model_set %>%
    slice(-data_split)

building_test <- as.data.table(building_test)

building_logistic <- glm(complaint_violation ~ ., 
                         data = building_train, 
                         family = binomial("logit"))

summary(building_logistic)

building_results <- building_train %>%
    select(complaint_violation) %>%
    mutate(train_pred = ifelse(building_logistic$fitted.values > .024, 1, 0) %>%
               as.numeric())

confusionMatrix(as.factor(building_results$train_pred), 
                as.factor(building_train$complaint_violation), 
                positive = "1")

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
         subtitle = "Highest accuracy (~83%) comes from boundary cut off at .46; Best balanced result (~70%) at .18",
         y = NULL)

# building test

building_test <- building_test %>%
    mutate(model_probs = predict.glm(building_logistic, 
                                     newdata = building_test, type = "response"),
           preds = ifelse(model_probs > .024, 1, 0))

confusionMatrix(as.factor(building_test$preds), 
                as.factor(building_test$complaint_violation), 
                positive = "1")


# validate on 2018 data
# create the full model

building_logistic_full <- glm(complaint_violation ~ ., 
                         data = model_set, 
                         family = binomial("logit"))

# convert to numeric as necessary

building_footprints_2012_2017 <- read.csv("building_footprints_2012-2017.csv", stringsAsFactors = F)

building_footprints_2012_2017_clean <- impute_footprints(building_footprints_2012_2017)

# create validation set

validation_set <- building_footprints_2012_2017_clean %>%
    select(complaint_violation, bldg_sq_fo, shape_area, 
           shape_len, stories, year_built, bldg_condi, 
           graffiti_address, one_light_address, pot_holes_address, all_lights_address, 
           tree_trims_address, tree_debris_address, alley_lights_address, 
           garbage_carts_address, sanitation_code_address, rodent_baiting_address, 
           abandoned_vehicles_address, graffiti_community, one_light_community, pot_holes_community, 
           all_lights_community, tree_trims_community, tree_debris_community, alley_lights_community, 
           garbage_carts_community, sanitation_code_community, rodent_baiting_community, 
           abandoned_vehicles_community,hardship_index, per_capita_income_,
           percent_households_below_poverty, percent_of_housing_crowded,
           percent_aged_under_18_or_over_64, complaint_violations, permit_violations, periodic_violations,
           registration_violations, total_buildings)

validation_set[, 19:29] <- validation_set[, 19:29] / validation_set$total_buildings
validation_set$total_buildings <- NULL


# building validation



validation_set <- validation_set %>%
    mutate(model_probs = predict.glm(building_logistic_full, 
                                     newdata = validation_set, type = "response"),
           preds = ifelse(model_probs > .024, 1, 0))

confusionMatrix(as.factor(validation_set$preds), 
                as.factor(validation_set$complaint_violation), 
                positive = "1")



## complaint violations

#       0      1
# 0 310196   3893
# 1 106635   6101
# Balanced Accuracy : 0.67732 

## any violations

#       0      1
# 0 272453   3847
# 1 141651   8874
# Balanced Accuracy : 0.67776 

write.csv(model_set, file='model_set.csv')
write.csv(validation_set, file='validation_set.csv')

# compare validation probailities

validation_set <- read.csv('validation_set.csv', header = T)
lda_val <- read.csv('lda_val_probs.csv', header = T)
qda_val <- read.csv('qda_val_probs.csv', header = T)
svm_val <- read.csv('svm_val_probs.csv', header = T)
rf_val <- read.csv('rf_val_probs.csv', header = T)




ggplot(validation_set, aes(x=model_probs)) + geom_density()
ggplot(lda_val, aes(x=probs)) + geom_density()
ggplot(qda_val, aes(x=probs)) + geom_density()
ggplot(svm_val, aes(x=probs)) + geom_density()
ggplot(rf_val, aes(x=probs)) + geom_density()

validation_set$lda <- lda_val$probs
validation_set$qda <- qda_val$probs
validation_set$svm <- svm_val$probs
validation_set$rf <- rf_val$probs

str(validation_set)

ggplot(validation_set, aes(x=lda, y=model_probs, color=as.factor(complaint_violation))) + geom_point()
ggplot(validation_set, aes(x=qda, y=model_probs)) + geom_point()
ggplot(validation_set, aes(x=svm, y=model_probs)) + geom_point()
ggplot(validation_set, aes(x=rf, y=model_probs, color=complaint_violation)) + geom_point()


# compare spearman correlations

cor(validation_set$model_probs, validation_set$lda, method = 'spearman')
cor(validation_set$model_probs, validation_set$rf, method = 'spearman')
cor(validation_set$lda, validation_set$rf, method = 'spearman')


# compare RF and LDA confusion matrix

validation_set <- validation_set %>%
    mutate(preds_lda = ifelse(lda > .45, 1, 0))

confusionMatrix(as.factor(validation_set$preds_lda), 
                as.factor(validation_set$complaint_violation), 
                positive = "1")


validation_set <- validation_set %>%
    mutate(preds_rf = ifelse(rf > .45, 1, 0))

confusionMatrix(as.factor(validation_set$preds_rf), 
                as.factor(validation_set$complaint_violation), 
                positive = "1")

# gains chart

building_lift <- gains(actual =  validation_set$complaint_violation,
                       predicted = validation_set$model_probs,
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

ggplot(lift_table, aes(x=depth, y=lift)) + geom_line()

# AUC

library(AUC)

roc_df <- data.frame(roc(predictions = validation_set$model_probs,
                         labels = as.factor(validation_set$complaint_violation))[2],
                     roc(predictions = validation_set$model_probs,
                         labels = as.factor(validation_set$complaint_violation))[3])

building_auc <- round(auc(roc(validation_set$model_probs,
                              as.factor(validation_set$complaint_violation))), 3)

roc_df %>%
    ggplot(aes(fpr, tpr)) +
    geom_line(size = 1.3, colour = "dodgerblue2") +
    geom_abline(intercept = 0, slope = 1, size = 1.3,
                alpha = .5, colour = "darkorchid3") +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand = c(.03, 0, 0, .03)) +
    labs(title = paste0("ROC curve for Building Predictions GLM in Validation (2017): AUC = ", building_auc),
         y = "1 - sensitivity (true positive rate)",
         x = "1 - specificity (false positive rate)")
