#Model Evaluation

#loading packages
library(here) #for data loading/saving
library(tidyverse)
library(recipes)
library(tidymodels)
library(workflowr) 
library(parsnip)
library(rsample)

#first loading in processed data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
data <- readRDS(data_location)

#checking
glimpse(data)


# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(397)
# Put 3/4 of the data into the training set 
data_split <- initial_split(data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Creates a simple recipe that fits our categorical outcome of interest to all predictors 
nausea_rec <- 
  recipe(Nausea ~ ., data = train_data) 


# Set a model as we did in the previous exercise
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")


# Use the workflow() package to create a
# simple workflow that fits a logistic model
# to all predictors using the glm function
nausea_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(nausea_rec)

# Fitting the model
nausea_fit <- 
  nausea_wflow %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
nausea_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()



# On Training Data

# Obtaining Predictions
predict(nausea_fit, train_data)

# Obtaining Predicted probablities 
predict(nausea_fit, train_data, type= "prob")

#using augment
nausea_aug <- 
  augment(nausea_fit, train_data)

nausea_aug %>%
  select(Nausea, .pred_Yes, .pred_No)

# Plotting
nausea_roc_train <- nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_Yes, event_level="second") %>% 
  autoplot()

nausea_roc_train

# Calculating Area under Curve
nausea_auc_train <-  nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level="second")


#The ROC-AUC of 0.789 indicates that the model might be useful
nausea_auc_train


# Now on Test Data

# Obtaining Predictions
predict(nausea_fit, test_data)

# Obtaining Predicted probablities 
predict(nausea_fit, test_data, type= "prob")

 #using augment
nausea_aug <- 
  augment(nausea_fit, test_data)

nausea_aug %>%
  select(Nausea, .pred_Yes, .pred_No)

# Plotting
nausea_roc_test <- nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_Yes, event_level= "second") %>% 
  autoplot()

nausea_roc_test

# Calculating Area under ROC
nausea_auc_test <- nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level= "second") 

#The ROC-AUC of 0.707 indicates that the model might be useful
nausea_auc_test






# NOW WITH ONLY ONE PREDICTOR
nausea_runny_rec <- 
  recipe(Nausea ~ RunnyNose, data = train_data) 


# Set a model as you did in the previous exercise, then use the workflow() package to create a
# simple workflow that fits a logistic model to all predictors using the glm function

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

nausea_runny_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(nausea_runny_rec)

# Fitting the model
nausea_runny_fit <- 
  nausea_runny_wflow %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
nausea_runny_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# On Training Data

# Obtaining Predictions
predict(nausea_runny_fit, train_data)

# Obtaining Predicted probablities 
predict(nausea_runny_fit, train_data, type= "prob")

#using augment
nausea_runny_aug <- 
  augment(nausea_runny_fit, train_data)

nausea_runny_aug %>%
  select(Nausea, .pred_Yes, .pred_No)

# Plotting
nausea_runny_roc_train <- nausea_runny_aug %>% 
  roc_curve(truth = Nausea, .pred_Yes, event_level= "second") %>% 
  autoplot()

nausea_runny_roc_train

# Calculating Area under Curve
nausea_runny_auc_train <- nausea_runny_aug %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level= "second")

#The ROC-AUC of 0.501 indicates that the model is not good
nausea_runny_auc_train



# Now on Test Data

# Obtaining Predictions
predict(nausea_runny_fit, test_data)

# Obtaining Predicted probablities 
predict(nausea_runny_fit, test_data, type= "prob")

#using augment
nausea_runny_aug <- 
  augment(nausea_runny_fit, test_data)

nausea_runny_aug %>%
  select(Nausea, .pred_Yes, .pred_No)

# Plotting
nausea_runny_roc_test <- nausea_runny_aug %>% 
  roc_curve(truth = Nausea, .pred_Yes, event_level= "second") %>% 
  autoplot()

nausea_runny_roc_test

# Calculating Area under ROC
nausea_runny_auc_test <- nausea_runny_aug %>% 
  roc_auc(truth = Nausea, .pred_Yes, event_level= "second") 

# The ROC-AUC of 0.476 indicates that the model is not good
nausea_runny_auc_test



