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

#Gabriella Veytsel************************************************************************************
#*****************************************************************************************************
#Fit linear models to the continuous outcome: BodyTemp

#Recipe() has two arguments: a formula and the data
bodytemp_cont_rec <- recipe(BodyTemp ~ ., data = train_data) #all predictors
bodytemp_cont_main_rec <- recipe(BodyTemp ~ RunnyNose, data = train_data) #main predictor

#Build a model specification using the parsnip package
lm_mod <- linear_reg() %>%
  set_engine("lm")

#Model workflow pairs a model and recipe together
bodytemp_cont_workflow <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(bodytemp_cont_rec)

#Main predictor
bodytemp_cont_main_workflow <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(bodytemp_cont_main_rec)
  
bodytemp_cont_workflow
bodytemp_cont_main_workflow

#The last_fit() function will fit the model to the training data and 
  #calculate the prediction on the test data
bodytemp_cont_fit_alternative <- 
  bodytemp_cont_workflow %>%
  last_fit(split = data_split)

#Main predictor
bodytemp_cont_fit_main_alternative <- 
  bodytemp_cont_main_workflow %>%
  last_fit(split = data_split)

#Extract the fitted model object and use tidy() to get a tidy tibble 
  #of model coefficients
bodytemp_cont_fit_alternative %>%
  extract_fit_parsnip() %>%
  tidy()  

#Main predicotr
bodytemp_cont_fit_main_alternative %>%
  extract_fit_parsnip() %>%
  tidy()  

#Model Evaluaton:
#Look at predictions and RMSE for my data
#****************************************

#Collect predictions
test_results <- bodytemp_cont_fit_alternative %>%
  collect_predictions()

#Main predictor
test_results_main <- bodytemp_cont_fit_main_alternative %>%
  collect_predictions()

test_results
test_results_main

#Collect metrics
bodytemp_cont_fit_alternative %>%
  collect_metrics() #rmse = 1.2, r-squared = 0.0266

#Main predictor performs worse according to residual squared error and r-squared
bodytemp_cont_fit_main_alternative %>%
  collect_metrics() #rmse = 1.18, r-squared = 0.00689 

#R-squared plot to visualize model performance on the test dataset
#Does this scatterplot still make sense when there are binary precictors included?
test_results %>%
  ggplot(aes(x = .pred, y = BodyTemp)) + 
  geom_abline(lty=2) + 
  geom_point(color = "blue", alpha = 0.5) +
  coord_obs_pred() +
  labs(title = 'Linear Regression Results, all', 
       x = "Predicted Body Temp",
       y = "Actual Body Temp")

#Apply prediction to the training data instead
#The following steps are no longer necessary when using the above last_fit function:
#***********************************************************************************

#Models are the same, workflows are the same, just change fit()
#Not using last_fit() this time, so it won't automatically calculate predictions on the test data
#Now have to add predict() step

bodytemp_cont_fit <- 
  bodytemp_cont_workflow %>%
  fit(data = train_data) 

#Main predictor
bodytemp_cont_fit_main <- 
  bodytemp_cont_main_workflow %>%
  fit(data = train_data) 

#Extract the fitted model object and use tidy() to get a tidy tibble 
#of model coefficients
bodytemp_cont_fit %>%
  extract_fit_parsnip() %>%
  tidy()  

#Main predicotr
bodytemp_cont_fit_main %>%
  extract_fit_parsnip() %>%
  tidy()  

#Use the trained workflow to predict with the *trained data* (isntead of the test data)
#Returns predicted class
predict(bodytemp_cont_fit, train_data)
predict(bodytemp_cont_fit_main, train_data) #main predictor

#Returns predicted class probabiliies
#Compare the observed alue and the predicted value
bodytemp_cont_aug <- augment(bodytemp_cont_fit, train_data) %>%
  select(BodyTemp, .pred)
bodytemp_cont_aug

rmse(bodytemp_cont_aug, truth = BodyTemp, estimate = .pred) #1.11
rsq(bodytemp_cont_aug, truth = BodyTemp,  estimate = .pred) #0.152

#Main predictor
bodytemp_cont_aug_main <- augment(bodytemp_cont_fit_main, train_data) %>%
  select(BodyTemp, .pred)
bodytemp_cont_aug_main

rmse(bodytemp_cont_aug_main, truth = BodyTemp, estimate = .pred) #1.19
rsq(bodytemp_cont_aug_main, truth = BodyTemp,  estimate = .pred) #0.0147

#Goal is to minimize the root mean square error (RMSE) and maximize r-squared, so
  #the model with only RunnyNose as the predictor performs worse than keeping all predictors

#R-squared plot to visualize model performance on the training dataset
#Does this scatterplot still make sense when there are binary precictors included?
bodytemp_cont_aug %>%
  ggplot(aes(x = .pred, y = BodyTemp)) + 
  geom_abline(lty=2) + 
  geom_point(color = "blue", alpha = 0.5) +
  coord_obs_pred() +
  labs(title = 'Linear Regression Results, Training, all', 
       x = "Predicted Body Temp",
       y = "Actual Body Temp")
