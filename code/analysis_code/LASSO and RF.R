

# The following script first loads the processed data, performs  a few more necessary data
# managment steps. Then we run the null model, followed by the LASS0
# and the Random Forest.

# The LASSO and Random Forest would not run on my computer. Everytime I tried to 
# run the tune_grid() it would abort R. I tried multiple ways and seeked help, but nothing would work.
# I'm not sure if it's my computer. 

# I provided code below that might work on a different computer. As far as I can tell,
# I have everything set up correct and it should be working. 

#loading packages
library(here) #for data loading/saving
library(tidyverse)
library(recipes)
library(tidymodels)
library(workflowr) 
library(parsnip)
library(rsample)
library(rpart)
library(glmnet)
library(ranger)


#first loading in processed data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
data <- readRDS(data_location)

#checking
glimpse(data)


# For those symptoms where you have both multiple levels and yes/no, 
# remove all the yes/no versions. That should remove 4 variables.


data <- select(data, -c(WeaknessYN, CoughYN, CoughYN2, MyalgiaYN))

glimpse(data)

# We also want to code the 3 ordinal/multi-level factors as ordered, 
# so make sure they are coded as ordered factors. 
# The order should of course be None/Mild/Moderate/Severe.

#as.factor(data$Weakness)
#table(data$Weakness)

#as.factor(data$CoughIntensity)
#table(data$CoughIntensity)

#as.factor(data$Myalgia)
#table(data$Myalgia)


data <- mutate(data, Weakness = factor(Weakness, levels = c("None", "Mild",
                                                            "Moderate","Severe"),ordered = TRUE))
data <- mutate(data, CoughIntensity = factor(CoughIntensity, levels = c("None", "Mild",
                                                                        "Moderate","Severe"),ordered = TRUE))
data <- mutate(data, Myalgia = factor(Myalgia, levels = c("None", "Mild",
                                                          "Moderate","Severe"),ordered = TRUE))

# But it’s often better to decide manually for each variable based on your scientific 
# expertise if you want to remove it or not. We’ll take that approach here.
# After looking at the data, we decide to remove those binary predictors that 
# have <50 entries in one category (there are 2). Write code to remove them.

library(table1) #loading the table 1 package

table1 <- table1(~ . , data=data, overall="Total")
table1

# Looks like hearing and vision both don't have more than 50 yes
data <- select(data, -c(Hearing, Vision))

glimpse(data)

# Start by setting the random seed to 123.
# This should make everything reproducible and
# everyone should get the same results.

set.seed(123)


# Split the dataset into 70% training, 30% testing. 
# Also use the outcome BodyTemp as stratification. 
# This allows for more balanced outcome values in the train and test sets.
# See e.g., section 3 of the Get Started tutorial.

library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(modeldata)  # for the cells data

data_split <- initial_split(data, prop = .7, strata = BodyTemp)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


# We want to do 5-fold cross-validation, 5 times repeated.
# (There’s no specific reason to do this 5x5 pattern, other 
# than to show you that there are different ways to pick the sample,
# and that I want you to not use the default.) For the CV folds, 
# we also want to stratify on BodyTemp, as we did for the main train/test split.
# Use the vfold_cv function to create a resample object for the training data 
# with these specifications.

folds <- vfold_cv(train_data, v = 5, r=5, strata= "BodyTemp")
folds


# Create a recipe for the data and fitting. You won’t need to do much, 
# just make sure you code the categorical variables as dummy variables,
# otherwise things might not work smoothly. For that, you want to use 
# the step_dummy function and pick all nominal predictor variables 
# (which are actually all variables here, since the only continuous
# variable is our outcome).


#Recipe() has two arguments: a formula and the data
bodytemp_cont_rec <- recipe(BodyTemp ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) #adding step_dummy




#Build a model specification using the parsnip package
lm_mod <- linear_reg() %>%
  set_engine("lm") 

#Model workflow pairs a model and recipe together
bodytemp_cont_workflow <- 
  workflow() %>%
  add_model(lm_mod) %>%
  add_recipe(bodytemp_cont_rec)


# Write some code to compute the performance of a null model, i.e. 
# a “model” that doesn’t use any predictor information. For a 
# continuous outcome and RMSE as our metric, a null model is one 
# that always predicts the mean of the outcome. Compute the RMSE 
# for both training and test data for such a “model”. We’ll use that 
# later to compare it to the performance of our real models. 
# Of course, we expect/hope our real models that use predictor 
# information to be better. If they aren’t that means they are no good.


# Creates a simple recipe that fits null model
bodytmp_rec_null <- recipe(BodyTemp ~  1 , data = train_data)

# Set a model as we did in the previous exercise
lr_mod <- 
  linear_reg() %>% 
  set_engine("lm")


# Use the workflow() package to create a
# simple workflow that fits a linear model
# to all predictors using the glm function
bodytmp_wflow_null <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(bodytmp_rec_null)

# Fitting the model
bodytmp_fit_null <- 
  bodytmp_wflow_null %>% 
  fit(data = train_data)

# Extracting Model/Recipes with Parsnip
bodytmp_fit_null %>% 
  extract_fit_parsnip() %>% 
  tidy()



# Obtaining Predictions
predict(bodytmp_fit_null, train_data)

bodytmp_aug_null <- 
  augment(bodytmp_fit_null, train_data)

bodytmp_aug_null %>%
  select(BodyTemp)


# Calculating Root RMSE 
rmse_train <- bodytmp_aug_null %>% 
  rmse(truth = BodyTemp, .pred)

# RMSE 1.19
rmse_train



# Now on Test Data


# Obtaining Predictions
predict(bodytmp_fit_null, test_data)

bodytmp_aug_null <- 
  augment(bodytmp_fit_null, test_data)

bodytmp_aug_null %>%
  select(BodyTemp)


# Calculating Root RMSE 
rmse_test <- bodytmp_aug_null %>% 
  rmse(truth = BodyTemp, .pred)

# RMSE 1.16
rmse_test 



# NOW THE LASSO AND RANDOM FOREST



# LASSO 

# model specification
tune_spec_LASSO <- 
  linear_reg( penalty = tune(),mixture = 1) %>%
  set_engine("glmnet") %>% 
  set_mode("regression")    # setting it to regression instead of classification

# grid
lasso_reg_grid <- tibble(penalty = 10^seq(-3, 0, length.out = 30))

# workflow definition
lr_workflow_LASSO <- workflow() %>% 
  add_model(tune_spec_LASSO) %>% 
  add_recipe(bodytemp_cont_rec)


# Tuning
lr_LASSO <- 
  lr_workflow_LASSO %>% 
  tune_grid(folds,
            control = control_grid(save_pred = TRUE),
            grid = lasso_reg_grid, 
            metrics = metric_set(rmse))


# Collecting Metrics
lr_LASSO %>% 
  collect_metrics()


#plotting metrics
lr_LASSO %>% autoplot()



# Plotting Metrics again (looks a bit nicer)
lr_LASSO %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


# Next, you want to get the model that the tuning process has determined 
# is the best. You can get the best-fit model with select_best() 
# and finalize_workflow() and then do one more fit to the training data with 
# this final workflow using the fit() function. Follow the examples in the tutorial.

# selecting best
best_LASSO <- lr_LASSO %>%
  select_best(lr_LASSO, metric = "rsq")

best_LASSO



# finalizing model
final_LASSO <- 
  lr_workflow_LASSO %>% 
  finalize_workflow(best_LASSO)

final_LASSO

# one more fit to the training data with 
# this final workflow using the fit() function

final_fit_LASSO <- 
  lr_workflow_LASSO %>%
  last_fit(data_split) 

# RMSE= 1.23, not much different from the null
final_fit_LASSO %>%
  collect_metrics()

#Collecting Predictions
LASSO_pred <- final_fit_LASSO %>%
  collect_predictions() 



# Make two plots, one that shows model predictions from the tuned model 
# versus actual outcomes

ggplot(data=LASSO_pred, aes(x=BodyTemp, y=.pred)) + geom_point() +
  labs(title= "Plot of LASSO Model Predictions from Tuned Model vs Actual Outcomes ", 
       x= "Actual Outcomes", y= "Model Predictions") 

#calculating residuals
LASSO_pred$resid <- LASSO_pred$BodyTemp - LASSO_pred$.pred 

# one that plots residuals.
# plotting residuals 
ggplot(data=LASSO_pred, aes(x=.pred, y=resid)) + geom_point() +
  labs(title= "Plot of LASSO Model Residuals vs Predictions", 
       x= "Model Predictions", y= "Residuals") 


# RMSE= ?????
show_best(final_fit_LASSO, metric= "rmse")

# Null Model. RMSE 1.21
rmse_test 









# Random Forest

# model specification

cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")


# workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(bodytemp_cont_rec1)


#grid

rf_grid  <- expand.grid(mtry = c(3, 4, 5, 6),
                        min_n = c(40,50,60), trees = c(500,1000)  )

# Tuning 
rf_res <- 
  rf_workflow %>% 
  tune_grid(folds,
            grid = 25,
            control = control_grid(verbose = TRUE, save_pred = TRUE))

#collecting metrics
rf_res %>% 
  collect_metrics()


#plotting metrics
rf_res %>% autoplot()



# Plotting Metrics again (looks a bit nicer)
rf_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


# Next, you want to get the model that the tuning process has determined 
# is the best. You can get the best-fit model with select_best() 
# and finalize_workflow() and then do one more fit to the training data with 
# this final workflow using the fit() function. Follow the examples in the tutorial.

# selecting best
best_rf <- rf_res %>%
  select_best(rf_res, metric = "rsq")

best_rf



# finalizing model
final_rf <- 
  rf_workflow %>% 
  finalize_workflow(best_rf)

final_rf

# one more fit to the training data with 
# this final workflow using the fit() function

final_fit_rf <- 
  rf_workflow %>%
  last_fit(data_split) 

# RMSE= 1.23, not much different from the null
final_fit_rf %>%
  collect_metrics()

#Collecting Predictions
rf_pred <- final_fit_rf %>%
  collect_predictions() 



# Make two plots, one that shows model predictions from the tuned model 
# versus actual outcomes

ggplot(data=rf_pred, aes(x=BodyTemp, y=.pred)) + geom_point() +
  labs(title= "Plot of Random Forest Model Predictions from Tuned Model vs Actual Outcomes ", 
       x= "Actual Outcomes", y= "Model Predictions") 

#calculating residuals
LASSO_pred$resid <- LASSO_pred$BodyTemp - LASSO_pred$.pred 

# one that plots residuals.
# plotting residuals 
ggplot(data=rf_pred, aes(x=.pred, y=resid)) + geom_point() +
  labs(title= "Plot of Random Forest Model Residuals vs Predictions", 
       x= "Model Predictions", y= "Residuals") 


# RMSE= ?????
show_best(final_fit_rf, metric= "rmse")

# Null Model. RMSE 1.21
rmse_test 






