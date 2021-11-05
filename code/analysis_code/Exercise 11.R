
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

set.seed(123)
data_split <- initial_split(data, prop = 3/4, strata = BodyTemp)

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

# RMSE 1.20
rmse_test 






# We’ll fit a tree, a LASSO model, and a random forest. 
# I chose those because they are used in the tutorial on the tidymodels website.
# You can of course add further models. For the tree, see the Tune model 
# parameters section of the Get Started tutorial. 
# For LASSO and the random forest, check out the Case Study section 
# of the Get Started tutorial. Note that you will need to adjust the code
# for our scenario since we have a continuous outcome.

# If you follow the tutorial, you’ll likely use the packages rpart, 
# glmnet and ranger to fit those 3 models. Make sure they are installed 
# and loaded.I suggest you write code for each model separately. A lot of the 
# code will look similar, so once you got the first one set up, the other two
# should be easier. They mainly differ in the commands specifying the tuning 
# parameters and the tuning grid.Each of these models requires some tuning. 
# For the choices regarding the tuning parameters, you can follow the examples.
# Most of the models have more things that can be tuned, but for now you can 
# stick to what they show in the tutorial. Follow the examples by setting up a
# workflow, set a tuning grid, and then use the tune_grid() function to tune the 
# model using cross-validation.


# TREE 

# model specification
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")    # setting it to regression instead of classification

tune_spec


# tuning grid specification
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)


tree_grid %>% 
  count(tree_depth)




# Tune a workflow() that bundles together a model
# specification and a recipe or model preprocessor.
# Here we use a workflow() with a straightforward formula; 
# if this model required more involved data preprocessing, we could use add_recipe() instead of add_formula().


tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(bodytemp_cont_rec) # using predefined recipe

# tuning using cross-validation and the tune_grid() function
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res %>% 
  collect_metrics()


# Once you have done the tuning, you can take a look at some diagnostics
#by sending your object returned from the tune_grid() function to autoplot(). 
#For instance if you tuned the tree and saved the result as tree_tune_res,
#you can run tree_tune_res %>% autoplot(). Depending on the model, the plot
#will be different, but in general it shows you what happened during the tuning process.

tree_res %>% autoplot()



# Plotting Metrics
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# choosing best
tree_res %>%
  show_best(tree_res, metric = "rmse")


# selecting best
best_tree <- tree_res %>%
  select_best(tree_res, metric = "rsq")

best_tree



# finalizing model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf


final_fit <- 
  final_wf %>%
  last_fit(data_split) 

final_tree <- extract_workflow(final_fit)
final_tree


library(rpart.plot)

final_tree %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)


# estimate variable importance based on the model’s structure.

library(vip)

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()


#  fit this final model to the training data and use our test 
# data to estimate the model performance we expect to see with new data
final_fit <- 
  final_wf %>%
  last_fit(train_data) 

tree_train_fit <- final_wf %>% fit(data = train_data)




# Make two plots, one that shows model predictions from the tuned model 
# versus actual outcomes, and one that plots residuals. The actual outcomes 
# you get straight from the data, the predicted outcomes you can get by 
# applying the predict() function to the final fit.


tree_train_fit <- tree_res %>% fit(data = train)


# Look at/print the model performance and compare it with the null model 
# (still only on training data). Here, we want the performance of the tuned, 
# best-fitting model on the CV dataset (we are not yet touching the test data). 
# You can get that for instance with the show_best() function, which gives you
# the mean cross-validated performance for the best models. It also shows the 
# standard deviation for the performance. Compare that model performance with the null model.
# The mean and standard deviation of the performance give you a measure of overall performance 
# and variability in that measure. The plots show you if there are any systematic deviations
# between model and data. Taken together, these can be compared for the different models and 
# based on those (and as wanted, other considerations) a final model can be chosen.
# Implement the model tuning/fitting and evaluating steps for all 3 models.








# For the tree model, if you want to plot the tree, you can use the rpart.plot package 
# and run this command rpart.plot(extract_fit_parsnip(best_tree_fit)$fit) 
# (assuming your result from the final workflow fit is called best_tree_fit). 
# You might get a warning message, but the tree will show. You will likely find when you 
# look at the actual/predicted plot or the residual plot that the tree model does not perform
# very well, and the model only predicts a few discrete outcome values. That’s also noticeable 
# when you compare RMSE for the tree model and the null model, they are very similar.






# LASSO 

# model specification
tune_spec_LASSO <- 
  linear_reg(
    penalty = tune(),
    mixture = 1
  ) %>%
  set_engine("glmnet") %>% 
  set_mode("regression")    # setting it to regression instead of classification

# grid
lasso_grid <- grid_regular(penalty(), levels = 10)


# workflow definition
lr_workflow_LASSO <- 
  workflow() %>% 
  add_model(tune_spec_LASSO) %>% 
  add_recipe(bodytemp_cont_rec)




lr_LASSO <- 
  lr_workflow_LASSO %>% 
  tune_grid(folds,
            control = control_grid(save_pred = TRUE),
            grid = lasso_grid)


# workflow definition
lr_workflow_LASSO <- 
  workflow() %>% 
  add_model(lr_mod_LASSO) %>% 
  add_recipe(bodytemp_cont_rec)


# tuning grid specification
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tune_spec


tree_grid %>% 
  count(tree_depth)




# Tune a workflow() that bundles together a model
# specification and a recipe or model preprocessor.
# Here we use a workflow() with a straightforward formula; 
# if this model required more involved data preprocessing, we could use add_recipe() instead of add_formula().


tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(bodytemp_cont_rec) # using predefined recipe

# tuning using cross-validation and the tune_grid() function
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res %>% 
  collect_metrics()






# LASSO

# model specification
lr_mod_LASSO <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")



# workflow definition
lr_workflow_LASSO <- 
  workflow() %>% 
  add_model(lr_mod_LASSO) %>% 
  add_recipe(bodytemp_cont_rec)


# grid
lasso_grid <- grid_regular(penalty(), levels = 10)


lr_LASSO <- 
  lr_workflow_LASSO %>% 
  tune_grid(folds,
            control = control_grid(save_pred = TRUE),
            grid = lasso_grid)



# Grid search for parameter values
lasso_grid_res <- lr_workflow_LASSO  %>% 
  tune_grid(
    resamples = folds,
    control = control_grid(verbose = TRUE),
    grid = lasso_grid
  )

lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid)
tree_res %>% 
  collect_metrics()



# tuning grid specification 
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5)

lr_reg_grid %>% top_n(5) 


# tuning using cross-validation and the tune_grid() function
lr_res <- 
  lr_workflow_LASSO %>% 
  tune_grid(cell_folds,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))



LASSO_res <- 
  lr_workflow_LASSO %>% 
  tune_grid(
    resamples = cell_folds,
    grid = lr_reg_grid
  )



lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))


tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")    # setting it to regression instead of classification

# tuning grid specification
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tune_spec


tree_grid %>% 
  count(tree_depth)


set.seed(123)

cell_folds <- vfold_cv(train_data)



# Tune a workflow() that bundles together a model
# specification and a recipe or model preprocessor.
# Here we use a workflow() with a straightforward formula; 
# if this model required more involved data preprocessing, we could use add_recipe() instead of add_formula().

# workflow definition
set.seed(123)

tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(bodytemp_cont_rec) # using predefined recipe

# tuning using cross-validation and the tune_grid() function
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )

tree_res %>% 
  collect_metrics()


# Plotting Metrics
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# choosing best
tree_res %>%
  show_best(tree_res, metric = "rmse")


# selecting best
best_tree <- tree_res %>%
  select_best(tree_res, metric = "rsq")

best_tree

# finalizing model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf

#  fit this final model to the training data and use our test 
# data to estimate the model performance we expect to see with new data
final_fit <- 
  final_wf %>%
  last_fit(train_data) 














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
  add_recipe(bodytemp_cont_rec)


rf_mod %>%    
  parameters()  


set.seed(123)

rf_res <- 
  rf_workflow %>% 
  tune_grid(folds,
            grid = 25,
            control = control_grid(save_pred = TRUE))

# model specification
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")    # setting it to regression instead of classification

# tuning grid specification
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tune_spec


tree_grid %>% 
  count(tree_depth)




# Tune a workflow() that bundles together a model
# specification and a recipe or model preprocessor.
# Here we use a workflow() with a straightforward formula; 
# if this model required more involved data preprocessing, we could use add_recipe() instead of add_formula().


tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(bodytemp_cont_rec) # using predefined recipe

# tuning using cross-validation and the tune_grid() function
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res %>% 
  collect_metrics()










