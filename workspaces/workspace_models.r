# Prepare a dataset useful for modeling and prediction
model_ds <- shots %>%
  left_join(distinct(games_details %>% select(TEAM_ID, TEAM_ABBREVIATION))) %>%
  left_join(EV_info) %>%
  group_by(GAME_ID, SEASON_2) %>%
  summarize(EV_home = sum(EV * (HOME_TEAM == TEAM_ABBREVIATION)),
            EV_away = sum(EV * (AWAY_TEAM == TEAM_ABBREVIATION)),
            .groups = "drop") %>%
  inner_join(games) %>%
  select(-GAME_ID, -SEASON_2,
         -PTS_home, -PTS_away)

# set.seed(123)
# split <- initial_split(model_ds, strata = WIN_home)
# train <- training(split)
# test <- testing(split)

# Split the data in training and testing based on the seasons
train_chrono <- model_ds %>%
  filter(SEASON <= 2019)
test_chrono <- model_ds %>%
  filter(SEASON > 2019)

# Set a tidymodels data recipe
data_recipe <- recipe(WIN_home ~ ., data = model_ds) %>%
  update_role(GAME_DATE_EST, SEASON,
              TEAM_ID_home, TEAM_ID_away,
              new_role = "ID")

# Fit a logistic regression
fit_lr <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(logistic_reg()) %>%
  fit(train_chrono)

tidy(fit_lr) # output model coefficients
# glance(fit_lr) # output model metrics

set.seed(123) # set seed for tree models

# Fit a random forest model
# fit_rf <- workflow() %>%
#   add_recipe(data_recipe) %>%
#   add_model(rand_forest(mode = "classification") %>%
#             set_engine(engine = "ranger",
#                        num.threads = 6,
#                        importance = "impurity")) %>%
#   fit(train_chrono)

# Fit a decision tree model
fit_dt <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(decision_tree(mode = "classification")) %>%
  fit(train_chrono)

fit_dt_nofg <- workflow() %>%
  add_recipe(data_recipe %>%
             step_rm(all_of(starts_with("FG")))) %>%
  add_model(decision_tree(mode = "classification")) %>%
  fit(train_chrono)

# Show a confusion matrix of performance of the logistic regression model
test_chrono %>%
  select(WIN_home) %>%
  mutate(predictions = predict(fit_lr, test_chrono, type = "class")$.pred_class) %>%
  conf_mat(WIN_home, predictions)

# Show a confusion matrix of performance of the random forest model
# test_chrono %>%
#   select(WIN_home) %>%
#   mutate(predictions = predict(fit_rf, test_chrono, type = "class")$.pred_class) %>%
#   conf_mat(WIN_home, predictions)

# Show a confusion matrix of performance of the decision tree model
test_chrono %>%
  select(WIN_home) %>%
  mutate(predictions = predict(fit_dt, test_chrono, type = "class")$.pred_class) %>%
  conf_mat(WIN_home, predictions)

# Function that performs v-fold cross-validation on pre-specified model fits
# and returns an array of model performance metrics
vfoldcv_fit <- function(vars_to_remove=NULL, recipe=data_recipe,
                        train=train_chrono, folds=10, seed=2023) {
  # Update recipe to remove specified variables
  updated_recipe <- recipe %>%
    step_rm(all_of(vars_to_remove)) %>%
    step_normalize(all_predictors()) # scale and center all predictors
  
  # Create a list of models
  models <- list(logistic_reg(),
                 decision_tree(mode="classification"))

  # Define the workflows for logistic regression and decision tree
  workflows <- lapply(models, function(x) {
    add_recipe(workflow(), updated_recipe) %>%
    add_model(x)
  })

  # Set seed for reproducibility
  set.seed(seed)
  train_folds <- vfold_cv(train, v = folds, strata = WIN_home)

  # Fit the models
  fits <- lapply(workflows, function(x) {
    fit_resamples(x, train_folds,
                  metrics = metric_set(roc_auc, f_meas, kap),
                  control = control_resamples(
                    extract = extract_fit_parsnip, save_pred = TRUE))
  })
  names(fits) <- list("Logistic_Regression","Decision_Tree")

  # Return model metrics
  fits_metrics <- lapply(fits, collect_metrics)
  names(fits_metrics) <- names(fits)

  # Return model predictions
  fits_pred <- lapply(fits, collect_predictions)
  names(fits_pred) <- names(fits)

  # Return logistic model coefficients across all folds
  model_coefs <- fits$Logistic_Regression %>% 
    select(id, .extracts) %>% # get the id and .extracts columns
    unnest(cols = .extracts) %>% # unnest .extracts, which produces the model in a list
    mutate(coefs = map(.extracts, tidy)) %>% # get the coefficients in their own column
    unnest(coefs) # get the coefficients for each fold
  
  object <- list(fits_metrics,fits_pred,model_coefs)
  names(object) <- list("Fits_Metrics","Fits_Predictions","Logistic_Model_Coefficients")
  return(object)
}

a <- vfoldcv_fit()
b <- vfoldcv_fit(starts_with("FG"))
# c <- vfoldcv_fit(starts_with("EV"))
# d <- vfoldcv_fit(starts_with(c("FG","EV")))