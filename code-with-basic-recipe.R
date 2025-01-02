

set.seed(1)

############ Decision Tree
dt_model |> translate()

dt_wf <-
  workflow() |>
  add_model(dt_model) |>
  add_recipe(basic_recipe)

dt_grid <-
  grid_regular(
    cost_complexity(),
    tree_depth(),
    min_n(),
    levels = c(5, 10, 4)
  )

dt_tune <-
  dt_wf |>
  tune_grid(
    resamples = folds,
    grid = dt_grid,
    metrics = metrics,
    control = control_gd
  )
dt_tune |> collect_metrics()

dt_tune |>
  show_best(metric = "f_meas")
# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n  std_err .config               
# <dbl>      <int> <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>                 
# 1             0.1          1     2 f_meas  binary     0.993    10 0.000196 Preprocessor1_Model005
# 2             0.1          2     2 f_meas  binary     0.993    10 0.000196 Preprocessor1_Model010
# 3             0.1          4     2 f_meas  binary     0.993    10 0.000196 Preprocessor1_Model015
# 4             0.1          5     2 f_meas  binary     0.993    10 0.000196 Preprocessor1_Model020
# 5             0.1          7     2 f_meas  binary     0.993    10 0.000196 Preprocessor1_Model025

autoplot(dt_tune)

dt_best <- 
  dt_tune |>
  select_best(metric = "f_meas")

dt_best
# A tibble: 1 × 4
# cost_complexity tree_depth min_n .config               
# <dbl>      <int> <int> <chr>                 
# 0.1          1     2 Preprocessor1_Model005

dt_final_wf <-
  dt_wf |>
  finalize_workflow(dt_best)

dt_final_frs <-
  dt_final_wf |>
  fit_resamples(
    folds,
    metrics = metrics,
    control = control_rs
  )

dt_final_frs |>
  collect_metrics()
# A tibble: 2 × 6
# .metric .estimator  mean     n  std_err .config             
# <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
# f_meas  binary     0.993    10 0.000196 Preprocessor1_Model1
# roc_auc binary     0.5      10 0        Preprocessor1_Model1

########### Random Forest

rf_wf <-
  workflow() |>
  add_model(rf_model) |>
  add_recipe(basic_recipe)

rf_grid <-
  grid_regular(
    mtry(c(2, 29)),
    trees(c(500, 1000)),
    min_n(c(2, 40)),
    levels = c(10, 3, 5)
  )

rf_tune <-
  rf_wf |>
  tune_grid(
    folds,
    metrics = metrics,
    control = control_gd
  )

rf_tune |> collect_metrics()

autoplot(rf_tune)

rf_best <-
  rf_tune |>
  select_best(metric = "f_meas")

rf_best
# A tibble: 1 × 3
# mtry min_n .config              
# <int> <int> <chr>                
# 24    10 Preprocessor1_Model10

# in 2nd test
# A tibble: 1 × 4
# mtry trees min_n .config              
# <int> <int> <int> <chr>                
#   1    64  1730     6 Preprocessor1_Model02

# in the 3rd test the best best
# A tibble: 1 × 4
# mtry trees min_n .config              
# <int> <int> <int> <chr>                
#   1    64  1730     6 Preprocessor1_Model02

# but I need the one with the worse f1-score
rf_tune |> collect_metrics() |> arrange(mean)
# select the parameters from the 1st line
rf_best <- 
  tibble(
    mtry = 6,
    trees = 528,
    min_n = 10
  )

rf_final_wf <-
  rf_wf |>
  finalize_workflow(rf_best)

# rf_final_frs <-
#   rf_final_wf |>
#   fit_resamples(
#     folds,
#     metrics = metrics,
#     control = control_rs
#   )
# 
# rf_final_frs |>
#   collect_metrics()
# A tibble: 2 × 6
# .metric .estimator  mean     n  std_err .config             
# <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
# f_meas  binary     0.996    10 0.000168 Preprocessor1_Model1
# roc_auc binary     0.987    10 0.00207  Preprocessor1_Model1


# So far, random forest it's best that decision tree
rf_final_fit <-
  rf_final_wf |>
  fit(data = train)

rf_predict <-
  rf_final_fit |>
  predict(test)

table(rf_predict)
# .pred_class
# No   Yes 
# 18049   545

# with the worst one
# .pred_class
# No   Yes 
# 18440   154

# with the best one
# .pred_class
# No   Yes 
# 18156   438 

test |>
  select(ID) |>
  bind_cols(rf_predict) |>
  mutate(target = .pred_class) |>
  select(ID, target) |>
  mutate(
    target = case_when(
      target == 'No' ~ 0,
      target == 'Yes' ~ 1
    )
  ) |>
  write.csv("submission-rf-basic.wthout.smote-with-best.csv", row.names = FALSE)
# => Submission score: 0.5875
# => Submission score with basic without smote: 0.145454545
# => Submission score with basic without smote with the best: 0.145454545

########## Logistic Regression

lr_model |> translate()

lr_wf <-
  workflow() |>
  add_model(lr_model) |>
  add_recipe(basic_recipe)

lr_wf |> extract_parameter_set_dials()

lr_frs <-
  lr_wf |>
  fit_resamples(
    folds,
    metrics = metrics,
    control = control_rs
  )

lr_frs |> collect_metrics()

lr_fit <- 
  lr_wf |>
  fit(data = train)

lr_fit |>
  predict(test) |>
  table()

#####
# Let's go with multiple recipe
#####

pca_recipe <-
  basic_recipe |>
  step_corr(all_predictors(), threshold = 0.9)

zv_recipe <-
  basic_recipe |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors())


########train
# FOLD BASED ON Target 
###
folds <- vfold_cv(train, v = 10, strata = target)

### Random Forest

rf_wf <-
  workflow() |>
  add_model(rf_model) |>
  add_recipe(basic_recipe)

rf_grid <-
  grid_regular(
    mtry(c(2, 29)),
    trees(c(500, 1000)),
    min_n(c(2, 40)),
    levels = c(10, 3, 5)
  )

rf_tune <-
  rf_wf |>
  tune_grid(
    folds,
    metrics = metrics,
    control = control_gd
  )

rf_tune |> collect_metrics()

autoplot(rf_tune)

rf_best <-
  rf_tune |>
  select_best(metric = "f_meas")

rf_best
# A tibble: 1 × 4
# mtry trees min_n .config              
# <int> <int> <int> <chr>                
#   1    28  1311    22 Preprocessor1_Model07

rf_final_wf <-
  rf_wf |>
  finalize_workflow(rf_best)

rf_final_frs <-
  rf_final_wf |>
  fit_resamples(
    folds,
    metrics = metrics,
    control = control_rs
  )

rf_final_frs |>
  collect_metrics()
# A tibble: 2 × 6
# .metric .estimator  mean     n  std_err .config             
# <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
# f_meas  binary     0.996    10 0.000181 Preprocessor1_Model1
# roc_auc binary     0.989    10 0.00291  Preprocessor1_Model1

rf_final_fit <-
  rf_final_wf |>
  fit(data = train)

rf_final_fit |>
  predict(test) |>
  table()

# .pred_class
# No   Yes 
# 18039   555 

rf_final_fit |>
  predict(test) |>
  bind_cols(test) |>
  mutate(target = .pred_class) |>
  select(ID, target) |>
  mutate(
    target = case_when(
      target == 'No' ~ 0,
      target == 'Yes' ~ 1
    )
  ) |>
  write.csv("submission.csv", row.names = FALSE)
# => Submission score: 0.579439252

#################
# Serious OVERFITTING
###################
