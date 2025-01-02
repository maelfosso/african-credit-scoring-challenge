if(!require(discrim)){install.packages("discrim")}
if(!require(MASS)){install.packages("MASS")}
if(!require(ranger)){install.packages("ranger")}
if(!require(xgboost)) install.packages("xgboost")
if(!require(lubridate)) install.packages("lubridate")
if(!require(themis)) install.packages("themis")

library(parsnip) # Load parsnip first
library(discrim)

library(tidymodels)
tidymodels_prefer()

train <- read.csv('data/train_no_duplicates.csv')
head(train)
str(train)

test <- read.csv('data/Test.csv')
head(test)

# Before all `target` must be a factor
# train$target <- as.factor(train$target)
train <-
  train |>
  mutate(
    target = case_when(
      target == 0 ~ 'No',
      target == 1 ~ 'Yes'
    )
  ) |>
  mutate(target = factor(target))

# folds <- group_vfold_cv(train, v = 10, group = customer_id)
folds <- vfold_cv(train, v = 10, strata = target)

#####
# lender_id
#####
train |> 
  pull(lender_id) |>
  unique()
train |>
  group_by(lender_id) |>
  arrange(lender_id) |>
  count()

test |>
  pull(lender_id) |>
  unique()
test |> 
  group_by(lender_id) |>
  arrange(lender_id) |>
  count()


# there are 8 lender_id in test train set and only 4 on train trainset

all_lender_id <- as.factor(test |> pull(lender_id) |> unique())
all_lender_id

########
# country_id
########
train |> 
  pull(country_id) |>
  unique()

test |>
  pull(country_id) |>
  unique()
# there are 2 country_id in test train set and only 1 on train trainset

train |>
  group_by(country_id) |>
  summarise(count = n()/nrow(train)) |>
  ungroup()

table(train$country_id)
dim(train)

test |>
  group_by(country_id) |>
  summarise(count = n()/nrow(test))

test |>
  group_by(country_id) |>
  summarise(count = n()/nrow(test)) |>
  ggplot(aes(x = reorder(country_id, -count), y = count)) + 
    geom_bar(stat = "identity") + coord_flip()

all_country_id <- as.factor(test |> pull(country_id) |> unique())
all_country_id

######
# country_id and lender_id
###
test |>
  group_by(country_id, lender_id) |>
  summarize(count = n()/nrow(test)) |>
  arrange(-count) |> ungroup()

train |>
  group_by(country_id, lender_id) |>
  summarize(count = n()/nrow(train)) |>
  arrange(-count) |> ungroup()

################train
# loan_type
########
train_loan_type <- train |>
  pull(loan_type) |>
  unique() 

test_loan_type <- test |>
  pull(loan_type) |>
  unique()

# check the percentage of each level into the test dataset
test |> 
  # pull(loan_type) |>
  group_by(loan_type) |>
  summarize(count = 100 * n()/nrow(test)) |>
  arrange(-count)
  
test |> 
  # pull(loan_type) |>
  group_by(loan_type) |>
  summarize(count = n()) |>
  arrange(-count) |>
  ggplot(aes(x = reorder(loan_type, -count), y = count)) + 
    geom_bar(stat = "identity") + coord_flip()

# check the percentage of each level into the train data set
train |>
  group_by(loan_type) |>
  summarize(count = 100 * n()/nrow(train)) |>
  arrange(-count)

train |> 
  # pull(loan_type) |>
  group_by(loan_type) |>
  summarize(count = n()) |>
  arrange(-count) |>
  ggplot(aes(x = reorder(loan_type, -count), y = count)) + 
  geom_bar(stat = "identity") + coord_flip()

# From Train
# Type_1 / Type_5 / Type_4 / Type_7 / Type_9 / Type_10 / Type_6 / (14, 2) / Others ( 0.2 or 0.1 / 100)
# From Test
# Type_1 / 

# check which levels are not present into the train data set
setdiff(test_loan_type, train_loan_type) # Type_3, Type_8

all_loan_type <- union(train_loan_type, test_loan_type)
all_loan_type

#######
# New_versus_Repeat
#######
train_nvr <- train |>
  pull(New_versus_Repeat) |>
  unique()

test_nvr <- test |>
  pull(New_versus_Repeat) |>
  unique()

all_new_versus_repeat <- union(train_nvr, test_nvr)
all_new_versus_repeat

######
# Total_Amount
######
train |> ggplot(aes(x = Total_Amount)) + geom_histogram(bins = 50) 
# => Really right skewed

# check the plot with transformed values
train |> 
  ggplot(aes(x = Total_Amount)) + geom_histogram(bins = 50) +
  scale_x_log10()
# => amazing; it's balanced

train |> 
  ggplot(aes(x = Total_Amount)) + geom_histogram(bins = 50) +
  scale_x_sqrt()
# => still right skewed

train |> 
  ggplot(aes(x = Total_Amount^(1/3))) + geom_histogram(bins = 50)
# => still right skewed but less than the previous


train |> 
  ggplot(aes(x = Total_Amount^(1/3))) + geom_histogram(bins = 50) + scale_x_log10()
# => skewed almost 0

train |> ggplot(aes(x = Total_Amount, fill = target)) + geom_histogram(bins = 50)
# => target = 0 represent most of the population

train |> 
  ggplot(aes(x = Total_Amount, fill = target)) + 
  geom_histogram(bins = 50) + scale_x_log10()
# => target = 0 is everywhere
# => target = 1  

#####
# Total_Amount_to_Repay
#####
train |> ggplot(aes(x = Total_Amount_to_Repay)) +
  geom_histogram(bins = 50)
# => strongly right skewed

train |> ggplot(aes(x = Total_Amount_to_Repay)) +
  geom_histogram(bins = 50) + scale_x_log10()
# => skewed fixed!

######
# duration
######
train |> ggplot(aes(x = duration)) +
  geom_histogram(bins = 50)
# => strongly right skewed

train |> ggplot(aes(x = duration)) +
  geom_histogram(bins = 50) + scale_x_log10()
# => still right skewed and the result clearly show well-defined and spaced bins 

# bar plot
train |> ggplot(aes(x = duration)) +
  geom_bar()

train |> distinct(duration) |> nrow()
# => 57 unique values

train |> distinct(duration) |> is.na() |> sum()
# => no NA values

p <- train |> ggplot(aes(x = duration)) +
  geom_histogram(bins = 10) # + scale_x_log10()
p
ggplot_build(p)$data[[1]] |> filter(count != 0) |> select(count, x, xmin, xmax) |>
  ggplot(aes(x = x, y = count)) + geom_bar(stat = "identity")

recipe(
    target ~ duration,
    data = train
  ) |>
  step_mutate(duration = log10(duration)) |>
  step_cut(duration, breaks = ggplot_build(p)$data[[1]]$x) |>
  prep() |> bake(train) |> ggplot(aes(x = duration)) + geom_bar(stat = "count")


######
# Amount_Funded_By_Lender
######
train |> ggplot(aes(x = Amount_Funded_By_Lender)) +
  geom_histogram(bins = 50)
# => right skewed

train |> ggplot(aes(x = Amount_Funded_By_Lender)) +
  geom_histogram(bins = 50) + scale_x_log10()
# => skweness fixed!

train |> ggplot(aes(x = scale(Amount_Funded_By_Lender))) +
  geom_histogram(bins = 50) + scale_x_log10()
# => skwness_fixed! It's better than the previous transformation


####
# Lender_portion_Funded
####
train |> ggplot(aes(x = Lender_portion_Funded)) +
  geom_histogram(bins = 50)
# => skewed at right with two modes
 
train |> ggplot(aes(x = Lender_portion_Funded)) +
  geom_histogram(bins = 50) + scale_x_log10()
# => skewed at left

train |> ggplot(aes(x = Lender_portion_Funded)) +
  geom_histogram(bins = 50) + scale_x_sqrt()
# => skewed with two modes

train |> filter(Lender_portion_Funded == 0) |> nrow()
# => 8518, that is around 13% of the training that
# Why 0? Why didn't the Lended fund a part of that lend?

##############
# Lender_portion_to_be_repaid
#########
train |> ggplot(aes(x = Lender_portion_to_be_repaid)) +
  geom_histogram(bins = 50)
# => right skewed

train |> ggplot(aes(x = Lender_portion_to_be_repaid)) +
  geom_histogram(bins = 50) + scale_x_log10()
# => skewness fixed! 

##############
# Preprocessing
########

basic_recipe <-
  recipe(target ~ 
           loan_type + New_versus_Repeat + 
           Total_Amount + Total_Amount_to_Repay +
           disbursement_date + due_date + duration +
           Amount_Funded_By_Lender + Lender_portion_Funded + Lender_portion_to_be_repaid, 
         data = train) |>
  step_string2factor(loan_type, levels = all_loan_type) |>
  step_string2factor(New_versus_Repeat, levels = all_new_versus_repeat) |>
  step_other(loan_type, threshold = 0.1 / 100) |>
  step_mutate(
    disbursement_date = ymd(disbursement_date),
    due_date = ymd(due_date)
  ) |>
  step_date(
    all_of(c("disbursement_date", "due_date")),
    features = c("dow", "doy", "month", "year"),
    keep_original_cols = FALSE
  ) |>
  step_log(duration, Total_Amount, Total_Amount_to_Repay, Amount_Funded_By_Lender, Lender_portion_to_be_repaid, base = 10, offset = 0.001) |>
  step_normalize(Total_Amount, Total_Amount_to_Repay, Amount_Funded_By_Lender, Lender_portion_to_be_repaid, Lender_portion_to_be_repaid, na_rm = TRUE) |>
  step_cut(duration, breaks = ggplot_build(p)$data[[1]]$x) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_smote(target, over_ratio = tune())

basic_recipe |>
  prep() |>
  bake(new_data = NULL) |> str()

tidy(basic_recipe, number = 2)
#####
# Let's go with multiple recipe
#####

pca_recipe <-
  basic_recipe |>
  step_pca(all_numeric_predictors(), num_comp = tune()) |>
  step_normalize(all_numeric_predictors())

zv_recipe <-
  basic_recipe |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.9) |>
  step_normalize(all_numeric_predictors())

pca_zv_recipe <-
  basic_recipe |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.9) |>
  step_pca(all_numeric_predictors(), num_comp = tune()) |>
  step_normalize(all_numeric_predictors())

############
# MODELS
#########
dt_model <-
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) |>
  set_engine('rpart') |>
  set_mode('classification')

lr_model <-
  logistic_reg(penalty = tune(), mixture = tune()) |>
  set_engine('glmnet') |>
  set_mode("classification")

nb_model <-
  naive_Bayes(smoothness = tune(), Laplace = tune()) |>
  set_engine('klaR') |>
  set_mode("classification")

rf_model <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) |>
  set_engine('ranger') |>
  set_mode('classification')

svm_model <-
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) |>
  set_engine("kernlab")

#########
# WORKFLOW
#########

metrics <- metric_set(roc_auc, f_meas)
control_rs <- control_resamples(
  verbose = TRUE,
  save_pred = FALSE,
  parallel_over = "everything"
)
control_gd <- control_grid(
  verbose = TRUE,
  save_pred = FALSE,
  parallel_over = "everything"
)
control_wf <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = FALSE
)

set.seed(1)

########## Decision Tree

### PCA Recipe
dt_pca_wf <-
  workflow() |>
  add_model(dt_model) |>
  add_recipe(pca_recipe)

dt_pca_grid <-
  grid_regular(
    over_ratio(c(0.5, 0.8)),
    num_comp(c(3, 20)),
    cost_complexity(),
    tree_depth(),
    min_n()
  )

dt_pca_tune <-
  dt_pca_wf |>
  tune_grid(
    resamples = folds,
    grid = dt_pca_grid,
    metrics = metrics,
    control = control_gd
  )
dt_pca_tune |> collect_metrics()

dt_pca_tune |>
  show_best(metric = "f_meas")
# A tibble: 5 × 11
# cost_complexity tree_depth min_n over_ratio num_comp .metric .estimator  mean     n  std_err .config              
# <dbl>      <int> <int>      <dbl>    <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>                
#   1    0.0000000001          1     2        0.8        3 f_meas  binary     0.984    10 0.000451 Preprocessor3_Model01
# 2    0.00000316            1     2        0.8        3 f_meas  binary     0.984    10 0.000451 Preprocessor3_Model02
# 3    0.1                   1     2        0.8        3 f_meas  binary     0.984    10 0.000451 Preprocessor3_Model03
# 4    0.1                   8     2        0.8        3 f_meas  binary     0.984    10 0.000451 Preprocessor3_Model06
# 5    0.1                  15     2        0.8        3 f_meas  binary     0.984    10 0.000451 Preprocessor3_Model09

dt_pca_tune |>
  show_best(metric = "roc_auc")
# A tibble: 5 × 11
# cost_complexity tree_depth min_n over_ratio num_comp .metric .estimator  mean     n std_err .config              
# <dbl>      <int> <int>      <dbl>    <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    0.0000000001         15    40       0.8        20 roc_auc binary     0.762    10 0.00771 Preprocessor9_Model25
# 2    0.00000316           15    40       0.8        20 roc_auc binary     0.762    10 0.00771 Preprocessor9_Model26
# 3    0.0000000001         15    40       0.65       20 roc_auc binary     0.750    10 0.00598 Preprocessor8_Model25
# 4    0.00000316           15    40       0.65       20 roc_auc binary     0.750    10 0.00598 Preprocessor8_Model26
# 5    0.0000000001         15    40       0.5        20 roc_auc binary     0.750    10 0.0103  Preprocessor7_Model25

dt_best_pca_f1 <-
  dt_pca_tune |>
  select_best(metric = "f_meas")
dt_best_pca_f1
# A tibble: 1 × 6
# cost_complexity tree_depth min_n over_ratio num_comp .config              
# <dbl>      <int> <int>      <dbl>    <int> <chr>                
#   1    0.0000000001          1     2        0.8        3 Preprocessor3_Model01

dt_best_pca_roc.auc <-
  dt_pca_tune |>
  select_best(metric = "roc_auc")
dt_best_pca_roc.auc
# A tibble: 1 × 6
# cost_complexity tree_depth min_n over_ratio num_comp .config              
# <dbl>      <int> <int>      <dbl>    <int> <chr>                
#   1    0.0000000001         15    40        0.8       20 Preprocessor9_Model25


dt_pca_final_wf.f1 <-
  dt_pca_wf |>
  finalize_workflow(dt_best_pca_f1)

dt_pca_final_fit.f1 <-
  dt_pca_final_wf.f1 |>
  fit(data = train)

dt_pca_final_fit.f1 |>
  predict(test) |>
  table()

dt_pca_final_fit.f1 |>
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
  write.csv("submission_dt_pca_f1.csv", row.names = FALSE)
# => Submission : 0.202247191 WORST EVER

dt_pca_final_wf.roc_auc <-
  dt_pca_wf |>
  finalize_workflow(dt_best_pca_roc.auc)

dt_pca_final_fit.roc_auc <-
  dt_pca_final_wf.roc_auc |>
  fit(data = train)

dt_pca_final_fit.roc_auc |>
  predict(test) |>
  table()

dt_pca_final_fit.roc_auc |>
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
  write.csv("submission_dt_pca_roc_auc.csv", row.names = FALSE)
# => Submission: Worst ever 0.182987848

####
# As Decision Tree is really worst, let's try Random Forest


##### BASIC
rf_wf.basic <-
  workflow() |>
  add_model(rf_model) |>
  add_recipe(basic_recipe)

rf_grid.basic <-
  grid_regular(
    over_ratio(c(0.5, 0.8)),
    mtry(c(2, 29)),
    trees(c(500, 1000)),
    min_n(c(2, 40)),
    levels = c(3, 10, 3, 5)
  )

rf_tune.basic <-
  rf_wf.basic |>
  tune_grid(
    folds,
    grid = rf_grid.basic,
    metrics = metrics,
    control = control_gd
  )

rf_tune.basic |> collect_metrics()

autoplot(rf_tune)

rf_best <-
  rf_tune |>
  select_best(metric = "f_meas")

rf_best

######### ZV PCA


rf_wf.pca_zv <-
  workflow() |>
  add_model(rf_model) |>
  add_recipe(pca_zv_recipe)

rf_grid.pca_zv <-
  grid_regular(
    over_ratio(c(0.5, 0.8)),
    num_comp(3, 21),
    mtry(c(2, 29)),
    trees(c(500, 1000)),
    min_n(c(2, 40)),
    levels = c(3, 10, 3, 5)
  )

rf_tune.pca_zv <-
  rf_wf.pca_zv |>
  tune_grid(
    folds,
    grid = rf_grid.pca_zv,
    metrics = metrics,
    control = control_gd
  )


####### Logistic Regression
lr_model |> translate()

lr_wf <-
  workflow() |>
  add_model(lr_model) |>
  add_recipe(pca_zv_recipe)

lr_wf |> extract_parameter_set_dials()

lr_frs <-
  lr_wf |>
  tune_grid(
    folds,
    grid = grid_regular(
      over_ratio(c(0.5, 1)),
      num_comp(c(3, 21)),
      penalty(),
      mixture()
    ),
    metrics = metrics,
    control = control_gd
  )

lr_frs |> collect_metrics()
# A tibble: 162 × 10
# penalty mixture over_ratio num_comp .metric .estimator  mean     n  std_err .config             
# <dbl>   <dbl>      <dbl>    <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
#   1 0.0000000001     0          0.5        3 f_meas  binary     0.970    10 0.000637 Preprocessor1_Model1
# 2 0.0000000001     0          0.5        3 roc_auc binary     0.662    10 0.00850  Preprocessor1_Model1
# 3 0.00001          0          0.5        3 f_meas  binary     0.970    10 0.000637 Preprocessor1_Model2
# 4 0.00001          0          0.5        3 roc_auc binary     0.662    10 0.00850  Preprocessor1_Model2
# 5 1                0          0.5        3 f_meas  binary     0.993    10 0.000234 Preprocessor1_Model3
# 6 1                0          0.5        3 roc_auc binary     0.661    10 0.00840  Preprocessor1_Model3
# 7 0.0000000001     0.5        0.5        3 f_meas  binary     0.968    10 0.000735 Preprocessor1_Model4
# 8 0.0000000001     0.5        0.5        3 roc_auc binary     0.662    10 0.00849  Preprocessor1_Model4
# 9 0.00001          0.5        0.5        3 f_meas  binary     0.968    10 0.000735 Preprocessor1_Model5
# 10 0.00001          0.5        0.5        3 roc_auc binary     0.662    10 0.00849  Preprocessor1_Model5

lr_frs |> show_best(metric = "f_meas")

lr_best <-
  lr_frs |>
  select_best(metric = "f_meas")

lr_best

# lr_fit <- 1
#   lr_wf |>
#   fit(data = train)

lr_final_wf <-
  lr_wf |>
  finalize_workflow(lr_best)

lr_fit <-
  lr_final_wf |>
  fit(data = train)

lr_fit |>
  predict(test) |>
  table()

lr_fit |>
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
  write.csv("submission-lr-pca-zv.csv", row.names = FALSE)
# => Submission lr-pca-zv 0.07494824

lr_frs |> 
  collect_metrics() |> 
  filter(0.7 <= mean & mean <= 0.8) |>
  arrange(mean)

lr_worst_param <-
  tibble(
    penalty = 1,
    mixture = 0.5,
    over_ratio = 0.5,
    num_comp = 3
  )

lr_worst_final_wf <-
  lr_wf |>
  finalize_workflow(lr_worst_param)

lr_worst_fit <-
  lr_worst_final_wf |>
  fit(data = train)

lr_worst_fit |>
  predict(test) |>
  table()

# .pred_class
# No   Yes 
# 18594     0 

lr_frs |> 
  collect_metrics() |> 
  filter(0.7 <= mean & mean <= 0.8) |>
  arrange(mean)

lr_middle_param <-
  tibble(
    penalty = 1,
    mixture = 0,
    over_ratio = 0.5,
    num_comp = 12
  )

lr_middle_final_wf <-
  lr_wf |>
  finalize_workflow(lr_middle_param)

lr_middle_fit <-
  lr_middle_final_wf |>
  fit(data = train)

lr_middle_fit |>
  predict(test) |>
  table()

############## Naive Bayes

nb_wf <-
  workflow() |>
  add_model(nb_model) |>
  add_recipe(pca_zv_recipe)

nb_wf |> extract_parameter_set_dials()

nb_tune <-
  nb_wf |>
  tune_grid(
    folds,
    grid = grid_regular(
      smoothness(),
      Laplace(),
      over_ratio(),
      num_comp(c(3, 29))
    ),
    metrics = metrics,
    control = control_gd
  )

lr_frs |> collect_metrics()