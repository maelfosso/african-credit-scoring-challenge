library(tidymodels)
tidymodels_prefer()

train <- read.csv('data/train_no_duplicates.csv')
head(train)

test <- read.csv('data/Test.csv')
head(test)

folds <- group_vfold_cv(train, v = 10, group = customer_id)

# folds <- vfold_cv(train, v = 10, strata = target)
# train <-
#   train |> 
#   select(-target) |>
#   bind_rows(test) |>
#   summary()


names(train)
features <- c(
  "country_id", "lender_id",
  "loan_type", "New_versus_Repeat",
  "disturbement_date", "due_date",
  "Total_Amount", "Total_Amount_to_Repay", "duration", 
  "Amount_Funded_By_Lender", "Lender_portion_Funded", "Lender_portion_to_be_repaid"
)

# Before all `target` must be a factor
train$target <- as.factor(train$target)

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

recipe(target ~ Lender_portion_Funded, data = train) |>
  step_mutate(Lender_portion_Funded = log10_trans(Lender_portion_Funded)) |>
  step_log(Lender_portion_Funded, base = 10) |>
  step_YeoJohnson(Lender_portion_Funded) |>
  prep() |> bake(new_data = NULL) |>
  ggplot(aes(x = Lender_portion_Funded)) + geom_histogram(bins = 50)
box# => Nothing as changed

recipe(target ~ Lender_portion_Funded, data = train) |>
  step_log(Lender_portion_Funded, base = 10) |>
  step_BoxCox(Lender_portion_Funded) |>
  prep() |> bake(new_data = NULL) |>
  ggplot(aes(x = Lender_portion_Funded)) + geom_histogram(bins = 50)


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
  step_date(all_date()) |>
  step_normalize(Total_Amount, Total_Amount_to_Repay, Amount_Funded_By_Lender, Lender_portion_to_be_repaid, Lender_portion_to_be_repaid) |>
  step_log(duration, Total_Amount, Total_Amount_to_Repay, Amount_Funded_By_Lender, Lender_portion_to_be_repaid, base = 10) |>
  step_cut(duration, breaks = ggplot_build(p)$data[[1]]$x) |>
  step_dummy(loan_type, New_versus_Repeat, duration, one_hot = TRUE)


############
# MODELS
#########
dt_model <-
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) |>
  set_engine('rpart') |>
  set_mode('classification')

lda_model <-
  discrim_linear() |>
  set_engine('MASS')

qda_model <-
  discrim_quad() |>
  set_engine('MASS')

lr_model <-
  logistic_reg() |>
  set_engine('glm')

nb_model <-
  naive_Bayes(smoothness = tune(), Laplace = tune()) |>
  set_engine('klaR') |>
  set_mode("classification")

knn_model <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) |>
  set_engine('kknn') |>
  set_mode('classification')

rf_model <-
  rand_forest(mtry = tune(), min_n = tune()) |>
  set_engine('ranger') |>
  set_mode('classification')

svm_model <-
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) |>
  set_engine("kernlab")

#########
# WORKFLOW
#########

set.seed(1)

preproc = list(
  basic = basic_recipe
)

models <- list(
  knn = knn_model,
  lr = lr_model,
  # svm = svm_model,
  # tree = dt_model,
  # lda = lda_model,
  # qda = qda_model,
  # rf = rf_model,
  nb = nb_model,
  svm = svm_model
)

wf_set <- workflow_set(preproc, models, cross = TRUE)
wf_set

tuning <- 
  wf_set |>
  workflow_map(
    "tune_grid",
    resamples = folds,
    grid = 20,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tuning

workflow() |>
  add_model(nb_model)

logistic_reg_glm_wf <-
  workflow() |>
  add_model(logistic_reg_glm_model) |>
  add_variables(outcomes = target, predictors = c(country_id, loan_type, new_vs_repeat))

logistic_reg_glm_fit <-
  logistic_reg_glm_wf |>
  remove_variables() |>
  add_recipe(cat_features_recipe) |> fit(train)

extract_fit_parsnip(logistic_reg_glm_fit) |>
  tidy()

logistic_reg_glm_pred <-
  predict(logistic_reg_glm_fit, test |> mutate(new_vs_repeat = New_versus_Repeat))
table(logistic_reg_glm_pred)
head(test)

test |>
  select(ID) |>
  bind_cols(logistic_reg_glm_pred) |>
  mutate(target = .pred_class) |>
  select(ID, target) |>
  write.csv("submission.csv", row.names = FALSE)
