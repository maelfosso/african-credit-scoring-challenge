
set.seed(1)

########## Decision Tree

### PCA Recipe
dt_wf <-
  workflow() |>
  add_model(dt_model) |>
  add_recipe(pca_recipe)

dt_grid <-
  grid_regular(
    num_comp(c(3, 20)),
    cost_complexity(),
    tree_depth(),
    min_n()
    # levels = c(4, 5, 4, 4)
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
# A tibble: 5 × 10
# cost_complexity tree_depth min_n num_comp .metric .estimator  mean     n  std_err .config              
# <dbl>      <int> <int>    <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>                
#   1    0.0000000001          1     2       20 f_meas  binary     0.993    10 0.000278 Preprocessor3_Model01
# 2    0.00000316            1     2       20 f_meas  binary     0.993    10 0.000278 Preprocessor3_Model02
# 3    0.0000000001          1    21       20 f_meas  binary     0.993    10 0.000278 Preprocessor3_Model10
# 4    0.00000316            1    21       20 f_meas  binary     0.993    10 0.000278 Preprocessor3_Model11
# 5    0.0000000001          1    40       20 f_meas  binary     0.993    10 0.000278 Preprocessor3_Model19

dt_best_pca <-
  dt_tune |>
  select_best(metric = "f_meas")
dt_best_pca
# A tibble: 1 × 5
# cost_complexity tree_depth min_n num_comp .config              
# <dbl>      <int> <int>    <int> <chr>                
#   1    0.0000000001          1     2       20 Preprocessor3_Model01


### ZV
dt_wf_zv <-
  workflow() |>
  add_model(dt_model) |>
  add_recipe(zv_recipe)

dt_zv_grid <-
  grid_regular(
    cost_complexity(),
    tree_depth(),
    min_n()
    # levels = c(4, 5, 4, 4)
  )

dt_zv_tune <-
  dt_wf_zv |>
  tune_grid(
    resamples = folds,
    grid = dt_zv_grid,
    metrics = metrics,
    control = control_gd
  )

dt_zv_tune |> collect_metrics()

dt_zv_tune |>
  show_best(metric = "f_meas")
# A tibble: 5 × 9
# cost_complexity tree_depth min_n .metric .estimator  mean     n  std_err .config              
# <dbl>      <int> <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>                
#   1    0.0000000001         15    21 f_meas  binary     0.994    10 0.000275 Preprocessor1_Model16
# 2    0.00000316           15    21 f_meas  binary     0.994    10 0.000275 Preprocessor1_Model17
# 3    0.0000000001         15    40 f_meas  binary     0.994    10 0.000256 Preprocessor1_Model25
# 4    0.00000316           15    40 f_meas  binary     0.994    10 0.000256 Preprocessor1_Model26
# 5    0.0000000001          8    21 f_meas  binary     0.993    10 0.000240 Preprocessor1_Model13



### ZV
dt_wf_zv_pca <-
  workflow() |>
  add_model(dt_model) |>
  add_recipe(pca_zv_recipe)

dt_zv_pca_grid <-
  grid_regular(
    num_comp(c(3, 20)),
    cost_complexity(),
    tree_depth(),
    min_n()
    # levels = c(4, 5, 4, 4)
  )

dt_zv_pca_tune <-
  dt_wf_zv_pca |>
  tune_grid(
    resamples = folds,
    grid = dt_zv_pca_grid,
    metrics = metrics,
    control = control_gd
  )

dt_zv_pca_tune |> collect_metrics()

dt_zv_pca_tune |>
  show_best(metric = "f_meas")
# A tibble: 5 × 10
# cost_complexity tree_depth min_n num_comp .metric .estimator  mean     n  std_err .config              
# <dbl>      <int> <int>    <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>                
#   1    0.0000000001          8    21        3 f_meas  binary     0.993    10 0.000245 Preprocessor1_Model13
# 2    0.00000316            8    21        3 f_meas  binary     0.993    10 0.000245 Preprocessor1_Model14
# 3    0.0000000001         15    40       20 f_meas  binary     0.993    10 0.000274 Preprocessor3_Model25
# 4    0.00000316           15    40       20 f_meas  binary     0.993    10 0.000274 Preprocessor3_Model26
# 5    0.0000000001          8    40        3 f_meas  binary     0.993    10 0.000242 Preprocessor1_Model22



folds
folds$splits |>
  map_df(~ {
    training_data <- analysis(.x)
    training_data |> head()
    # count(target) |>
    # mutate(fold = .y)
  }, .id = "fold")

# Function to calculate class proportions within a split
calculate_proportions <- function(split) {
  # Extract the analysis data (training set for the fold)
  data <- analysis(split)
  
  # Calculate proportions
  data %>%
    count(target) %>%
    mutate(proportion = n / sum(n))
}

# Apply the function to each split and store the results
fold_proportions <- folds %>%
  mutate(proportions = map(splits, calculate_proportions))

# Unnest the results for easier viewing
fold_proportions %>%
  unnest(proportions)
