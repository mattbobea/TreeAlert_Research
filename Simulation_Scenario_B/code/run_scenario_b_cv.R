# Scenario B: fixed simulation and reproducible error-series cross-validation.
# The data and one auto.arima fit are created once. Trees are then evaluated
# using a reproducible randomized 20-fold partition of the training errors.

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

root_dir <- if (dir.exists("TA_functions")) "." else "../.."
simulation_dir <- if (dir.exists("Simulation_Scenario_B")) "Simulation_Scenario_B" else ".."
source(file.path(root_dir, "TA_functions", "TA_functions.R"))
source(file.path(root_dir, "TA_functions", "Rephrase_Rule_Func.R"))
source(file.path(simulation_dir, "code", "generate_actual_and_forecasts_data.R"))

results_dir <- file.path(simulation_dir, "results")
image_dir <- file.path(simulation_dir, "images")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)

data_seed <- 1001L
fold_seed <- 3001L
n_folds <- 20L
n_groups <- 20L
n_rules <- 3L
lift_metric <- "rmse"
tree_formula <- .resid ~ WeekofYear + Month
param_grid <- expand.grid(cp = c(0, 0.00001, 0.0001, 0.0005),
                          minbucket = 4:8, maxdepth = 1:8)

# Generate and persist the series once; subsequent reruns reproduce it.
analysis_data <- generate_actual_and_forecasts_data(data_seed, write_output = TRUE) %>%
  mutate(WeekofYear = factor(as.character(WeekofYear), levels = as.character(1:53)))
train_all <- filter(analysis_data, Time <= as.Date("2023-12-31"), is.finite(.resid))
test_set <- filter(analysis_data, Time >= as.Date("2024-01-01"), is.finite(.resid))

set.seed(fold_seed)
fold_id <- sample(rep(seq_len(n_folds), length.out = nrow(train_all)))

run_fold <- function(fold) {
  fit_set <- train_all[fold_id != fold, , drop = FALSE]
  held_out <- train_all[fold_id == fold, , drop = FALSE]
  search <- tree_alert_grid_search(train_set = fit_set, formula = tree_formula,
                                   param_grid = param_grid, n_groups = n_groups,
                                   pred_col_name = "Tree_Prediction",
                                   lift_metric = lift_metric)
  model <- tree_alert_train_best_model(fit_set, tree_formula, search$best_params)
  fit_set$Tree_Prediction <- predict(model, fit_set, type = "vector")
  test_eval <- test_set
  test_eval$Tree_Prediction <- predict(model, test_eval, type = "vector")
  tibble(fold = fold, fold_seed = fold_seed, data_seed = data_seed,
         fit_periods = nrow(fit_set), held_out_periods = nrow(held_out),
         test_periods = nrow(test_eval),
         cp = search$best_params$cp, minbucket = search$best_params$minbucket,
         maxdepth = search$best_params$maxdepth,
         train_lift = tree_alert_compute_lift_numeric(fit_set, ".resid", "Tree_Prediction", n_groups, lift_metric),
         test_lift = tree_alert_compute_lift_numeric(test_eval, ".resid", "Tree_Prediction", n_groups, lift_metric)) %>%
    list(summary = ., grid = mutate(search$results, fold = fold, fold_seed = fold_seed))
}

folds <- lapply(seq_len(n_folds), run_fold)
cv_results <- bind_rows(lapply(folds, `[[`, "summary"))
cv_grid <- bind_rows(lapply(folds, `[[`, "grid"))
write_csv(cv_results, file.path(results_dir, "scenario_b_cv_results.csv"))
write_csv(cv_grid, file.path(results_dir, "scenario_b_cv_grid_search.csv"))
write_csv(tibble(data_seed = data_seed, fold_seed = fold_seed, n_folds = n_folds),
          file.path(results_dir, "scenario_b_cv_seeds.csv"))

cv_long <- pivot_longer(cv_results, c(train_lift, test_lift),
                        names_to = "evaluation", values_to = "lift")
lower_whiskers <- cv_long %>%
  group_by(evaluation) %>%
  summarise(
    lower = boxplot.stats(lift)$stats[1],
    first_quartile = quantile(lift, 0.25),
    .groups = "drop"
  )
p1 <- ggplot(cv_long, aes(lift, evaluation, fill = evaluation)) +
  geom_boxplot(width = .55, coef = 0, outlier.shape = NA) +
  geom_errorbar(data = lower_whiskers,
                aes(xmin = lower, xmax = first_quartile, y = evaluation),
                orientation = "y", width = .15, inherit.aes = FALSE) +
  geom_jitter(width = .08, height = .08, alpha = .65) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  scale_x_continuous(breaks = seq(1, 3, by = 0.2),
                     minor_breaks = seq(1, 3, by = 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  scale_y_discrete(labels = c(train_lift = "Training",
                              test_lift = "Fixed test")) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 11, face = "plain"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.grid.minor.x = element_line(colour = "grey90")) +
  labs(x = "First-Ventile Lift", y = NULL)
ggsave(file.path(image_dir, "scenario_b_cv_lift_distribution.png"), p1, width = 10, height = 3.2, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_replication_lift_sensitivity.png"), p1, width = 10, height = 3.2, dpi = 1000)

param_long <- pivot_longer(cv_results, c(cp, minbucket, maxdepth), names_to = "parameter", values_to = "value")
p2 <- ggplot(param_long, aes(factor(value), fill = parameter)) +
  geom_bar() + facet_wrap(~parameter, scales = "free", nrow = 1) +
  theme_bw(base_size = 13) + theme(legend.position = "none") +
  labs(x = "Selected hyperparameter value", y = "Number of folds")
ggsave(file.path(image_dir, "scenario_b_cv_selected_parameters.png"), p2, width = 8, height = 4.5, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_selected_parameter_lift.png"), p2, width = 8, height = 4.5, dpi = 1000)

cat("Scenario B CV complete. Data seed:", data_seed, "\n")
print(read_csv(file.path(simulation_dir, "data", "scenario_b_arima_metadata.csv"), show_col_types = FALSE))
print(cv_results)
