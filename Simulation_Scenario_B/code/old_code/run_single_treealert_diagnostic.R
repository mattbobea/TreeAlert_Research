library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(rpart.plot)
library(scales)

root_dir <- if (dir.exists("TA_functions")) "." else if (dir.exists("../TA_functions")) ".." else "../.."
source(file.path(root_dir, "TA_functions", "TA_functions.R"))

simulation_dir <- if (dir.exists("Simulation_Scenario_B")) "Simulation_Scenario_B" else ".."
data_dir <- file.path(simulation_dir, "data")
image_dir <- file.path(simulation_dir, "images")
results_dir <- file.path(simulation_dir, "results")
dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

training_end <- as.Date("2021-12-31")
test_start <- as.Date("2022-01-01")
n_groups <- 20
n_rules <- 3
lift_metric <- "rmse"
predictor_names <- c("Day", "Week", "Month")

param_grid <- expand.grid(
  cp = c(0, 0.0001, 0.001, 0.005, 0.01),
  minbucket = seq(4, 8, by = 1),
  maxdepth = seq(4, 8, by = 1),
  stringsAsFactors = FALSE
)

analysis_data <- read_csv(
  file.path(data_dir, "scenario_b_actual_forecast_series.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Time = as.Date(Time),
    Day = day(Time),
    Week = ceiling(day(Time) / 7),
    Month = month(Time)
  ) %>%
  select(Time, Value, .fitted, .resid, all_of(predictor_names))

train_set <- filter(analysis_data, Time <= training_end, is.finite(.resid))
test_set <- filter(analysis_data, Time >= test_start, is.finite(.resid))
tree_formula <- .resid ~ Day + Week + Month

grid_search <- tree_alert_grid_search(
  train_set = train_set,
  formula = tree_formula,
  param_grid = param_grid,
  n_groups = n_groups,
  pred_col_name = "Tree_Prediction",
  lift_metric = lift_metric
)
best_params <- grid_search$best_params
tree_model <- tree_alert_train_best_model(train_set, tree_formula, best_params)

train_set$Tree_Prediction <- as.numeric(predict(tree_model, newdata = train_set, type = "vector"))
test_set$Tree_Prediction <- as.numeric(predict(tree_model, newdata = test_set, type = "vector"))
full_data <- bind_rows(train_set, test_set) %>% arrange(Time)

train_lift <- tree_alert_compute_lift_numeric(
  train_set, ".resid", "Tree_Prediction", n_groups, lift_metric
)
test_lift <- tree_alert_compute_lift_numeric(
  test_set, ".resid", "Tree_Prediction", n_groups, lift_metric
)

tree_rules <- rpart.plot::rpart.rules(tree_model, cover = FALSE, digits = 4, nn = FALSE)
rules_to_report <- min(n_rules, nrow(tree_rules))
rule_selection <- tree_alert_pull_top_k_rules(tree_rules, n_rules = rules_to_report)
raw_rules <- vapply(
  seq_len(nrow(rule_selection$selected_rules)),
  function(i) tree_alert_format_rule_raw(rule_selection$selected_rules[i, ]),
  character(1)
)
selected_rule_values <- unlist(rule_selection$selected_rule_values, use.names = FALSE)

train_cutoff <- quantile(train_set$Tree_Prediction, probs = 0.95, na.rm = TRUE)
full_data <- full_data %>% mutate(Flagged = Tree_Prediction >= train_cutoff)
rule_points <- bind_rows(lapply(seq_along(selected_rule_values), function(i) {
  full_data %>%
    filter(round(Tree_Prediction, 4) == round(as.numeric(selected_rule_values[i]), 4)) %>%
    mutate(Rule = paste("Rule", i))
})) %>%
  mutate(Rule = factor(Rule, levels = paste("Rule", seq_along(selected_rule_values))))
rule_coverage <- rule_points %>%
  group_by(Rule) %>%
  summarise(
    train_points = sum(Time <= training_end),
    test_points = sum(Time >= test_start),
    total_points = n(),
    .groups = "drop"
  ) %>%
  mutate(rule = as.integer(sub("Rule ", "", Rule))) %>%
  select(rule, train_points, test_points, total_points)

summary_output <- tibble(
  setting = c("Predictors", "Rules retained", "Lift metric", "Best cp", "Best minbucket", "Best maxdepth", "Training first-ventile lift", "Test first-ventile lift"),
  value = c(
    paste(predictor_names, collapse = ", "),
    as.character(rules_to_report),
    "RMSE",
    format(best_params$cp, scientific = FALSE, trim = TRUE),
    as.character(best_params$minbucket),
    as.character(best_params$maxdepth),
    sprintf("%.3f", train_lift),
    sprintf("%.3f", test_lift)
  )
)

model_details <- tibble(
  specification = c(
    "Tree method",
    "Candidate hyperparameter combinations",
    "Internal splits",
    "Terminal nodes",
    "Predictors used by selected tree"
  ),
  value = c(
    "rpart regression tree (ANOVA)",
    as.character(nrow(param_grid)),
    as.character(sum(tree_model$frame$var != "<leaf>")),
    as.character(sum(tree_model$frame$var == "<leaf>")),
    paste(names(tree_model$variable.importance), collapse = ", ")
  )
)

write_csv(summary_output, file.path(results_dir, "scenario_b_single_run_summary.csv"))
write_csv(model_details, file.path(results_dir, "scenario_b_single_run_model_details.csv"))
write_csv(
  tibble(rule = seq_along(raw_rules), raw_rule = raw_rules),
  file.path(results_dir, "scenario_b_single_run_rules.csv")
)
write_csv(rule_coverage, file.path(results_dir, "scenario_b_single_run_rule_coverage.csv"))
write_csv(grid_search$results, file.path(results_dir, "scenario_b_single_run_grid_search.csv"))
saveRDS(tree_model, file.path(results_dir, "scenario_b_single_run_tree_model.rds"))

base_theme <- theme_bw(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey70")
  )

actual_forecast_plot <- ggplot(full_data, aes(x = Time)) +
  geom_line(aes(y = Value, color = "Actual"), linewidth = 0.35) +
  geom_line(aes(y = .fitted, color = "ARIMA fitted / forecast"), linewidth = 0.35) +
  geom_point(
    data = filter(full_data, Flagged), aes(y = Value),
    shape = 21, fill = "white", color = "#B2182B", size = 1.8, stroke = 0.8
  ) +
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.5, color = "#333333") +
  scale_color_manual(values = c("Actual" = "#4C78A8", "ARIMA fitted / forecast" = "#E17C05")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Daily demand", color = NULL) +
  base_theme

error_plot <- ggplot(full_data, aes(x = Time, y = .resid)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.45) +
  geom_line(color = "#4C78A8", linewidth = 0.35) +
  geom_point(
    data = filter(full_data, Flagged),
    shape = 21, fill = "white", color = "#B2182B", size = 1.8, stroke = 0.8
  ) +
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.5, color = "#333333") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "ARIMA forecast error") +
  base_theme +
  theme(legend.position = "none")

rule_colors <- c(
  "Actual" = "#4C78A8",
  "ARIMA fitted / forecast" = "#E17C05",
  "Rule 1" = "#B2182B",
  "Rule 2" = "#2166AC",
  "Rule 3" = "#1B7837",
  "Rule 4" = "#762A83",
  "Rule 5" = "#E08214"
)

actual_fitted_rules_plot <- ggplot(full_data, aes(x = Time)) +
  geom_line(aes(y = Value, color = "Actual"), linewidth = 0.35) +
  geom_line(aes(y = .fitted, color = "ARIMA fitted / forecast"), linewidth = 0.35) +
  geom_point(
    data = rule_points,
    aes(y = Value, color = Rule),
    shape = 21, fill = "white", size = 1.8, stroke = 0.8
  ) +
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.5, color = "#333333") +
  scale_color_manual(values = rule_colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Daily demand", color = NULL) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.98),
    legend.key.height = grid::unit(0.28, "cm")
  )

error_rules_plot <- ggplot(full_data, aes(x = Time, y = .resid)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.45) +
  geom_line(color = "#4C78A8", linewidth = 0.35) +
  geom_point(
    data = rule_points,
    aes(color = Rule),
    shape = 21, fill = "white", size = 1.8, stroke = 0.8
  ) +
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.5, color = "#333333") +
  scale_color_manual(values = rule_colors[names(rule_colors) %in% levels(rule_points$Rule)]) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "ARIMA forecast error", color = NULL) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.98),
    legend.key.height = grid::unit(0.28, "cm")
  )

make_lift_plot <- function(data) {
  lift_data <- tree_alert_lift_data(data, ".resid", "Tree_Prediction", n_groups, lift_metric)
  ggplot(lift_data, aes(x = group, y = meanError)) +
    geom_col(fill = "#4C78A8") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#B2182B", linewidth = 0.7) +
    scale_x_continuous(breaks = seq(1, n_groups, by = 2)) +
    scale_y_continuous(breaks = breaks_pretty(n = 6), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Ventile", y = "Lift") +
    base_theme + theme(legend.position = "none")
}

ggsave(file.path(image_dir, "scenario_b_single_run_actual_forecast.png"), actual_forecast_plot, width = 11, height = 5.5, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_single_run_error.png"), error_plot, width = 11, height = 4.7, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_single_run_actual_fitted_rules.png"), actual_fitted_rules_plot, width = 11, height = 5.5, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_single_run_error_rules.png"), error_rules_plot, width = 11, height = 4.7, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_single_run_lift_train.png"), make_lift_plot(train_set), width = 7, height = 4.5, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_single_run_lift_test.png"), make_lift_plot(test_set), width = 7, height = 4.5, dpi = 1000)

cat("Predictors:", paste(predictor_names, collapse = ", "), "\n")
cat("Rules retained:", rules_to_report, "\n")
cat("Best parameters: cp =", best_params$cp, ", minbucket =", best_params$minbucket, ", maxdepth =", best_params$maxdepth, "\n")
cat("Training first-ventile lift:", round(train_lift, 3), "\n")
cat("Test first-ventile lift:", round(test_lift, 3), "\n\n")
cat("Top rules:\n")
cat(paste0(seq_along(raw_rules), ". ", raw_rules, collapse = "\n"), "\n")
