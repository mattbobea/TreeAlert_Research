#!/usr/bin/env Rscript

# Reproduce Appendix B: retain the three strongest positive and three
# strongest negative mean-error terminal-node rules.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Run with Rscript, for example: Rscript Electricity/Appendix_B_positive_negative_rules.R")
}
root_dir <- dirname(dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)))
setwd(root_dir)

required_packages <- c("dplyr", "ggplot2", "lubridate", "rpart", "rpart.plot", "tidyr", "gridExtra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required R packages: ", paste(missing_packages, collapse = ", "))
}

source(file.path(root_dir, "TA_functions", "TA_functions.R"), local = TRUE)
source(file.path(root_dir, "TA_functions", "Rephrase_Rule_Func.R"), local = TRUE)

data_path <- file.path(root_dir, "Electricity", "Elec_data",
                       "time_series_15min_singleindex.csv.gz")
if (!file.exists(data_path)) stop("Missing electricity input: ", data_path)

read_electricity_data <- function(path) {
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  read.csv(connection, stringsAsFactors = FALSE, check.names = FALSE)
}

temporal_encoding <- function(data) {
  data <- data |>
    dplyr::mutate(
      Time = lubridate::parse_date_time(Time, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
      Hour = factor(lubridate::hour(Time)),
      Minute = factor(lubridate::minute(Time)),
      Weekday = factor(lubridate::wday(Time, label = TRUE)),
      DayofMonth = factor(lubridate::mday(Time)),
      WeekofMonth = factor(ceiling(lubridate::day(Time) / 7)),
      Month = factor(lubridate::month(Time, label = TRUE))
    ) |>
    dplyr::arrange(Time)

  for (week in 1:4) {
    lag_label <- paste0("Lag_", week, "_Week")
    lag_time_col <- paste0("LagTime_", week, "_Week")
    data <- data |>
      dplyr::mutate(!!lag_time_col := Time - lubridate::weeks(week)) |>
      dplyr::left_join(
        data |>
          dplyr::select(Time, .resid) |>
          dplyr::rename(!!lag_label := .resid),
        by = stats::setNames("Time", lag_time_col)
      ) |>
      dplyr::select(-dplyr::all_of(lag_time_col))
  }

  data |>
    dplyr::arrange(Time) |>
    tidyr::fill(dplyr::starts_with("Lag_"), .direction = "down")
}

process_data <- function(data) {
  data |>
    dplyr::select(utc_timestamp,
                  DE_LU_load_actual_entsoe_transparency,
                  DE_LU_load_forecast_entsoe_transparency) |>
    dplyr::rename(
      Time = utc_timestamp,
      actual = DE_LU_load_actual_entsoe_transparency,
      forecast = DE_LU_load_forecast_entsoe_transparency
    ) |>
    dplyr::mutate(
      Time = lubridate::parse_date_time(
        Time,
        orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM")
      ),
      .resid = actual - forecast
    ) |>
    stats::na.omit()
}

raw_data <- read_electricity_data(data_path) |>
  dplyr::slice(-(1:5))

analysis_data <- process_data(raw_data) |>
  dplyr::filter(
    Time >= as.POSIXct("2019-02-01", tz = "UTC"),
    Time < as.POSIXct("2019-05-30", tz = "UTC")
  ) |>
  dplyr::mutate(hour = format(Time, "%Y-%m-%d %H")) |>
  dplyr::group_by(hour) |>
  dplyr::filter(actual == max(actual)) |>
  dplyr::ungroup() |>
  dplyr::select(-hour) |>
  temporal_encoding() |>
  dplyr::mutate(Value = actual, .fitted = forecast)

train_set <- analysis_data |>
  dplyr::filter(
    Time > as.POSIXct("2019-01-01", tz = "UTC"),
    Time < as.POSIXct("2019-05-01", tz = "UTC")
  )

tree_model <- rpart::rpart(
  .resid ~ Minute + Hour + Weekday,
  data = train_set,
  method = "anova",
  control = rpart::rpart.control(cp = 0, minbucket = 5)
)

tree_rules <- rpart.plot::rpart.rules(tree_model, cover = FALSE, digits = 4, nn = FALSE)
selection <- tree_alert_select_positive_negative_rules(
  tree_rules = tree_rules,
  n_positive = 3,
  n_negative = 3
)

selected_values <- as.numeric(selection$rule_values)
expected_values <- c(4621.59, 3757.46, 3626.57, -1398.24, -606.43, -534.00)
if (length(selected_values) != length(expected_values) ||
    max(abs(selected_values - expected_values)) > 0.05) {
  stop(
    "Appendix B validation failed. Expected mean errors ",
    paste(expected_values, collapse = ", "), " but obtained ",
    paste(sprintf("%.2f", selected_values), collapse = ", "), "."
  )
}

raw_rule_text <- vapply(
  selection$rule_rows,
  function(row_id) tree_alert_format_rule_raw(tree_rules[row_id, , drop = FALSE]),
  character(1)
)
interpreted_rule_text <- vapply(
  raw_rule_text,
  rephrase_rule,
  character(1),
  minute_increment = 15
)

leaf_nodes <- predict(tree_model, newdata = analysis_data, type = "vector")
plot_data <- dplyr::bind_cols(analysis_data, Leaf_Node = leaf_nodes)
rule_colors <- c("#D55E00", "#E69F00", "#F0E442", "#0072B2", "#56B4E9", "#009E73")
plot_data$Selected_Rule <- factor(
  match(round(plot_data$Leaf_Node, 2), round(selected_values, 2)),
  levels = seq_along(selected_values),
  labels = paste("Rule", seq_along(selected_values))
)

output_results <- file.path(root_dir, "Electricity", "results", "appendix_b_rules.csv")
output_figure <- file.path(root_dir, "Electricity", "images", "Elec_3x3_both.png")
dir.create(dirname(output_results), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_figure), recursive = TRUE, showWarnings = FALSE)

results <- data.frame(
  rule = seq_along(selected_values),
  average_error = selected_values,
  direction = ifelse(selected_values > 0, "under-forecast", "over-forecast"),
  raw_rule = raw_rule_text,
  interpreted_rule = interpreted_rule_text,
  stringsAsFactors = FALSE
)
write.csv(results, output_results, row.names = FALSE)

highlighted <- plot_data[!is.na(plot_data$Selected_Rule), , drop = FALSE]
split_time <- as.POSIXct("2019-05-01", tz = "UTC")
actual_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(Time)) +
  ggplot2::geom_line(ggplot2::aes(y = Value, color = "Actual"), show.legend = FALSE) +
  ggplot2::geom_line(ggplot2::aes(y = .fitted, color = "Fitted"), alpha = 0.6, show.legend = FALSE) +
  ggplot2::geom_point(
    data = highlighted,
    ggplot2::aes(y = Value, color = Selected_Rule, shape = Selected_Rule),
    size = 1.5
  ) +
  ggplot2::geom_vline(xintercept = as.numeric(split_time), linetype = "dotted", color = "red") +
  ggplot2::scale_color_manual(values = c("Actual" = "#4682B4", "Fitted" = "#FF8C00", setNames(rule_colors, levels(plot_data$Selected_Rule)))) +
  ggplot2::scale_shape_manual(values = setNames(rep(1, 6), levels(plot_data$Selected_Rule))) +
  ggplot2::labs(x = NULL, y = "Electricity demand") +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")

residual_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(Time, .resid)) +
  ggplot2::geom_line(color = "#00BFC4") +
  ggplot2::geom_point(
    data = highlighted,
    ggplot2::aes(color = Selected_Rule, shape = Selected_Rule),
    size = 1.5
  ) +
  ggplot2::geom_vline(xintercept = as.numeric(split_time), linetype = "dotted", color = "red") +
  ggplot2::scale_color_manual(values = setNames(rule_colors, levels(plot_data$Selected_Rule))) +
  ggplot2::scale_shape_manual(values = setNames(rep(1, 6), levels(plot_data$Selected_Rule))) +
  ggplot2::labs(x = NULL, y = "Forecast error") +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")

figure <- gridExtra::arrangeGrob(actual_plot, residual_plot, ncol = 1)
ggplot2::ggsave(output_figure, figure, width = 6, height = 4, dpi = 800)

message("Appendix B reproduced successfully.")
message("Rules: ", output_results)
message("Figure: ", output_figure)
