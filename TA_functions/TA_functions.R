# TreeAlert shared parameters and helper functions.

# Prep Parameters
color_string <- c(
  "#9400D3", "#008000", "#0000FF", "#FF8C00", "#000066",
  "#FF00FF", "#AB0000", "#FF69B4", "#7B68EE", "#Ff0000",
  "#7B68EE", "#20B2AA", "#703366", "#32CD32", "#CC6633",
  "#C77CFF", "#8A2BE2", "#00FF7F", "#00BFFF", "#FFA500",
  "#66CDAA", "#BA55D3", "#00CED1", "#4682B4", "#00FFFF"
)

add_fitted_points_func <- function(data_list, color_list, shape_values) {
  layers <- lapply(seq_along(data_list), function(i) {
    ggplot2::geom_point(
      data = data_list[[i]],
      ggplot2::aes(
        x = Time,
        y = Value,
        shape = .resid >= top_5_percent_value | .resid <= bottom_5_percent_value
      ),
      size = 2.5,
      stroke = 1,
      color = color_list[i]
    )
  })

  layers
}

add_resid_points_func <- function(data_list, color_list, shape_values) {
  layers <- lapply(seq_along(data_list), function(i) {
    ggplot2::geom_point(
      data = data_list[[i]],
      ggplot2::aes(
        x = Time,
        y = .resid,
        shape = .resid >= top_5_percent_value | .resid <= bottom_5_percent_value
      ),
      size = 2.5,
      stroke = 1,
      color = color_list[i]
    )
  })

  layers
}

create_monthly_plot <- function(data,
                                start_date,
                                end_date,
                                data_list = list_of_dfs,
                                plot_colors = color_string,
                                shape_values = c("TRUE" = 1, "FALSE" = 16)) {
  ggplot2::ggplot(data, ggplot2::aes(x = Time)) +
    ggplot2::geom_line(ggplot2::aes(y = Value, color = "Actual"), show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = model_1, color = "Fitted"), alpha = 0.6, show.legend = FALSE) +
    add_fitted_points_func(data_list, plot_colors, shape_values) +
    ggplot2::scale_shape_manual(values = shape_values) +
    ggplot2::scale_color_manual(values = c("Actual" = "#4682B4", "Fitted" = "#FF8C00")) +
    ggplot2::scale_x_datetime(
      limits = as.POSIXct(c(start_date, end_date)),
      date_labels = "%m-%d",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::labs(title = paste(""), x = NULL, y = NULL, color = "Variable") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggtext::element_markdown(size = 10),
      plot.title.position = "panel"
    )
}

tree_alert_lift_data <- function(df,
                                 actual_col,
                                 pred_col,
                                 n_groups = 20,
                                 lift_metric = c("mean_abs", "rmse")) {
  lift_metric <- match.arg(lift_metric)
  actual <- df[[actual_col]]
  score <- abs(df[[pred_col]])

  ord <- order(score, decreasing = TRUE)
  actual_ord <- actual[ord]
  n <- length(actual_ord)

  # When an evaluation fold contains fewer observations than the requested
  # number of groups, retain one ranked observation per available group so
  # that the highest-ranked group remains defined.
  group <- if (n < n_groups) {
    seq_len(n)
  } else {
    ceiling(seq_len(n) / (n / n_groups))
  }
  group[group > n_groups] <- n_groups

  lift_summary <- if (lift_metric == "rmse") {
    function(error) sqrt(mean(error^2, na.rm = TRUE))
  } else {
    function(error) mean(abs(error), na.rm = TRUE)
  }

  df_lift <- data.frame(group = group, error = actual_ord) |>
    dplyr::group_by(group) |>
    dplyr::summarise(meanError = lift_summary(error), .groups = "drop")

  df_lift$percentile <- df_lift$group * (100 / n_groups)
  df_lift$meanError <- df_lift$meanError / lift_summary(actual_ord)

  df_lift
}

tree_alert_compute_lift_numeric <- function(df,
                                            actual_col,
                                            pred_col,
                                            n_groups = 20,
                                            lift_metric = c("mean_abs", "rmse")) {
  df_lift <- tree_alert_lift_data(
    df = df,
    actual_col = actual_col,
    pred_col = pred_col,
    n_groups = n_groups,
    lift_metric = lift_metric
  )

  df_lift$meanError[df_lift$group == 1]
}

tree_alert_lift_summary_function <- function(lift_metric = c("mean_abs", "rmse")) {
  lift_metric <- match.arg(lift_metric)
  if (lift_metric == "rmse") {
    function(error) sqrt(mean(error^2, na.rm = TRUE))
  } else {
    function(error) mean(abs(error), na.rm = TRUE)
  }
}

tree_alert_compute_top_percent_lift_numeric <- function(df,
                                                        actual_col,
                                                        pred_col,
                                                        threshold,
                                                        lift_metric = c("mean_abs", "rmse")) {
  lift_metric <- match.arg(lift_metric)
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      is.na(threshold) || threshold <= 0 || threshold >= 1) {
    stop("threshold must be a single number between 0 and 1.")
  }

  actual <- df[[actual_col]]
  score <- abs(df[[pred_col]])
  valid <- is.finite(score) & !is.na(actual)
  actual <- actual[valid]
  score <- score[valid]
  total_count <- length(actual)

  if (total_count == 0) {
    return(data.frame(
      selected_count = 0,
      total_count = 0,
      percent_selected = NA_real_,
      lift = NA_real_
    ))
  }

  selected_count <- max(1, floor(threshold * total_count))
  ord <- order(score, decreasing = TRUE)
  selected_actual <- actual[ord][seq_len(selected_count)]
  lift_summary <- tree_alert_lift_summary_function(lift_metric)
  denominator <- lift_summary(actual)

  lift <- if (is.na(denominator) || denominator == 0) {
    NA_real_
  } else {
    lift_summary(selected_actual) / denominator
  }

  data.frame(
    selected_count = selected_count,
    total_count = total_count,
    percent_selected = selected_count / total_count * 100,
    lift = lift
  )
}

tree_alert_make_lift_chart <- function(df,
                                       actual_col,
                                       pred_col,
                                       n_groups = 20,
                                       bar_fill = "#FF8C00",
                                       x_break_by = 2,
                                       axis_title_size = 20,
                                       axis_text_x_size = 18,
                                       axis_text_y_size = 20,
                                       lift_metric = c("mean_abs", "rmse")) {
  df_lift <- tree_alert_lift_data(
    df = df,
    actual_col = actual_col,
    pred_col = pred_col,
    n_groups = n_groups,
    lift_metric = lift_metric
  )

  g2 <- ggplot2::ggplot(df_lift, ggplot2::aes(x = group, y = meanError)) +
    ggplot2::geom_col(fill = bar_fill) +
    ggplot2::scale_x_continuous(breaks = seq(1, n_groups, by = x_break_by)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = NULL, x = "Ventile", y = "Lift") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray95"),
      axis.title.x = ggplot2::element_text(size = axis_title_size, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = axis_title_size, margin = ggplot2::margin(r = 10)),
      axis.text.x = ggplot2::element_text(size = axis_text_x_size),
      axis.text.y = ggplot2::element_text(size = axis_text_y_size)
    )

  print(g2)

  df_lift$meanError[df_lift$group == 1]
}

tree_alert_compute_lift <- function(data,
                                    model,
                                    pred_col_name,
                                    label,
                                    actual_col = ".resid",
                                    plot = TRUE,
                                    n_groups = 20,
                                    lift_label = "First Ventile",
                                    axis_title_size = 20,
                                    axis_text_x_size = 18,
                                    axis_text_y_size = 20,
                                    lift_metric = c("mean_abs", "rmse")) {
  data_with_pred <- data
  data_with_pred[[pred_col_name]] <- as.numeric(predict(model, newdata = data, type = "vector"))

  lift_value <- if (plot) {
    tree_alert_make_lift_chart(
      df = data_with_pred,
      actual_col = actual_col,
      pred_col = pred_col_name,
      n_groups = n_groups,
      axis_title_size = axis_title_size,
      axis_text_x_size = axis_text_x_size,
      axis_text_y_size = axis_text_y_size,
      lift_metric = lift_metric
    )
  } else {
    tree_alert_compute_lift_numeric(
      df = data_with_pred,
      actual_col = actual_col,
      pred_col = pred_col_name,
      n_groups = n_groups,
      lift_metric = lift_metric
    )
  }

  cat(label, "Lift (", lift_label, "): ", lift_value, "\n", sep = "")

  list(
    lift = lift_value,
    merged_data = data_with_pred
  )
}

tree_alert_grid_search <- function(train_set,
                                   formula,
                                   param_grid,
                                   n_groups = 20,
                                   actual_col = ".resid",
                                   pred_col_name = "Pred_Temp",
                                   cp_digits = NULL,
                                   lift_metric = c("mean_abs", "rmse")) {
  evaluate_params <- function(params_row) {
    cp <- params_row$cp
    minbucket <- params_row$minbucket
    maxdepth <- params_row$maxdepth

    model <- rpart::rpart(
      formula,
      data = train_set,
      method = "anova",
      control = rpart::rpart.control(
        cp = cp,
        minbucket = minbucket,
        maxdepth = maxdepth
      )
    )

    train_df <- train_set
    train_df[[pred_col_name]] <- as.numeric(predict(model, newdata = train_set, type = "vector"))

    lift_value <- tree_alert_compute_lift_numeric(
      df = train_df,
      actual_col = actual_col,
      pred_col = pred_col_name,
      n_groups = n_groups,
      lift_metric = lift_metric
    )

    data.frame(
      cp = if (is.null(cp_digits)) cp else round(cp, cp_digits),
      minbucket = as.integer(minbucket),
      maxdepth = as.integer(maxdepth),
      first_ventile_lift = lift_value
    )
  }

  results <- do.call(
    rbind,
    lapply(seq_len(nrow(param_grid)), function(i) {
      evaluate_params(param_grid[i, , drop = FALSE])
    })
  )

  best_params <- results |>
    dplyr::filter(!is.na(first_ventile_lift)) |>
    dplyr::arrange(dplyr::desc(first_ventile_lift)) |>
    dplyr::slice(1)

  list(results = results, best_params = best_params)
}

tree_alert_threshold_grid_search <- function(train_set,
                                             formula,
                                             param_grid,
                                             threshold,
                                             actual_col = ".resid",
                                             pred_col_name = "Pred_Threshold",
                                             cp_digits = NULL,
                                             lift_metric = c("mean_abs", "rmse")) {
  lift_metric <- match.arg(lift_metric)

  evaluate_params <- function(params_row) {
    cp <- params_row$cp
    minbucket <- params_row$minbucket
    maxdepth <- params_row$maxdepth

    model <- rpart::rpart(
      formula,
      data = train_set,
      method = "anova",
      control = rpart::rpart.control(
        cp = cp,
        minbucket = minbucket,
        maxdepth = maxdepth
      )
    )

    train_df <- train_set
    train_df[[pred_col_name]] <- as.numeric(predict(model, newdata = train_set, type = "vector"))

    lift_value <- tree_alert_compute_top_percent_lift_numeric(
      df = train_df,
      actual_col = actual_col,
      pred_col = pred_col_name,
      threshold = threshold,
      lift_metric = lift_metric
    )$lift

    data.frame(
      cp = if (is.null(cp_digits)) cp else round(cp, cp_digits),
      minbucket = as.integer(minbucket),
      maxdepth = as.integer(maxdepth),
      threshold_lift = lift_value
    )
  }

  results <- do.call(
    rbind,
    lapply(seq_len(nrow(param_grid)), function(i) {
      evaluate_params(param_grid[i, , drop = FALSE])
    })
  )

  best_params <- results |>
    dplyr::filter(!is.na(threshold_lift)) |>
    dplyr::arrange(dplyr::desc(threshold_lift)) |>
    dplyr::slice(1)

  list(results = results, best_params = best_params)
}

tree_alert_train_best_model <- function(train_set, formula, best_params) {
  rpart::rpart(
    formula,
    data = train_set,
    method = "anova",
    control = rpart::rpart.control(
      cp = best_params$cp,
      minbucket = best_params$minbucket,
      maxdepth = best_params$maxdepth
    )
  )
}

tree_alert_ordered_leave_block_validation <- function(data,
                                                       test_set,
                                                       formula,
                                                       param_grid,
                                                       n_folds = 20,
                                                       n_groups = 20,
                                                       actual_col = ".resid",
                                                       pred_col_name = "Pred_Ordered_Validation",
                                                       lift_metric = c("mean_abs", "rmse"),
                                                       cp_digits = NULL) {
  lift_metric <- match.arg(lift_metric)

  if (!is.data.frame(data) || !is.data.frame(test_set)) {
    stop("data and test_set must both be data frames.")
  }
  if (!is.numeric(n_folds) || length(n_folds) != 1 ||
      is.na(n_folds) || n_folds < 2 || n_folds != floor(n_folds)) {
    stop("n_folds must be a single integer greater than or equal to 2.")
  }
  if (nrow(data) < n_folds) {
    stop("data must contain at least n_folds observations.")
  }
  if (!actual_col %in% names(data) || !actual_col %in% names(test_set)) {
    stop("actual_col must be present in both data and test_set.")
  }

  ordered_data <- as.data.frame(data)
  if ("Time" %in% names(ordered_data)) {
    ordered_data <- ordered_data[order(ordered_data$Time), , drop = FALSE]
  }

  fold_id <- floor((seq_len(nrow(ordered_data)) - 1) * n_folds / nrow(ordered_data)) + 1

  fold_results <- dplyr::bind_rows(lapply(seq_len(n_folds), function(fold_number) {
    fold_train <- ordered_data[fold_id != fold_number, , drop = FALSE]
    held_out <- ordered_data[fold_id == fold_number, , drop = FALSE]

    grid_search <- tree_alert_grid_search(
      train_set = fold_train,
      formula = formula,
      param_grid = param_grid,
      n_groups = n_groups,
      actual_col = actual_col,
      pred_col_name = pred_col_name,
      cp_digits = cp_digits,
      lift_metric = lift_metric
    )

    if (nrow(grid_search$best_params) != 1) {
      stop("No valid optimal parameter set was found for fold ", fold_number, ".")
    }

    fold_model <- tree_alert_train_best_model(
      train_set = fold_train,
      formula = formula,
      best_params = grid_search$best_params
    )

    fold_train[[pred_col_name]] <- as.numeric(
      predict(fold_model, newdata = fold_train, type = "vector")
    )
    fixed_test <- as.data.frame(test_set)
    fixed_test[[pred_col_name]] <- as.numeric(
      predict(fold_model, newdata = fixed_test, type = "vector")
    )

    time_value <- function(values, position) {
      if (!"Time" %in% names(ordered_data) || !nrow(values)) {
        return(NA_character_)
      }
      as.character(values$Time[position])
    }

    data.frame(
      fold = fold_number,
      held_out_start = time_value(held_out, 1),
      held_out_end = time_value(held_out, nrow(held_out)),
      held_out_periods = nrow(held_out),
      train_periods = nrow(fold_train),
      test_periods = nrow(fixed_test),
      cp = grid_search$best_params$cp[[1]],
      minbucket = grid_search$best_params$minbucket[[1]],
      maxdepth = grid_search$best_params$maxdepth[[1]],
      train_lift = tree_alert_compute_lift_numeric(
        df = fold_train,
        actual_col = actual_col,
        pred_col = pred_col_name,
        n_groups = n_groups,
        lift_metric = lift_metric
      ),
      test_lift = tree_alert_compute_lift_numeric(
        df = fixed_test,
        actual_col = actual_col,
        pred_col = pred_col_name,
        n_groups = n_groups,
        lift_metric = lift_metric
      ),
      stringsAsFactors = FALSE
    )
  }))

  interval_summary <- dplyr::bind_rows(lapply(
    c("train_lift", "test_lift"),
    function(lift_column) {
      lift_values <- fold_results[[lift_column]]
      lift_values <- lift_values[is.finite(lift_values)]
      if (!length(lift_values)) {
        stop("No finite lift values were produced for ", lift_column, ".")
      }

      lower <- unname(stats::quantile(lift_values, probs = 0.025, names = FALSE))
      upper <- unname(stats::quantile(lift_values, probs = 0.975, names = FALSE))

      data.frame(
        series = if (lift_column == "train_lift") "Retained training" else "Fixed test",
        n_folds = length(lift_values),
        mean_lift = mean(lift_values),
        median_lift = stats::median(lift_values),
        empirical_95_lower = lower,
        empirical_95_upper = upper,
        interval_above_one = lower > 1,
        interval_includes_one = lower <= 1 && upper >= 1,
        stringsAsFactors = FALSE
      )
    }
  ))

  list(
    fold_results = fold_results,
    interval_summary = interval_summary,
    n_folds = n_folds,
    n_groups = n_groups,
    lift_metric = lift_metric
  )
}

tree_alert_ordered_validation_plot <- function(validation_results,
                                                image_path = NULL,
                                                width = 8.5,
                                                height = 5.5,
                                                dpi = 1000,
                                                legend_position = c(0.02, 0.04),
                                                legend_justification = c("left", "bottom")) {
  if (is.null(validation_results$fold_results) ||
      is.null(validation_results$interval_summary)) {
    stop("validation_results must be returned by tree_alert_ordered_leave_block_validation().")
  }

  fold_results <- validation_results$fold_results
  interval_summary <- validation_results$interval_summary
  n_folds <- validation_results$n_folds

  plot_data <- dplyr::bind_rows(
    data.frame(
      fold = fold_results$fold,
      series = "Retained training",
      lift = fold_results$train_lift,
      stringsAsFactors = FALSE
    ),
    data.frame(
      fold = fold_results$fold,
      series = "Fixed test",
      lift = fold_results$test_lift,
      stringsAsFactors = FALSE
    )
  )

  ribbon_data <- interval_summary |>
    dplyr::transmute(
      series,
      xmin = 1,
      xmax = n_folds,
      ymin = empirical_95_lower,
      ymax = empirical_95_upper
    )

  # Keep the no-concentration benchmark visibly inside the plotting region.
  reference_line <- data.frame(y = 1, series = "Lift = 1")

  validation_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = fold, y = lift, color = series, shape = series)
  ) +
    ggplot2::geom_rect(
      data = ribbon_data,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = series),
      inherit.aes = FALSE,
      alpha = 0.16,
      color = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_hline(
      data = reference_line,
      ggplot2::aes(yintercept = y, color = series, linetype = series),
      linewidth = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(
      size = 2.4,
      fill = "white",
      stroke = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Retained training" = "#4682B4",
        "Fixed test" = "#FF8C00",
        "Lift = 1" = "black"
      ),
      breaks = c("Retained training", "Fixed test", "Lift = 1"),
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Retained training" = "#4682B4", "Fixed test" = "#FF8C00")
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Retained training" = 21,
        "Fixed test" = 24,
        "Lift = 1" = NA
      ),
      breaks = c("Retained training", "Fixed test", "Lift = 1"),
      drop = FALSE,
      guide = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Retained training" = "solid",
        "Fixed test" = "solid",
        "Lift = 1" = "solid"
      ),
      breaks = c("Retained training", "Fixed test", "Lift = 1"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(c(seq(1, n_folds, by = 2), n_folds))),
      limits = c(1, n_folds)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(1, 2.8, by = 0.3),
      limits = c(1, 2.8),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::labs(
      x = "Ordered leave-one-block-out fold",
      y = "Lift",
      color = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(24, 21, NA),
          linetype = c("solid", "solid", "solid"),
          linewidth = c(0.8, 0.8, 0.8)
        )
      ),
      linetype = "none"
    ) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "inside",
      legend.position.inside = legend_position,
      legend.justification = legend_justification,
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(
        fill = scales::alpha("white", 0.85),
        color = "grey70",
        linewidth = 0.3
      ),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 15),
      legend.key.width = grid::unit(0.9, "cm"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (!is.null(image_path)) {
    ggplot2::ggsave(
      filename = image_path,
      plot = validation_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  validation_plot
}

tree_alert_threshold_lift_sensitivity_analysis <- function(train_set,
                                                           test_set,
                                                           formula,
                                                           param_grid,
                                                           thresholds = seq(0.01, 0.10, by = 0.01),
                                                           actual_col = ".resid",
                                                           pred_col_name = "Pred_Threshold_Sensitivity",
                                                           lift_metric = c("mean_abs", "rmse"),
                                                           cp_digits = NULL) {
  lift_metric <- match.arg(lift_metric)
  thresholds <- sort(unique(as.numeric(thresholds)))
  thresholds <- thresholds[!is.na(thresholds) & thresholds > 0 & thresholds < 1]
  if (length(thresholds) == 0) {
    stop("thresholds must contain at least one value between 0 and 1.")
  }

  do.call(
    rbind,
    lapply(thresholds, function(threshold) {
      grid_search <- tree_alert_threshold_grid_search(
        train_set = train_set,
        formula = formula,
        param_grid = param_grid,
        threshold = threshold,
        actual_col = actual_col,
        pred_col_name = pred_col_name,
        cp_digits = cp_digits,
        lift_metric = lift_metric
      )

      best_model <- tree_alert_train_best_model(
        train_set = train_set,
        formula = formula,
        best_params = grid_search$best_params
      )

      threshold_train <- train_set
      threshold_train[[pred_col_name]] <- as.numeric(
        predict(best_model, newdata = train_set, type = "vector")
      )

      threshold_test <- test_set
      threshold_test[[pred_col_name]] <- as.numeric(
        predict(best_model, newdata = test_set, type = "vector")
      )

      train_lift <- tree_alert_compute_top_percent_lift_numeric(
        df = threshold_train,
        actual_col = actual_col,
        pred_col = pred_col_name,
        threshold = threshold,
        lift_metric = lift_metric
      )
      test_lift <- tree_alert_compute_top_percent_lift_numeric(
        df = threshold_test,
        actual_col = actual_col,
        pred_col = pred_col_name,
        threshold = threshold,
        lift_metric = lift_metric
      )

      data.frame(
        threshold_percent = threshold * 100,
        cp = grid_search$best_params$cp,
        minbucket = grid_search$best_params$minbucket,
        maxdepth = grid_search$best_params$maxdepth,
        train_selected = train_lift$selected_count,
        train_total = train_lift$total_count,
        train_percent_selected = train_lift$percent_selected,
        train_lift = train_lift$lift,
        test_selected = test_lift$selected_count,
        test_total = test_lift$total_count,
        test_percent_selected = test_lift$percent_selected,
        test_lift = test_lift$lift
      )
    })
  )
}

tree_alert_lift_axis_limits <- function(x, step = 0.1) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(0, step))
  }

  c(
    floor(min(x) / step) * step,
    ceiling(max(x) / step) * step
  )
}

tree_alert_lift_axis_breaks <- function(x, step = 0.1) {
  limits <- tree_alert_lift_axis_limits(x, step)
  seq(limits[1], limits[2], by = step)
}

tree_alert_lift_axis_breaks_adaptive <- function(x,
                                                 step = 0.1,
                                                 max_breaks = 8,
                                                 pretty_n = 6) {
  regular_breaks <- tree_alert_lift_axis_breaks(x, step)
  if (length(regular_breaks) <= max_breaks) {
    return(regular_breaks)
  }

  limits <- tree_alert_lift_axis_limits(x, step)
  pretty_breaks <- scales::breaks_pretty(n = pretty_n)(limits)
  pretty_breaks[pretty_breaks >= limits[1] & pretty_breaks <= limits[2]]
}

tree_alert_lift_axis_settings <- function(x,
                                          fixed_limits = NULL,
                                          fixed_break_interval = 0.5) {
  if (is.null(fixed_limits)) {
    return(list(
      limits = tree_alert_lift_axis_limits(x),
      breaks = tree_alert_lift_axis_breaks_adaptive(x)
    ))
  }

  if (!is.numeric(fixed_limits) ||
      length(fixed_limits) != 2 ||
      any(is.na(fixed_limits)) ||
      fixed_limits[1] >= fixed_limits[2]) {
    stop("fixed_limits must be a numeric vector of length 2 with increasing values.")
  }

  if (!is.numeric(fixed_break_interval) ||
      length(fixed_break_interval) != 1 ||
      is.na(fixed_break_interval) ||
      fixed_break_interval <= 0) {
    stop("fixed_break_interval must be a single positive number.")
  }

  list(
    limits = fixed_limits,
    breaks = seq(fixed_limits[1], fixed_limits[2], by = fixed_break_interval)
  )
}

tree_alert_threshold_lift_sensitivity_plot <- function(threshold_results,
                                                       image_path = NULL,
                                                       width = 7,
                                                       height = 4.2,
                                                       dpi = 1000) {
  required_cols <- c("threshold_percent", "train_lift", "test_lift")
  missing_cols <- setdiff(required_cols, names(threshold_results))
  if (length(missing_cols) > 0) {
    stop(
      "threshold_results is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  plot_data <- rbind(
    data.frame(
      threshold_percent = threshold_results$threshold_percent,
      set = "Training",
      lift = threshold_results$train_lift
    ),
    data.frame(
      threshold_percent = threshold_results$threshold_percent,
      set = "Test",
      lift = threshold_results$test_lift
    )
  )

  plot_data$set <- factor(plot_data$set, levels = c("Training", "Test"))
  x_breaks <- sort(unique(plot_data$threshold_percent))

  threshold_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = threshold_percent,
      y = lift,
      color = set,
      shape = set,
      group = set
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.6, stroke = 0.8, fill = "white") +
    ggplot2::scale_color_manual(values = c("Training" = "grey25", "Test" = "#B2182B")) +
    ggplot2::scale_shape_manual(values = c("Training" = 21, "Test" = 24)) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = function(x) paste0(scales::number(x, accuracy = 1), "%")
    ) +
    ggplot2::scale_y_continuous(
      breaks = tree_alert_lift_axis_breaks_adaptive(plot_data$lift),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::labs(
      x = "Lift's top quantile size",
      y = "Lift",
      color = NULL,
      shape = NULL
    ) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(
        fill = ggplot2::alpha("white", 0.85),
        color = "grey70",
        linewidth = 0.25
      ),
      legend.key = ggplot2::element_rect(fill = ggplot2::alpha("white", 0)),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )

  if (!is.null(image_path)) {
    ggplot2::ggsave(
      image_path,
      plot = threshold_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  threshold_plot
}

tree_alert_sensitivity_analysis <- function(train_set,
                                            test_set,
                                            formula,
                                            param_grid,
                                            fixed_params = NULL,
                                            n_groups = 20,
                                            actual_col = ".resid",
                                            pred_col_name = "Pred_Sensitivity",
                                            lift_metric = c("mean_abs", "rmse"),
                                            reference_train_lift = NULL,
                                            reference_test_lift = NULL,
                                            train_image_path = NULL,
                                            test_image_path = NULL,
                                            width = 8,
                                            height = 4.0,
                                            dpi = 1000,
                                            lift_axis_limits = c(1, 3),
                                            lift_axis_break_interval = 0.5) {
  lift_metric <- match.arg(lift_metric)
  parameter_names <- c("cp", "minbucket", "maxdepth")
  missing_params <- setdiff(parameter_names, names(param_grid))
  if (length(missing_params) > 0) {
    stop("param_grid is missing required columns: ", paste(missing_params, collapse = ", "))
  }

  if (is.null(fixed_params)) {
    fixed_params <- param_grid[1, parameter_names, drop = FALSE]
  }

  missing_fixed <- setdiff(parameter_names, names(fixed_params))
  if (length(missing_fixed) > 0) {
    stop("fixed_params is missing required columns: ", paste(missing_fixed, collapse = ", "))
  }

  fixed_params <- fixed_params[1, parameter_names, drop = FALSE]

  format_sensitivity_value <- function(parameter, value) {
    if (parameter == "cp") {
      return(formatC(as.numeric(value), format = "fg", digits = 6))
    }

    as.character(as.integer(value))
  }

  sensitivity_param_grid <- do.call(
    rbind,
    lapply(parameter_names, function(parameter) {
      values <- sort(unique(param_grid[[parameter]]))

      do.call(
        rbind,
        lapply(values, function(value) {
          params <- fixed_params
          params[[parameter]] <- value
          data.frame(
            parameter = parameter,
            value = value,
            cp = params$cp,
            minbucket = as.integer(params$minbucket),
            maxdepth = as.integer(params$maxdepth),
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )

  sensitivity_results <- do.call(
    rbind,
    lapply(seq_len(nrow(sensitivity_param_grid)), function(i) {
      params <- sensitivity_param_grid[i, , drop = FALSE]

      sensitivity_model <- rpart::rpart(
        formula,
        data = train_set,
        method = "anova",
        control = rpart::rpart.control(
          cp = params$cp,
          minbucket = params$minbucket,
          maxdepth = params$maxdepth
        )
      )

      sensitivity_train <- train_set
      sensitivity_train[[pred_col_name]] <- as.numeric(
        predict(sensitivity_model, newdata = train_set, type = "vector")
      )

      sensitivity_test <- test_set
      sensitivity_test[[pred_col_name]] <- as.numeric(
        predict(sensitivity_model, newdata = test_set, type = "vector")
      )

      data.frame(
        parameter = params$parameter,
        value = params$value,
        cp = params$cp,
        minbucket = params$minbucket,
        maxdepth = params$maxdepth,
        train_lift = tree_alert_compute_lift_numeric(
          df = sensitivity_train,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        ),
        test_lift = tree_alert_compute_lift_numeric(
          df = sensitivity_test,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        )
      )
    })
  )

  parameter_value_levels <- unlist(
    lapply(parameter_names, function(parameter) {
      values <- sort(unique(param_grid[[parameter]]))
      paste(
        parameter,
        vapply(
          values,
          function(value) format_sensitivity_value(parameter, value),
          character(1)
        ),
        sep = "__"
      )
    }),
    use.names = FALSE
  )

  sensitivity_plot_data <- sensitivity_results |>
    dplyr::mutate(
      value_label = mapply(format_sensitivity_value, parameter, value),
      value = factor(
        paste(parameter, value_label, sep = "__"),
        levels = parameter_value_levels
      ),
      parameter = factor(
        parameter,
        levels = c("cp", "minbucket", "maxdepth"),
        labels = c("Complexity parameter", "Minimum bucket", "Maximum depth")
      )
    )

  make_sensitivity_plot <- function(y_col, y_label, reference_lift = NULL) {
    y_values <- sensitivity_plot_data[[y_col]]
    if (!is.null(reference_lift)) {
      y_values <- c(y_values, reference_lift)
    }
    lift_axis <- tree_alert_lift_axis_settings(
      y_values,
      fixed_limits = lift_axis_limits,
      fixed_break_interval = lift_axis_break_interval
    )

    sensitivity_plot <- ggplot2::ggplot(
      sensitivity_plot_data,
      ggplot2::aes(x = value, y = .data[[y_col]])
    )

    if (!is.null(reference_lift)) {
      sensitivity_plot <- sensitivity_plot +
        ggplot2::geom_hline(
          yintercept = reference_lift,
          color = "#B2182B",
          linetype = "dashed",
          linewidth = 0.55
        )
    }

    sensitivity_plot <- sensitivity_plot +
      ggplot2::geom_line(
        ggplot2::aes(group = 1),
        color = "grey30",
        linewidth = 0.45
      ) +
      ggplot2::geom_point(
        shape = 21,
        size = 2.2,
        stroke = 0.45,
        fill = "white",
        color = "grey20"
      ) +
      ggplot2::facet_wrap(~ parameter, scales = "free_x", nrow = 1) +
      ggplot2::scale_x_discrete(labels = function(x) sub("^.*__", "", x)) +
      ggplot2::scale_y_continuous(
        breaks = lift_axis$breaks,
        labels = scales::label_number(accuracy = 0.1)
      ) +
      ggplot2::coord_cartesian(ylim = lift_axis$limits) +
      ggplot2::labs(
        x = "Hyperparameter Value",
        y = y_label
      ) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "grey85", color = "grey30"),
        strip.text = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    sensitivity_plot
  }

  train_plot <- make_sensitivity_plot("train_lift", "Training Lift", reference_train_lift)
  test_plot <- make_sensitivity_plot("test_lift", "Test Lift", reference_test_lift)

  if (!is.null(train_image_path)) {
    ggplot2::ggsave(
      train_image_path,
      plot = train_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  if (!is.null(test_image_path)) {
    ggplot2::ggsave(
      test_image_path,
      plot = test_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  list(
    results = sensitivity_results,
    train_plot = train_plot,
    test_plot = test_plot
  )
}

tree_alert_performance_summary <- function(list_of_dfs,
                                           train_set,
                                           test_set,
                                           train_lift,
                                           test_lift,
                                           print_output = TRUE) {
  flagged_data <- dplyr::bind_rows(list_of_dfs) |>
    dplyr::distinct(Time, .keep_all = TRUE)

  train_flagged <- flagged_data |>
    dplyr::filter(Time %in% train_set$Time) |>
    nrow()

  test_flagged <- flagged_data |>
    dplyr::filter(Time %in% test_set$Time) |>
    nrow()

  train_total <- nrow(train_set)
  test_total <- nrow(test_set)
  train_percent_flagged <- 100 * train_flagged / train_total
  test_percent_flagged <- 100 * test_flagged / test_total

  summary <- data.frame(
    set = c("Training", "Test"),
    flagged = c(train_flagged, test_flagged),
    total = c(train_total, test_total),
    percent_flagged = c(train_percent_flagged, test_percent_flagged),
    lift = c(train_lift, test_lift)
  )

  output <- c(
    sprintf(
      "Training: %s flagged / %s, %.1f%%, lift %.2f",
      scales::comma(train_flagged),
      scales::comma(train_total),
      train_percent_flagged,
      train_lift
    ),
    sprintf(
      "Test:     %s flagged / %s, %.1f%%, lift %.2f",
      scales::comma(test_flagged),
      scales::comma(test_total),
      test_percent_flagged,
      test_lift
    )
  )

  if (print_output) {
    cat(paste(output, collapse = "\n"), "\n", sep = "")
  }

  list(
    summary = summary,
    output = output
  )
}

tree_alert_split_sensitivity_analysis <- function(data,
                                                  formula,
                                                  param_grid,
                                                  current_test_size = NULL,
                                                  n_groups = 20,
                                                  actual_col = ".resid",
                                                  pred_col_name = "Pred_Split_Sensitivity",
                                                  lift_metric = c("mean_abs", "rmse"),
                                                  train_share_min = 70,
                                                  train_share_max = 90,
                                                  train_share_step = 1,
                                                  train_share_axis_step = 5,
                                                  reference_train_lift = NULL,
                                                  reference_test_lift = NULL,
                                                  train_image_path = NULL,
                                                  test_image_path = NULL,
                                                  width = 8,
                                                  height = 4.2,
                                                  dpi = 1000,
                                                  lift_axis_limits = c(1, 3),
                                                  lift_axis_break_interval = 0.5) {
  lift_metric <- match.arg(lift_metric)

  split_data <- as.data.frame(data)
  if ("Time" %in% names(split_data)) {
    split_data <- split_data[order(split_data$Time), ]
  }

  n_obs <- nrow(split_data)
  if (train_share_min <= 0 || train_share_max >= 100) {
    stop("train_share_min and train_share_max must be between 0 and 100.")
  }
  if (train_share_min > train_share_max) {
    stop("train_share_min must be less than or equal to train_share_max.")
  }
  if (!is.numeric(train_share_axis_step) ||
      length(train_share_axis_step) != 1 ||
      is.na(train_share_axis_step) ||
      train_share_axis_step <= 0) {
    stop("train_share_axis_step must be a single positive number.")
  }

  target_train_shares <- seq(train_share_min, train_share_max, by = train_share_step)
  x_axis_breaks <- seq(
    ceiling(train_share_min / train_share_axis_step) * train_share_axis_step,
    floor(train_share_max / train_share_axis_step) * train_share_axis_step,
    by = train_share_axis_step
  )
  x_axis_breaks <- sort(unique(c(train_share_min, x_axis_breaks, train_share_max)))

  split_grid <- data.frame(
    target_train_share = target_train_shares,
    split_index = floor(target_train_shares / 100 * n_obs)
  )
  split_grid <- split_grid |>
    dplyr::filter(split_index > 1, split_index < n_obs) |>
    dplyr::arrange(split_index) |>
    dplyr::distinct(split_index, .keep_all = TRUE)

  split_results <- do.call(
    rbind,
    lapply(seq_len(nrow(split_grid)), function(i) {
      split_index <- split_grid$split_index[i]
      split_train <- split_data[seq_len(split_index), , drop = FALSE]
      split_test <- split_data[(split_index + 1):n_obs, , drop = FALSE]

      grid_search <- tree_alert_grid_search(
        train_set = split_train,
        formula = formula,
        param_grid = param_grid,
        n_groups = n_groups,
        pred_col_name = pred_col_name,
        lift_metric = lift_metric
      )

      split_model <- tree_alert_train_best_model(
        train_set = split_train,
        formula = formula,
        best_params = grid_search$best_params
      )

      split_train[[pred_col_name]] <- as.numeric(
        predict(split_model, newdata = split_train, type = "vector")
      )
      split_test[[pred_col_name]] <- as.numeric(
        predict(split_model, newdata = split_test, type = "vector")
      )

      data.frame(
        train_share = split_grid$target_train_share[i],
        train_periods = nrow(split_train),
        test_periods = nrow(split_test),
        cp = grid_search$best_params$cp,
        minbucket = grid_search$best_params$minbucket,
        maxdepth = grid_search$best_params$maxdepth,
        train_lift = tree_alert_compute_lift_numeric(
          df = split_train,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        ),
        test_lift = tree_alert_compute_lift_numeric(
          df = split_test,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        )
      )
    })
  )

  make_split_plot <- function(y_col, y_label, reference_lift = NULL) {
    y_values <- split_results[[y_col]]
    if (!is.null(reference_lift)) {
      y_values <- c(y_values, reference_lift)
    }
    lift_axis <- tree_alert_lift_axis_settings(
      y_values,
      fixed_limits = lift_axis_limits,
      fixed_break_interval = lift_axis_break_interval
    )

    split_plot <- ggplot2::ggplot(
      split_results,
      ggplot2::aes(x = train_share, y = .data[[y_col]])
    )

    if (!is.null(reference_lift)) {
      split_plot <- split_plot +
        ggplot2::geom_hline(
          yintercept = reference_lift,
          color = "#B2182B",
          linetype = "dashed",
          linewidth = 0.55
        )
    }

    split_plot <- split_plot +
      ggplot2::geom_line(color = "grey30", linewidth = 0.45) +
      ggplot2::geom_point(shape = 21, size = 2.2, fill = "grey92", color = "grey20") +
      ggplot2::scale_x_continuous(
        limits = c(train_share_min, train_share_max),
        breaks = x_axis_breaks,
        labels = function(x) paste0(scales::number(x, accuracy = 1), "%")
      ) +
      ggplot2::scale_y_continuous(
        breaks = lift_axis$breaks,
        labels = scales::label_number(accuracy = 0.1)
      ) +
      ggplot2::coord_cartesian(ylim = lift_axis$limits) +
      ggplot2::labs(
        x = "Training Share",
        y = y_label
      ) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    split_plot
  }

  train_plot <- make_split_plot("train_lift", "Training Lift", reference_train_lift)
  test_plot <- make_split_plot("test_lift", "Test Lift", reference_test_lift)

  if (!is.null(train_image_path)) {
    ggplot2::ggsave(
      train_image_path,
      plot = train_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  if (!is.null(test_image_path)) {
    ggplot2::ggsave(
      test_image_path,
      plot = test_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  list(
    results = split_results,
    train_plot = train_plot,
    test_plot = test_plot
  )
}

tree_alert_feature_sensitivity_analysis <- function(train_set,
                                                    test_set,
                                                    feature_formulas,
                                                    param_grid,
                                                    n_groups = 20,
                                                    actual_col = ".resid",
                                                    pred_col_name = "Pred_Feature_Sensitivity",
                                                    lift_metric = c("mean_abs", "rmse"),
                                                    reference_train_lift = NULL,
                                                    reference_test_lift = NULL,
                                                    image_path = NULL,
                                                    make_plot = !is.null(image_path),
                                                    width = 7,
                                                    height = 4.2,
                                                    dpi = 1000) {
  lift_metric <- match.arg(lift_metric)

  if (is.null(names(feature_formulas)) || any(names(feature_formulas) == "")) {
    names(feature_formulas) <- paste("Specification", seq_along(feature_formulas))
  }

  feature_results <- do.call(
    rbind,
    lapply(seq_along(feature_formulas), function(i) {
      feature_label <- names(feature_formulas)[i]
      feature_formula <- feature_formulas[[i]]

      grid_search <- tree_alert_grid_search(
        train_set = train_set,
        formula = feature_formula,
        param_grid = param_grid,
        n_groups = n_groups,
        pred_col_name = pred_col_name,
        lift_metric = lift_metric
      )

      feature_model <- tree_alert_train_best_model(
        train_set = train_set,
        formula = feature_formula,
        best_params = grid_search$best_params
      )

      feature_train <- train_set
      feature_train[[pred_col_name]] <- as.numeric(
        predict(feature_model, newdata = train_set, type = "vector")
      )

      feature_test <- test_set
      feature_test[[pred_col_name]] <- as.numeric(
        predict(feature_model, newdata = test_set, type = "vector")
      )

      data.frame(
        specification = feature_label,
        formula = paste(deparse(feature_formula), collapse = " "),
        cp = grid_search$best_params$cp,
        minbucket = grid_search$best_params$minbucket,
        maxdepth = grid_search$best_params$maxdepth,
        train_lift = tree_alert_compute_lift_numeric(
          df = feature_train,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        ),
        test_lift = tree_alert_compute_lift_numeric(
          df = feature_test,
          actual_col = actual_col,
          pred_col = pred_col_name,
          n_groups = n_groups,
          lift_metric = lift_metric
        )
      )
    })
  )

  feature_results$specification <- factor(
    feature_results$specification,
    levels = rev(names(feature_formulas))
  )

  make_plot <- isTRUE(make_plot) || !is.null(image_path)
  feature_plot <- NULL

  if (make_plot) {
    feature_plot_data <- feature_results |>
      tidyr::pivot_longer(
        cols = c(train_lift, test_lift),
        names_to = "set",
        values_to = "lift"
      ) |>
      dplyr::mutate(
        set = factor(
          set,
          levels = c("train_lift", "test_lift"),
          labels = c("Training", "Test")
        )
      )

    feature_plot <- ggplot2::ggplot(
      feature_plot_data,
      ggplot2::aes(x = lift, y = specification, color = set)
    ) +
      ggplot2::geom_vline(
        xintercept = 1,
        linetype = "dotted",
        color = "grey45",
        size = 0.4
      ) +
      ggplot2::geom_segment(
        data = feature_results,
        ggplot2::aes(
          x = train_lift,
          xend = test_lift,
          y = specification,
          yend = specification
        ),
        inherit.aes = FALSE,
        color = "grey70",
        size = 0.35
      ) +
      ggplot2::geom_path(ggplot2::aes(group = set), linewidth = 0.45) +
      ggplot2::geom_point(ggplot2::aes(shape = set), size = 2.4, show.legend = FALSE) +
      ggplot2::scale_color_manual(values = c("Training" = "grey20", "Test" = "#B2182B")) +
      ggplot2::scale_shape_manual(values = c("Training" = 16, "Test" = 17), guide = "none") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.45)) +
      ggplot2::labs(
        x = "First-Ventile Lift",
        y = "Feature Specification",
        color = NULL
      ) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.97, 0.97),
        legend.justification = c(1, 1),
        legend.background = ggplot2::element_rect(
          fill = ggplot2::alpha("white", 0.9),
          color = "grey80",
          linewidth = 0.25
        ),
        legend.key = ggplot2::element_rect(fill = ggplot2::alpha("white", 0)),
        axis.text.y = ggplot2::element_text(hjust = 1)
      )

    if (!is.null(image_path)) {
      ggplot2::ggsave(
        image_path,
        plot = feature_plot,
        width = width,
        height = height,
        dpi = dpi
      )
    }
  }

  list(
    results = feature_results,
    plot = feature_plot
  )
}

tree_alert_k_rule_sensitivity_analysis <- function(train_set,
                                                   test_set,
                                                   tree_rules,
                                                   model,
                                                   k_values = 1:10,
                                                   n_groups = 20,
                                                   actual_col = ".resid",
                                                   pred_col_name = "Pred_K_Rule_Sensitivity",
                                                   lift_metric = c("mean_abs", "rmse"),
                                                   image_path = NULL,
                                                   width = 7,
                                                   height = 4.2,
                                                   dpi = 1000) {
  lift_metric <- match.arg(lift_metric)

  k_values <- sort(unique(as.integer(k_values)))
  k_values <- k_values[!is.na(k_values) & k_values > 0]
  if (length(k_values) == 0) {
    stop("k_values must contain at least one positive integer.")
  }

  train_leaf_scores <- unique(as.numeric(predict(model, newdata = train_set, type = "vector")))
  train_leaf_scores <- train_leaf_scores[is.finite(train_leaf_scores)]
  sorted_rule_values <- train_leaf_scores[order(abs(train_leaf_scores), decreasing = TRUE)]

  max_available_rules <- length(sorted_rule_values)
  if (max_available_rules == 0) {
    stop("model must produce at least one finite terminal-node prediction.")
  }

  k_values <- k_values[k_values <= max_available_rules]
  if (length(k_values) == 0) {
    stop("No k_values are less than or equal to the number of available rules.")
  }

  score_rule_data <- function(data, selected_rule_values) {
    scored_data <- as.data.frame(data)
    leaf_scores <- as.numeric(predict(model, newdata = scored_data, type = "vector"))
    covered_by_rule <- leaf_scores %in% selected_rule_values

    scored_data[[pred_col_name]] <- ifelse(covered_by_rule, abs(leaf_scores), 0)
    scored_data
  }

  lift_summary <- if (lift_metric == "rmse") {
    function(error) sqrt(mean(error^2, na.rm = TRUE))
  } else {
    function(error) mean(abs(error), na.rm = TRUE)
  }

  compute_rule_set_lift <- function(scored_data) {
    flagged <- scored_data[[pred_col_name]] > 0
    if (!any(flagged, na.rm = TRUE)) {
      return(NA_real_)
    }

    lift_summary(scored_data[[actual_col]][flagged]) /
      lift_summary(scored_data[[actual_col]])
  }

  k_results <- do.call(
    rbind,
    lapply(k_values, function(k) {
      selected_rule_values <- sorted_rule_values[seq_len(k)]

      k_train <- score_rule_data(train_set, selected_rule_values)
      k_test <- score_rule_data(test_set, selected_rule_values)

      data.frame(
        k = k,
        train_flagged = sum(k_train[[pred_col_name]] > 0, na.rm = TRUE),
        test_flagged = sum(k_test[[pred_col_name]] > 0, na.rm = TRUE),
        train_lift = compute_rule_set_lift(k_train),
        test_lift = compute_rule_set_lift(k_test)
      )
    })
  )

  k_plot <- tree_alert_k_rule_sensitivity_plot(
    k_results = k_results,
    image_path = image_path,
    width = width,
    height = height,
    dpi = dpi
  )

  list(
    results = k_results,
    plot = k_plot
  )
}

tree_alert_k_rule_sensitivity_plot <- function(k_results,
                                               image_path = NULL,
                                               width = 7,
                                               height = 4.2,
                                               dpi = 1000) {
  required_cols <- c("k", "train_lift", "test_lift")
  missing_cols <- setdiff(required_cols, names(k_results))
  if (length(missing_cols) > 0) {
    stop(
      "k_results is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  k_values <- sort(unique(as.integer(k_results$k)))

  k_plot_data <- k_results |>
    tidyr::pivot_longer(
      cols = c(train_lift, test_lift),
      names_to = "set",
      values_to = "lift"
    ) |>
    dplyr::mutate(
      set = factor(
        set,
        levels = c("train_lift", "test_lift"),
        labels = c("Training", "Test")
      )
    )

  k_plot <- ggplot2::ggplot(
    k_plot_data,
    ggplot2::aes(x = k, y = lift, color = set, shape = set, group = set)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.3, fill = "white", stroke = 0.8) +
    ggplot2::scale_color_manual(values = c("Training" = "grey25", "Test" = "#B2182B")) +
    ggplot2::scale_shape_manual(values = c("Training" = 21, "Test" = 24)) +
    ggplot2::scale_x_continuous(breaks = k_values) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = 6),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    ggplot2::labs(
      x = "Number of Rules (k)",
      y = "Top-k Rule Lift",
      color = NULL,
      shape = NULL
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.8), color = "grey70"),
      legend.key = ggplot2::element_rect(fill = ggplot2::alpha("white", 0)),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )

  if (!is.null(image_path)) {
    ggplot2::ggsave(
      image_path,
      plot = k_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  k_plot
}

tree_alert_sort_rules_by_resid <- function(tree_rules,
                                           resid_col = ".resid",
                                           decreasing_abs = TRUE) {
  tree_rules[[resid_col]] <- as.numeric(tree_rules[[resid_col]])

  ord <- if (decreasing_abs) {
    order(abs(tree_rules[[resid_col]]), decreasing = TRUE)
  } else {
    order(tree_rules[[resid_col]], decreasing = TRUE)
  }

  tree_rules[ord, ]
}

tree_alert_select_rule_resids <- function(tree_rules,
                                          resid_col = ".resid",
                                          start_row = 1,
                                          end_row = nrow(tree_rules),
                                          decreasing_abs = TRUE) {
  sorted_rules <- tree_alert_sort_rules_by_resid(
    tree_rules = tree_rules,
    resid_col = resid_col,
    decreasing_abs = decreasing_abs
  )

  list(
    tree_rules = sorted_rules,
    selected_rules = sorted_rules[start_row:end_row, , drop = FALSE],
    rule_values = as.list(sorted_rules[[resid_col]][start_row:end_row])
  )
}

tree_alert_pull_top_k_rules <- function(tree_rules,
                                        n_rules,
                                        resid_col = ".resid",
                                        decreasing_abs = TRUE) {
  rule_selection <- tree_alert_select_rule_resids(
    tree_rules = tree_rules,
    resid_col = resid_col,
    start_row = 1,
    end_row = n_rules,
    decreasing_abs = decreasing_abs
  )

  list(
    tree_rules = rule_selection$tree_rules,
    selected_rules = rule_selection$selected_rules,
    selected_rule_values = rule_selection$rule_values
  )
}

tree_alert_format_rule_raw <- function(rule_row) {
  rule_values <- as.character(unlist(rule_row, use.names = FALSE))
  rule_names <- names(rule_row)
  keep_values <- !is.na(rule_values) & trimws(rule_values) != ""
  rule_values <- rule_values[keep_values]
  rule_names <- rule_names[keep_values]

  pieces <- mapply(
    function(rule_name, rule_value, index) {
      rule_value <- trimws(rule_value)
      rule_name <- trimws(rule_name)

      if (index == 1 && grepl("^[-+]?[0-9]", rule_value)) {
        return(rule_value)
      }

      if (rule_name == "" || tolower(rule_name) == tolower(rule_value)) {
        return(rule_value)
      }

      paste(rule_name, rule_value)
    },
    rule_names,
    rule_values,
    seq_along(rule_values),
    USE.NAMES = FALSE
  )

  paste(pieces, collapse = " ")
}

tree_alert_list_top_rules <- function(selected_rules,
                                      minute_increment = NULL,
                                      print_output = TRUE) {
  if (!exists("rephrase_rule", mode = "function")) {
    stop("rephrase_rule() is not available. Source Rephrase_Rule_Func.R first.")
  }

  raw_rules <- vapply(
    seq_len(nrow(selected_rules)),
    function(i) tree_alert_format_rule_raw(selected_rules[i, ]),
    character(1)
  )

  interpreted_rules <- vapply(
    raw_rules,
    rephrase_rule,
    character(1),
    minute_increment = minute_increment,
    USE.NAMES = FALSE
  )

  rule_output <- as.vector(rbind(
    paste("Rule", seq_along(raw_rules), "raw:", raw_rules),
    paste("Rule", seq_along(interpreted_rules), "interpretation:", interpreted_rules)
  ))

  if (print_output) {
    cat(paste(rule_output, collapse = "\n"), "\n", sep = "")
  }

  list(
    raw_rules = raw_rules,
    interpreted_rules = interpreted_rules,
    output = rule_output
  )
}

tree_alert_top_leaf_values <- function(data,
                                       pred_col_name = "Leaf_Node",
                                       actual_col = ".resid",
                                       n_rules = 3,
                                       train_end_time = NULL) {
  leaf_data <- as.data.frame(data)

  if (!is.null(train_end_time)) {
    leaf_data <- leaf_data[leaf_data$Time < train_end_time, ]
  }

  leaf_summary <- leaf_data |>
    dplyr::group_by(.data[[pred_col_name]]) |>
    dplyr::summarise(mean_resid = mean(.data[[actual_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(abs(mean_resid)))

  names(leaf_summary)[names(leaf_summary) == pred_col_name] <- "Leaf_Node"

  list(
    leaf_summary = leaf_summary,
    top_leaf_values = leaf_summary |>
      dplyr::slice(1:n_rules) |>
      dplyr::pull(Leaf_Node)
  )
}

tree_alert_select_positive_negative_rules <- function(tree_rules,
                                                      resid_col = ".resid",
                                                      n_positive = 3,
                                                      n_negative = 3) {
  tree_rules[[resid_col]] <- as.numeric(tree_rules[[resid_col]])

  rules_df <- tibble::tibble(
    row_id = seq_len(nrow(tree_rules)),
    resid = tree_rules[[resid_col]]
  )

  positive_rules <- rules_df |>
    dplyr::filter(resid > 0) |>
    dplyr::arrange(dplyr::desc(resid)) |>
    dplyr::slice(1:n_positive)

  negative_rules <- rules_df |>
    dplyr::filter(resid < 0) |>
    dplyr::arrange(resid) |>
    dplyr::slice(1:n_negative)

  selected_rules <- dplyr::bind_rows(positive_rules, negative_rules)

  list(
    selected_rules = selected_rules,
    rule_values = as.list(selected_rules$resid),
    rule_rows = selected_rules$row_id
  )
}

tree_alert_markdown_cell <- function(x, digits = 4) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return("")
  }

  if (is.numeric(x)) {
    value <- if (isTRUE(all.equal(x, round(x)))) {
      format(x, trim = TRUE, scientific = FALSE)
    } else {
      format(round(x, digits), trim = TRUE, scientific = FALSE)
    }
  } else {
    value <- as.character(x)
  }

  value <- gsub("\\|", "\\\\|", value)
  value <- gsub("\r?\n", "<br>", value)
  trimws(value)
}

tree_alert_markdown_table <- function(data, digits = 4, max_rows = NULL) {
  if (is.null(data) || length(data) == 0) {
    return("_No values recorded._")
  }

  data <- as.data.frame(data)
  if (!is.null(max_rows) && nrow(data) > max_rows) {
    data <- utils::head(data, max_rows)
  }

  if (nrow(data) == 0 || ncol(data) == 0) {
    return("_No values recorded._")
  }

  names(data) <- vapply(names(data), tree_alert_markdown_cell, character(1), digits = digits)
  body <- apply(
    data,
    1,
    function(row) paste0("| ", paste(vapply(row, tree_alert_markdown_cell, character(1), digits = digits), collapse = " | "), " |")
  )

  c(
    paste0("| ", paste(names(data), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(data)), collapse = " | "), " |"),
    body
  )
}

tree_alert_rules_table <- function(rule_text_output) {
  if (is.null(rule_text_output) || is.null(rule_text_output$raw_rules)) {
    return(NULL)
  }

  data.frame(
    rule = seq_along(rule_text_output$raw_rules),
    raw_rule = rule_text_output$raw_rules,
    interpretation = rule_text_output$interpreted_rules,
    stringsAsFactors = FALSE
  )
}

tree_alert_update_markdown_section <- function(file_path, section_id, section_lines) {
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  start_marker <- paste0("<!-- TREE_ALERT_RESULTS_START:", section_id, " -->")
  end_marker <- paste0("<!-- TREE_ALERT_RESULTS_END:", section_id, " -->")
  section_block <- c(start_marker, section_lines, end_marker)

  if (file.exists(file_path)) {
    existing_lines <- readLines(file_path, warn = FALSE)
  } else {
    existing_lines <- c(
      "# TreeAlert Run Results",
      "",
      "This file is generated by the TreeAlert R Markdown runs. Each section is replaced on subsequent runs of the corresponding analysis.",
      ""
    )
  }

  start_index <- which(existing_lines == start_marker)
  end_index <- which(existing_lines == end_marker)

  if (length(start_index) == 1 && length(end_index) == 1 && start_index < end_index) {
    before_section <- if (start_index > 1) existing_lines[seq_len(start_index - 1)] else character(0)
    after_section <- if (end_index < length(existing_lines)) existing_lines[(end_index + 1):length(existing_lines)] else character(0)
    updated_lines <- c(
      before_section,
      section_block,
      after_section
    )
  } else {
    updated_lines <- c(existing_lines, "", section_block)
  }

  writeLines(updated_lines, file_path)
  invisible(file_path)
}

tree_alert_update_results_summary <- function(output_path,
                                              section_id,
                                              title,
                                              analysis_choice = NULL,
                                              analysis_target = NULL,
                                              formula = NULL,
                                              n_groups = NULL,
                                              n_rules = NULL,
                                              minute_increment = NULL,
                                              train_set = NULL,
                                              test_set = NULL,
                                              best_params = NULL,
                                              train_lift = NULL,
                                              test_lift = NULL,
                                              full_lift = NULL,
                                              performance_summary = NULL,
                                              rule_text_output = NULL,
                                              rule_coverage = NULL,
                                              sensitivity_results = NULL,
                                              split_sensitivity_results = NULL,
                                              feature_sensitivity_results = NULL,
                                              k_rule_sensitivity_results = NULL,
                                              threshold_sensitivity_results = NULL,
                                              image_outputs = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  split_summary <- data.frame(
    set = c("Training", "Test"),
    periods = c(if (!is.null(train_set)) nrow(train_set) else NA_integer_,
                if (!is.null(test_set)) nrow(test_set) else NA_integer_),
    start = c(if (!is.null(train_set) && "Time" %in% names(train_set)) min(train_set$Time, na.rm = TRUE) else NA,
              if (!is.null(test_set) && "Time" %in% names(test_set)) min(test_set$Time, na.rm = TRUE) else NA),
    end = c(if (!is.null(train_set) && "Time" %in% names(train_set)) max(train_set$Time, na.rm = TRUE) else NA,
            if (!is.null(test_set) && "Time" %in% names(test_set)) max(test_set$Time, na.rm = TRUE) else NA)
  )

  lift_summary <- data.frame(
    metric = character(0),
    value = numeric(0)
  )
  if (!is.null(train_lift)) {
    lift_summary <- rbind(
      lift_summary,
      data.frame(metric = "Training first-ventile lift", value = train_lift)
    )
  }
  if (!is.null(test_lift)) {
    lift_summary <- rbind(
      lift_summary,
      data.frame(metric = "Test first-ventile lift", value = test_lift)
    )
  }
  if (!is.null(full_lift)) {
    lift_summary <- rbind(
      lift_summary,
      data.frame(metric = "Full-sample first-ventile lift", value = full_lift)
    )
  }
  lift_summary <- lift_summary[!is.na(lift_summary$value), , drop = FALSE]

  performance_table <- if (!is.null(performance_summary) && !is.null(performance_summary$summary)) {
    performance_summary$summary
  } else {
    NULL
  }

  rules_table <- tree_alert_rules_table(rule_text_output)

  if (!is.null(image_outputs) && "path" %in% names(image_outputs)) {
    image_outputs$path <- vapply(
      image_outputs$path,
      normalizePath,
      character(1),
      winslash = "/",
      mustWork = FALSE
    )
  }

  section_lines <- c(
    paste0("## ", title),
    "",
    paste0("- Last updated: ", timestamp),
    paste0("- Analysis choice: ", ifelse(is.null(analysis_choice), "not applicable", analysis_choice)),
    paste0("- Analysis target: ", ifelse(is.null(analysis_target), "not recorded", analysis_target)),
    paste0("- Formula: `", ifelse(is.null(formula), "not recorded", paste(deparse(formula), collapse = " ")), "`"),
    paste0("- Number of lift groups: ", ifelse(is.null(n_groups), "not recorded", n_groups)),
    paste0("- Number of retained rules: ", ifelse(is.null(n_rules), "not recorded", n_rules)),
    paste0("- Minute increment: ", ifelse(is.null(minute_increment), "not recorded", minute_increment)),
    "",
    "### Data Split",
    tree_alert_markdown_table(split_summary),
    "",
    "### Best Tree Parameters",
    tree_alert_markdown_table(best_params),
    "",
    "### Lift Values",
    tree_alert_markdown_table(lift_summary),
    "",
    "### Paper Performance Table",
    tree_alert_markdown_table(performance_table),
    "",
    "### Selected Rules: Raw and Interpreted",
    tree_alert_markdown_table(rules_table),
    "",
    "### Rule Coverage",
    tree_alert_markdown_table(rule_coverage),
    "",
    "### Hyperparameter Sensitivity Results",
    tree_alert_markdown_table(sensitivity_results),
    "",
    "### Train/Test Split Sensitivity Results",
    tree_alert_markdown_table(split_sensitivity_results),
    "",
    "### Feature Specification Sensitivity Results",
    tree_alert_markdown_table(feature_sensitivity_results),
    "",
    "### K-Rule Sensitivity Results",
    tree_alert_markdown_table(k_rule_sensitivity_results),
    "",
    "### Quantile Threshold Sensitivity Results",
    tree_alert_markdown_table(threshold_sensitivity_results),
    "",
    "### Image Outputs",
    tree_alert_markdown_table(image_outputs)
  )

  tree_alert_update_markdown_section(
    file_path = output_path,
    section_id = section_id,
    section_lines = section_lines
  )
}

tree_alert_kfold_cross_validation <- function(train_set,
                                              test_set,
                                              formula,
                                              param_grid,
                                              n_folds = 20,
                                              n_groups = 20,
                                              actual_col = ".resid",
                                              pred_col_name = "Pred_CV",
                                              lift_metric = c("mean_abs", "rmse"),
                                              fold_seed = 3001L) {
  lift_metric <- match.arg(lift_metric)

  if (!is.data.frame(train_set) || !is.data.frame(test_set)) {
    stop("train_set and test_set must both be data frames.")
  }
  if (!is.numeric(n_folds) || length(n_folds) != 1 ||
      is.na(n_folds) || n_folds < 2 || n_folds != floor(n_folds)) {
    stop("n_folds must be a single integer greater than or equal to 2.")
  }
  if (!actual_col %in% names(train_set) || !actual_col %in% names(test_set)) {
    stop("actual_col must be present in both train_set and test_set.")
  }
  if (nrow(train_set) < n_folds) {
    stop("train_set must contain at least n_folds observations.")
  }

  training_data <- as.data.frame(train_set)
  external_test <- as.data.frame(test_set)

  set.seed(as.integer(fold_seed))
  fold_id <- sample(rep(seq_len(as.integer(n_folds)), length.out = nrow(training_data)))

  fold_results <- lapply(seq_len(as.integer(n_folds)), function(fold) {
    fit_data <- training_data[fold_id != fold, , drop = FALSE]
    held_out <- training_data[fold_id == fold, , drop = FALSE]

    grid_search <- tree_alert_grid_search(
      train_set = fit_data,
      formula = formula,
      param_grid = param_grid,
      n_groups = n_groups,
      pred_col_name = pred_col_name,
      actual_col = actual_col,
      lift_metric = lift_metric
    )

    model <- tree_alert_train_best_model(
      train_set = fit_data,
      formula = formula,
      best_params = grid_search$best_params
    )

    fit_data[[pred_col_name]] <- as.numeric(
      predict(model, newdata = fit_data, type = "vector")
    )
    external_test_fold <- external_test
    external_test_fold[[pred_col_name]] <- as.numeric(
      predict(model, newdata = external_test_fold, type = "vector")
    )

    data.frame(
      fold = fold,
      fold_seed = fold_seed,
      fit_periods = nrow(fit_data),
      held_out_periods = nrow(held_out),
      test_periods = nrow(external_test_fold),
      cp = grid_search$best_params$cp,
      minbucket = grid_search$best_params$minbucket,
      maxdepth = grid_search$best_params$maxdepth,
      train_lift = tree_alert_compute_lift_numeric(
        df = fit_data,
        actual_col = actual_col,
        pred_col = pred_col_name,
        n_groups = n_groups,
        lift_metric = lift_metric
      ),
      test_lift = tree_alert_compute_lift_numeric(
        df = external_test_fold,
        actual_col = actual_col,
        pred_col = pred_col_name,
        n_groups = n_groups,
        lift_metric = lift_metric
      )
    )
  })

  list(
    results = do.call(rbind, fold_results),
    fold_id = fold_id,
    fold_seed = fold_seed,
    n_folds = n_folds,
    n_groups = n_groups,
    lift_metric = lift_metric
  )
}

tree_alert_make_lift_boxplot <- function(cv_results,
                                         image_path = NULL,
                                         width = 10,
                                         height = 3.2,
                                         dpi = 1000) {
  required_cols <- c("train_lift", "test_lift")
  missing_cols <- setdiff(required_cols, names(cv_results))
  if (length(missing_cols) > 0) {
    stop(
      "cv_results is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  plot_data <- tidyr::pivot_longer(
    cv_results,
    cols = dplyr::all_of(required_cols),
    names_to = "evaluation",
    values_to = "lift"
  )
  plot_data$evaluation <- factor(
    plot_data$evaluation,
    levels = c("train_lift", "test_lift"),
    labels = c("Training", "Fixed test")
  )

  lower_whiskers <- plot_data |>
    dplyr::group_by(evaluation) |>
    dplyr::summarise(
      lower = boxplot.stats(lift)$stats[1],
      first_quartile = quantile(lift, 0.25, na.rm = TRUE),
      .groups = "drop"
    )

  max_lift <- max(plot_data$lift, na.rm = TRUE)
  upper_limit <- max(3, ceiling(max_lift * 5) / 5)
  major_breaks <- seq(1, upper_limit, by = 0.2)
  minor_breaks <- seq(1, upper_limit, by = 0.1)

  lift_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = lift, y = evaluation, fill = evaluation)
  ) +
    ggplot2::geom_boxplot(
      width = 0.55,
      coef = 0,
      outlier.shape = NA
    ) +
    ggplot2::geom_errorbar(
      data = lower_whiskers,
      ggplot2::aes(
        xmin = lower,
        xmax = first_quartile,
        y = evaluation
      ),
      orientation = "y",
      width = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_jitter(width = 0.08, height = 0.08, alpha = 0.65) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "red"
    ) +
    ggplot2::scale_x_continuous(
      breaks = major_breaks,
      minor_breaks = minor_breaks,
      labels = function(x) sprintf("%.1f", x)
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 11, face = "plain"),
      axis.text.x = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 11),
      panel.grid.minor.x = ggplot2::element_line(colour = "grey90")
    ) +
    ggplot2::labs(x = "First-Ventile Lift", y = NULL)

  if (!is.null(image_path)) {
    ggplot2::ggsave(
      filename = image_path,
      plot = lift_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  list(
    plot = lift_plot,
    data = plot_data,
    lower_whiskers = lower_whiskers
  )
}
