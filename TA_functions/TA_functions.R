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

  group <- ceiling(seq_len(n) / (n / n_groups))
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

tree_alert_sensitivity_analysis <- function(train_set,
                                            test_set,
                                            formula,
                                            param_grid,
                                            n_groups = 20,
                                            actual_col = ".resid",
                                            pred_col_name = "Pred_Sensitivity",
                                            lift_metric = c("mean_abs", "rmse"),
                                            reference_train_lift = NULL,
                                            reference_test_lift = NULL,
                                            train_image_path = NULL,
                                            test_image_path = NULL,
                                            width = 8,
                                            height = 4.2,
                                            dpi = 1000) {
  lift_metric <- match.arg(lift_metric)

  sensitivity_results <- do.call(
    rbind,
    lapply(seq_len(nrow(param_grid)), function(i) {
      params <- param_grid[i, , drop = FALSE]

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

  parameter_value_levels <- c(
    paste("cp", sort(unique(sensitivity_results$cp)), sep = "__"),
    paste("minbucket", sort(unique(sensitivity_results$minbucket)), sep = "__"),
    paste("maxdepth", sort(unique(sensitivity_results$maxdepth)), sep = "__")
  )

  sensitivity_plot_data <- sensitivity_results |>
    tidyr::pivot_longer(
      cols = c(cp, minbucket, maxdepth),
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      value = factor(
        paste(parameter, value, sep = "__"),
        levels = parameter_value_levels
      ),
      parameter = factor(
        parameter,
        levels = c("cp", "minbucket", "maxdepth"),
        labels = c("Complexity parameter", "Minimum bucket", "Maximum depth")
      )
    )

  make_sensitivity_plot <- function(y_col, y_label, reference_lift = NULL) {
    sensitivity_plot <- ggplot2::ggplot(
      sensitivity_plot_data,
      ggplot2::aes(x = value, y = .data[[y_col]])
    ) +
      ggplot2::geom_boxplot(
        width = 0.65,
        fill = "grey92",
        color = "grey20",
        outlier.shape = 21,
        outlier.fill = "white",
        outlier.color = "grey20",
        outlier.size = 1.8
      ) +
      ggplot2::facet_wrap(~ parameter, scales = "free_x", nrow = 1) +
      ggplot2::scale_x_discrete(labels = function(x) sub("^.*__", "", x)) +
      ggplot2::labs(
        x = NULL,
        y = y_label
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "grey85", color = "grey30"),
        strip.text = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    if (!is.null(reference_lift)) {
      sensitivity_plot <- sensitivity_plot +
        ggplot2::geom_hline(
          yintercept = reference_lift,
          linetype = "dashed",
          color = "#B2182B",
          size = 0.45
        )
    }

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
