# TreeAlert shared parameters and helper functions.

# Prep Parameters
color_string <- c(
  "#9400D3", "#008000", "#0000FF", "#FF8C00", "#000066",
  "#FF00FF", "#AB0000", "#FF69B4", "#7B68EE", "#Ff0000",
  "#7B68EE", "#20B2AA", "#703366", "#32CD32", "#CC6633",
  "#C77CFF", "#8A2BE2", "#00FF7F", "#00BFFF", "#FFA500",
  "#66CDAA", "#BA55D3", "#00CED1", "#4682B4", "#00FFFF"
)

tree_alert_lift_data <- function(df, actual_col, pred_col, n_groups = 20) {
  actual <- abs(df[[actual_col]])
  score <- abs(df[[pred_col]])

  ord <- order(score, decreasing = TRUE)
  actual_ord <- actual[ord]
  n <- length(actual_ord)

  group <- ceiling(seq_len(n) / (n / n_groups))
  group[group > n_groups] <- n_groups

  df_lift <- data.frame(group = group, error = actual_ord) |>
    dplyr::group_by(group) |>
    dplyr::summarise(meanError = mean(error, na.rm = TRUE), .groups = "drop")

  df_lift$percentile <- df_lift$group * (100 / n_groups)
  df_lift$meanError <- df_lift$meanError / mean(actual_ord, na.rm = TRUE)

  df_lift
}

tree_alert_compute_lift_numeric <- function(df, actual_col, pred_col, n_groups = 20) {
  df_lift <- tree_alert_lift_data(
    df = df,
    actual_col = actual_col,
    pred_col = pred_col,
    n_groups = n_groups
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
                                       axis_text_y_size = 20) {
  df_lift <- tree_alert_lift_data(
    df = df,
    actual_col = actual_col,
    pred_col = pred_col,
    n_groups = n_groups
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
                                    axis_text_y_size = 20) {
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
      axis_text_y_size = axis_text_y_size
    )
  } else {
    tree_alert_compute_lift_numeric(
      df = data_with_pred,
      actual_col = actual_col,
      pred_col = pred_col_name,
      n_groups = n_groups
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
                                   cp_digits = NULL) {
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
      n_groups = n_groups
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
    rule_values = as.list(sorted_rules[[resid_col]][start_row:end_row])
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
