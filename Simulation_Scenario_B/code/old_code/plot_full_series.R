library(readr)
library(dplyr)
library(ggplot2)
library(scales)

simulation_dir <- if (dir.exists("Simulation_Scenario_B")) "Simulation_Scenario_B" else ".."
data_dir <- file.path(simulation_dir, "data")
image_dir <- file.path(simulation_dir, "images")
dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)

series_data <- read_csv(
  file.path(data_dir, "scenario_b_actual_forecast_series.csv"),
  show_col_types = FALSE
) %>%
  mutate(Time = as.Date(Time))

split_date <- as.Date("2022-01-01")

base_theme <- theme_bw(base_size = 17) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey70"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90")
  )

actual_fitted_plot <- ggplot(series_data, aes(x = Time)) +
  geom_line(aes(y = Value, color = "Actual"), linewidth = 0.35) +
  geom_line(aes(y = .fitted, color = "ARIMA fitted / forecast"), linewidth = 0.35) +
  geom_vline(
    xintercept = split_date,
    linetype = "dashed",
    color = "#333333",
    linewidth = 0.7
  ) +
  annotate(
    "text", x = split_date, y = Inf, label = "Test period",
    vjust = 1.3, hjust = -0.05, size = 4.8, color = "#333333"
  ) +
  scale_color_manual(values = c(
    "Actual" = "#4C78A8",
    "ARIMA fitted / forecast" = "#E17C05"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Daily demand", color = NULL) +
  base_theme

error_plot <- ggplot(series_data, aes(x = Time, y = .resid)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.45) +
  geom_line(color = "#4C78A8", linewidth = 0.35) +
  geom_vline(
    xintercept = split_date,
    linetype = "dashed",
    color = "#333333",
    linewidth = 0.7
  ) +
  annotate(
    "text", x = split_date, y = Inf, label = "Test period",
    vjust = 1.3, hjust = -0.05, size = 4.8, color = "#333333"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "ARIMA forecast error") +
  base_theme +
  theme(legend.position = "none")

ggsave(
  file.path(image_dir, "scenario_b_full_actual_fitted.png"),
  actual_fitted_plot, width = 11, height = 5.5, dpi = 1000
)
ggsave(
  file.path(image_dir, "scenario_b_full_error.png"),
  error_plot, width = 11, height = 4.7, dpi = 1000
)
