library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(forecast)
library(scales)

simulation_dir <- if (dir.exists("Simulation_Scenario_B")) "Simulation_Scenario_B" else ".."
data_dir <- file.path(simulation_dir, "data")
image_dir <- file.path(simulation_dir, "images")
dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)

simulation_start <- as.Date("2015-01-01")
simulation_end <- as.Date("2024-12-31")
training_end <- as.Date("2021-12-31")
test_start <- as.Date("2022-01-01")
representative_seed <- 1001

lunar_new_year_dates <- read_csv(
  file.path(data_dir, "lunar_new_year_dates.csv"),
  show_col_types = FALSE
) %>%
  mutate(lunar_new_year = as.Date(lunar_new_year))

nearest_lunar_new_year <- function(dates, lunar_dates) {
  date_differences <- outer(as.numeric(dates), as.numeric(lunar_dates), FUN = "-")
  nearest_index <- apply(abs(date_differences), 1, which.min)
  date_differences[cbind(seq_along(dates), nearest_index)]
}

lunar_new_year_effect <- function(days_from_lunar_new_year, event_shocks) {
  pre_holiday_peak <- ifelse(
    days_from_lunar_new_year >= -14 & days_from_lunar_new_year <= -1,
    34 * (1 + days_from_lunar_new_year / 18), 0
  )
  holiday_dip <- ifelse(
    days_from_lunar_new_year >= 0 & days_from_lunar_new_year <= 7,
    -42 * (1 - days_from_lunar_new_year / 16), 0
  )
  post_holiday_rebound <- ifelse(
    days_from_lunar_new_year >= 8 & days_from_lunar_new_year <= 16,
    16 * (1 - (days_from_lunar_new_year - 8) / 14), 0
  )

  (pre_holiday_peak + holiday_dip + post_holiday_rebound) * event_shocks
}

generate_lunar_calendar_demand <- function(seed, lunar_dates) {
  set.seed(seed)
  dates <- seq.Date(simulation_start, simulation_end, by = "day")
  days_from_lunar_new_year <- nearest_lunar_new_year(dates, lunar_dates)
  annual_pattern <- 4 * sin(2 * pi * yday(dates) / 365.25) +
    2 * cos(2 * pi * yday(dates) / 365.25)
  weekday_pattern <- c("1" = -4, "2" = -2, "3" = 0, "4" = 1,
                       "5" = 3, "6" = 7, "7" = 4)[as.character(wday(dates))]
  event_shocks <- rnorm(length(lunar_dates), mean = 1, sd = 0.08)
  nearest_event_index <- apply(
    abs(outer(as.numeric(dates), as.numeric(lunar_dates), FUN = "-")),
    1, which.min
  )
  innovations <- as.numeric(arima.sim(model = list(ar = 0.45), n = length(dates), sd = 5))

  tibble(
    Time = dates,
    Value = 160 + annual_pattern + weekday_pattern +
      lunar_new_year_effect(days_from_lunar_new_year, event_shocks[nearest_event_index]) +
      innovations
  )
}

series_data <- generate_lunar_calendar_demand(
  representative_seed, lunar_new_year_dates$lunar_new_year
)
train_data <- filter(series_data, Time <= training_end)
test_data <- filter(series_data, Time >= test_start)
arima_model <- forecast::auto.arima(
  ts(train_data$Value, frequency = 7),
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = TRUE
)

train_data$.fitted <- as.numeric(fitted(arima_model))
test_data$.fitted <- as.numeric(forecast(arima_model, h = nrow(test_data))$mean)
series_data <- bind_rows(train_data, test_data) %>%
  arrange(Time) %>%
  mutate(.resid = Value - .fitted)

write_csv(
  series_data,
  file.path(data_dir, "scenario_b_actual_forecast_series.csv")
)

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
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.8, color = "#333333") +
  scale_color_manual(values = c("Actual" = "#4C78A8", "ARIMA fitted / forecast" = "#E17C05")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Daily demand", color = NULL) +
  base_theme

error_plot <- ggplot(series_data, aes(x = Time, y = .resid)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.45) +
  geom_line(color = "#4C78A8", linewidth = 0.35) +
  geom_vline(xintercept = test_start, linetype = "dashed", color = "#333333", linewidth = 0.7) +
  annotate("text", x = test_start, y = Inf, label = "Test period",
           vjust = 1.3, hjust = -0.05, size = 4.8, color = "#333333") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "ARIMA forecast error") +
  base_theme +
  theme(legend.position = "none")

ggsave(file.path(image_dir, "scenario_b_full_actual_fitted.png"),
       actual_fitted_plot, width = 11, height = 5.5, dpi = 1000)
ggsave(file.path(image_dir, "scenario_b_full_error.png"),
       error_plot, width = 11, height = 4.7, dpi = 1000)
