library(dplyr)
library(readr)
library(lubridate)
library(forecast)

simulation_dir <- if (dir.exists("Simulation_Scenario_B")) {
  "Simulation_Scenario_B"
} else if (dir.exists("data")) {
  "."
} else {
  ".."
}
data_dir <- file.path(simulation_dir, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

simulation_start <- as.Date("2010-01-01")
simulation_end <- as.Date("2026-12-31")
training_end <- as.Date("2023-12-31")
test_start <- as.Date("2024-01-01")
representative_seed <- 1001
arima_seed <- 20260817

lunar_new_year_dates <- tibble(
  year = 2010:2026,
  lunar_new_year = as.Date(c(
    "2010-02-14", "2011-02-03", "2012-01-23", "2013-02-10", "2014-01-31",
    "2015-02-19", "2016-02-08", "2017-01-28", "2018-02-16", "2019-02-05",
    "2020-01-25", "2021-02-12", "2022-02-01", "2023-01-22", "2024-02-10",
    "2025-01-29", "2026-02-17"
  ))
)

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

generate_actual_and_forecasts_data <- function(seed = representative_seed,
                                               write_output = TRUE,
                                               write_metadata = write_output) {
  series_data <- generate_lunar_calendar_demand(
    seed, lunar_new_year_dates$lunar_new_year
  )
  train_data <- filter(series_data, Time <= training_end)
  test_data <- filter(series_data, Time >= test_start)

  # auto.arima is deterministic for fixed data, but we set and record a seed
  # so the complete analysis remains reproducible if implementation details change.
  set.seed(arima_seed)
  arima_model <- forecast::auto.arima(
    ts(train_data$Value, frequency = 7),
    seasonal = TRUE,
    stepwise = TRUE,
    approximation = TRUE
  )

  train_data$.fitted <- as.numeric(fitted(arima_model))
  # Update the fitted ARIMA state with observed test history to obtain
  # fixed-parameter one-step-ahead forecasts throughout the test period.
  updated_model <- forecast::Arima(
    ts(c(train_data$Value, test_data$Value), frequency = 7),
    model = arima_model
  )
  test_data$.fitted <- tail(as.numeric(fitted(updated_model)), nrow(test_data))

  # Aggregate the generated daily series to weekly observations for TreeAlert.
  # ARIMA is still estimated once on the generated daily training series above;
  # only the saved analysis series and tree inputs are weekly.
  series_data <- bind_rows(train_data, test_data) %>%
    mutate(week = floor_date(Time, unit = "week", week_start = 1)) %>%
    group_by(week) %>%
    summarise(
      Time = min(week),
      Value = mean(Value, na.rm = TRUE),
      .fitted = mean(.fitted, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Time) %>%
    mutate(
      .resid = Value - .fitted,
      WeekofMonth = ceiling(day(Time) / 7),
      WeekofYear = isoweek(Time),
      Month = month(Time)
    )

  if (write_output) {
    write_csv(
      series_data,
      file.path(data_dir, "scenario_b_actual_forecast_series.csv")
    )
  }

  if (write_metadata) {
    order <- forecast::arimaorder(arima_model)
    metadata <- tibble(
      data_seed = seed,
      arima_seed = arima_seed,
      model = paste0("ARIMA(", order[[1]], ",", order[[2]], ",", order[[3]], ")",
                     "(", order[[4]], ",", order[[5]], ",", order[[6]], ")[", order[[7]], "]"),
      arima_p = order[[1]], arima_d = order[[2]], arima_q = order[[3]],
      arima_P = order[[4]], arima_D = order[[5]], arima_Q = order[[6]],
      frequency = order[[7]],
      coefficients = paste(names(coef(arima_model)), signif(coef(arima_model), 10), collapse = "; "),
      seasonal = TRUE, stepwise = TRUE, approximation = TRUE
    )
    write_csv(metadata, file.path(data_dir, "scenario_b_arima_metadata.csv"))
  }

  series_data
}

if (sys.nframe() == 0L) {
  generate_actual_and_forecasts_data()
}
