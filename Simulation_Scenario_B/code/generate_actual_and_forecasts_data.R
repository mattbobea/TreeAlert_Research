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

simulation_start <- as.Date("2015-01-01")
simulation_end <- as.Date("2024-12-31")
training_end <- as.Date("2021-12-31")
test_start <- as.Date("2022-01-01")
representative_seed <- 1001

lunar_new_year_dates <- tibble(
  year = 2015:2024,
  lunar_new_year = as.Date(c(
    "2015-02-19", "2016-02-08", "2017-01-28", "2018-02-16", "2019-02-05",
    "2020-01-25", "2021-02-12", "2022-02-01", "2023-01-22", "2024-02-10"
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
  mutate(
    .resid = Value - .fitted,
    Day = day(Time),
    WeekofMonth = ceiling(day(Time) / 7),
    WeekofYear = isoweek(Time),
    Month = month(Time)
  )

write_csv(
  series_data,
  file.path(data_dir, "scenario_b_actual_forecast_series.csv")
)
