# Tests for the raintracker helper functions and the main raintracker function.
# Instead of calling the real weather API, the API is swapped out for fake
# forecasts so the tests run offline and always return predictable data.

# A made-up route with 4 stops over 3 km and 15 minutes
timed_df <- data.frame(
  time_min = c(0,      5,      10,     15),
  dist_km  = c(0,      1,      2,      3),
  lon      = c(4.890,  4.900,  4.910,  4.920),
  lat      = c(52.370, 52.360, 52.350, 52.340)
)
start_time <- "2026-05-17 08:00"

# Helper — creates a fake forecast with constant rain intensity for every slot
make_forecast <- function(mm_h_value) {
  function(lat, lon) {
    times <- seq(
      as.POSIXct("2026-05-17 00:00", tz = Sys.timezone()),
      by = "15 min", length.out = 96
    )
    data.frame(time = times, mm_h = mm_h_value)
  }
}

test_that("classify_rain returns correct label at and between boundaries", {
  expect_equal(classify_rain(0),    "none")
  expect_equal(classify_rain(0.09), "none")
  expect_equal(classify_rain(0.1),  "light")
  expect_equal(classify_rain(1.0),  "light")
  expect_equal(classify_rain(2.49), "light")
  expect_equal(classify_rain(2.5),  "moderate")
  expect_equal(classify_rain(10),   "moderate")
  expect_equal(classify_rain(10.1), "heavy")
  expect_equal(classify_rain(50),   "heavy")
})

test_that("exceeds_threshold returns TRUE only when level strictly exceeds", {
  expect_true(exceeds_threshold("light",    "none"))
  expect_true(exceeds_threshold("moderate", "light"))
  expect_true(exceeds_threshold("heavy",    "moderate"))
  expect_true(exceeds_threshold("heavy",    "none"))
})

test_that("exceeds_threshold returns FALSE when level is at or below", {
  expect_false(exceeds_threshold("none",     "none"))
  expect_false(exceeds_threshold("light",    "light"))
  expect_false(exceeds_threshold("heavy",    "heavy"))
  expect_false(exceeds_threshold("none",     "light"))
  expect_false(exceeds_threshold("light",    "moderate"))
  expect_false(exceeds_threshold("none",     "heavy"))
})

# Fetch a real forecast once — skip API tests below if the connection fails
forecast_ams <- tryCatch(fetch_rain_forecast(52.3731, 4.8922),
                         error = function(e) NULL)

test_that("fetch_rain_forecast returns a data frame with correct columns", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_s3_class(forecast_ams, "data.frame")
  expect_named(forecast_ams, c("time", "mm_h"))
})

test_that("fetch_rain_forecast time column is POSIXct", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_s3_class(forecast_ams$time, "POSIXct")
})

test_that("fetch_rain_forecast returns 96 rows (24h at 15-min resolution)", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_equal(nrow(forecast_ams), 96)
})

test_that("fetch_rain_forecast mm_h values are non-negative", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_true(all(forecast_ams$mm_h >= 0))
})

test_that("raintracker returns a named list with the correct elements", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_type(result, "list")
  expect_true(all(c("safe_to_go", "suggested_departure",
                    "route_rain_summary") %in% names(result)))
})

test_that("raintracker returns safe_to_go TRUE on a fully dry route", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_true(result$safe_to_go)
  expect_s3_class(result$suggested_departure, "POSIXct")
  # route_rain_summary is always a data frame — all levels "none" when dry
  expect_s3_class(result$route_rain_summary, "data.frame")
  expect_true(all(result$route_rain_summary$rain_level == "none"))
})

test_that("raintracker includes route_rain_summary when rain is below", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0.5),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time, threshold = "moderate")
  expect_true(result$safe_to_go)
  expect_s3_class(result$route_rain_summary, "data.frame")
  expect_named(result$route_rain_summary,
               c("time_min", "dist_km", "lon", "lat", "rain_mm_h",
                 "rain_level"))
})

test_that("raintracker shifts departure when rain exceeds threshold", {
  rainy_until_noon <- function(lat, lon) {
    times  <- seq(as.POSIXct("2026-05-17 00:00", tz = Sys.timezone()),
                  by = "15 min", length.out = 96)
    cutoff <- as.POSIXct("2026-05-17 12:00", tz = Sys.timezone())
    data.frame(time = times, mm_h = ifelse(times < cutoff, 15, 0))
  }
  local_mocked_bindings(
    fetch_rain_forecast = rainy_until_noon,
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_true(result$safe_to_go)
  expect_gt(result$suggested_departure,
            as.POSIXct(start_time, tz = Sys.timezone()))
})

test_that("raintracker returns NA departure when no dry window exists", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(15),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_false(result$safe_to_go)
  expect_true(is.na(result$suggested_departure))
})

test_that("raintracker always returns route_rain_summary as a data frame", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(15),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_s3_class(result$route_rain_summary, "data.frame")
  expect_named(result$route_rain_summary,
               c("time_min", "dist_km", "lon", "lat",
                 "rain_mm_h", "rain_level"))
})

test_that("raintracker no-dry-window summary shows heavy rain at start_time", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(15),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_true(all(result$route_rain_summary$rain_level == "heavy"))
})

test_that("raintracker stops for an invalid threshold value", {
  expect_error(
    raintracker(timed_df, start_time, threshold = "extreme"),
    "threshold must be one of"
  )
})

test_that("raintracker is safe with threshold none on a dry route", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time, threshold = "none")
  expect_true(result$safe_to_go)
})

test_that("raintracker shifts departure with threshold none for light rain", {
  light_rain_until_noon <- function(lat, lon) {
    times  <- seq(as.POSIXct("2026-05-17 00:00", tz = Sys.timezone()),
                  by = "15 min", length.out = 96)
    cutoff <- as.POSIXct("2026-05-17 12:00", tz = Sys.timezone())
    data.frame(time = times, mm_h = ifelse(times < cutoff, 0.5, 0))
  }
  local_mocked_bindings(
    fetch_rain_forecast = light_rain_until_noon,
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time, threshold = "none")
  expect_true(result$safe_to_go)
  expect_gt(result$suggested_departure,
            as.POSIXct(start_time, tz = Sys.timezone()))
})

test_that("raintracker uses fewer checkpoints for a long route", {
  long_route <- data.frame(
    time_min = seq(0, 240, length.out = 100),
    dist_km  = seq(0, 60,  length.out = 100),
    lon      = rep(4.89, 100),
    lat      = rep(52.37, 100)
  )
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(long_route, start_time)
  # 60 km / 30 target = 2 km interval — ~31 checkpoints
  n <- nrow(result$route_rain_summary)
  expect_gte(n, 25)
  expect_lte(n, 35)
})

test_that("raintracker uses 1 km floor for routes shorter than 30 km", {
  short_route <- timed_df  # 3 km
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(short_route, start_time)
  # 3 km at 1 km intervals = 4 checkpoints (0, 1, 2, 3 + final row)
  expect_lte(nrow(result$route_rain_summary), 6)
})
