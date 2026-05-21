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

# Helper: creates a fake forecast with the same rain intensity for every slot
make_forecast <- function(mm_h_value) {
  function(lat, lon) {
    times <- seq(
      as.POSIXct("2026-05-17 00:00", tz = "Europe/Amsterdam"),
      by = "15 min", length.out = 96
    )
    data.frame(time = times, mm_h = mm_h_value)
  }
}

# Test that each mm/h value gets the right rain label
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

# Test that exceeds_threshold correctly identifies rain that is bad enough to act on
test_that("exceeds_threshold returns TRUE when level meets or exceeds threshold", {
  expect_true(exceeds_threshold("none",     "none"))
  expect_true(exceeds_threshold("light",    "light"))
  expect_true(exceeds_threshold("moderate", "light"))
  expect_true(exceeds_threshold("heavy",    "moderate"))
  expect_true(exceeds_threshold("heavy",    "heavy"))
})

# Test that exceeds_threshold correctly identifies rain that is still acceptable
test_that("exceeds_threshold returns FALSE when level is below threshold", {
  expect_false(exceeds_threshold("none",  "light"))
  expect_false(exceeds_threshold("light", "moderate"))
  expect_false(exceeds_threshold("none",  "heavy"))
})

# Fetch a real forecast once; skip the API tests below if the connection fails
forecast_ams <- tryCatch(fetch_rain_forecast(52.3731, 4.8922),
                         error = function(e) NULL)

# Test that the forecast comes back in the right shape
test_that("fetch_rain_forecast returns a data frame with correct columns", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_s3_class(forecast_ams, "data.frame")
  expect_named(forecast_ams, c("time", "mm_h"))
})

# Test that the time column is a proper datetime object
test_that("fetch_rain_forecast time column is POSIXct", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_s3_class(forecast_ams$time, "POSIXct")
})

# Test that 96 rows are returned (one per 15-minute slot over 24 hours)
test_that("fetch_rain_forecast returns 96 rows (24 hours at 15-min resolution)", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_equal(nrow(forecast_ams), 96)
})

# Test that rain values make physical sense (cannot be negative)
test_that("fetch_rain_forecast mm_h values are non-negative", {
  skip_if(is.null(forecast_ams), "Open-Meteo API unavailable")
  expect_true(all(forecast_ams$mm_h >= 0))
})

# Test that the result contains all the expected fields
test_that("raintracker returns a named list with the correct elements", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_type(result, "list")
  expect_true(all(c("safe_to_go", "suggested_departure",
                    "end_of_route_note", "route_rain_summary") %in% names(result)))
})

# Test a completely dry route — should be safe to go right away
test_that("raintracker returns safe_to_go TRUE on a fully dry route", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_true(result$safe_to_go)
  expect_s3_class(result$suggested_departure, "POSIXct")
  expect_null(result$end_of_route_note)
  expect_null(result$route_rain_summary)
})

# Test that a rain summary is included when there is some light rain along the route
test_that("raintracker includes route_rain_summary when rain is below threshold", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(0.5),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time, max_rain_level = "moderate")
  expect_true(result$safe_to_go)
  expect_s3_class(result$route_rain_summary, "data.frame")
  expect_named(result$route_rain_summary,
               c("time_min", "lon", "lat", "rain_mm_h", "rain_level"))
})

# Test that a rainy morning pushes the suggested departure to after the rain clears
test_that("raintracker shifts departure when rain exceeds threshold mid-route", {
  rainy_until_noon <- function(lat, lon) {
    times  <- seq(as.POSIXct("2026-05-17 00:00", tz = "Europe/Amsterdam"),
                  by = "15 min", length.out = 96)
    cutoff <- as.POSIXct("2026-05-17 12:00", tz = "Europe/Amsterdam")
    data.frame(time = times, mm_h = ifelse(times < cutoff, 5, 0))
  }
  local_mocked_bindings(
    fetch_rain_forecast = rainy_until_noon,
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_true(result$safe_to_go)
  expect_gt(result$suggested_departure,
            as.POSIXct(start_time, tz = "Europe/Amsterdam"))
})

# Test that all-day rain returns no usable departure time
test_that("raintracker returns NA departure when no dry window exists", {
  local_mocked_bindings(
    fetch_rain_forecast = make_forecast(5),
    .package = "BikeWise"
  )
  result <- raintracker(timed_df, start_time)
  expect_false(result$safe_to_go)
  expect_true(is.na(result$suggested_departure))
})

# Test that rain only at the very end of the route gives a warning, not a delay
test_that("end buffer rain produces a note but does not shift departure", {
  call_n <- 0L
  last_only_rain <- function(lat, lon) {
    call_n <<- call_n + 1L
    times <- seq(as.POSIXct("2026-05-17 00:00", tz = "Europe/Amsterdam"),
                 by = "15 min", length.out = 96)
    # Only the last checkpoint (the finish) gets rain
    data.frame(time = times, mm_h = if (call_n == 4L) 5 else 0)
  }
  local_mocked_bindings(
    fetch_rain_forecast = last_only_rain,
    .package = "BikeWise"
  )
  # With end_buffer_min = 5, the last two checkpoints (at 10 and 15 min) are near the end
  result <- raintracker(timed_df, start_time, end_buffer_min = 5)
  expect_true(result$safe_to_go)
  expect_false(is.null(result$end_of_route_note))
  expect_equal(result$suggested_departure,
               as.POSIXct(start_time, tz = "Europe/Amsterdam"))
})
