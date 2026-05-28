# Tests for bikeroute().
# Fetches a real route once to avoid repeated API calls across all tests.
# All tests are skipped if OSRM is unreachable.

# Fetch route once
route <- tryCatch(bikeroute(52.3731, 4.8922, 52.3579, 4.8686),
                  error = function(e) NULL)

# Test return structure
test_that("bikeroute returns a named list with correct elements", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_type(route, "list")
  expect_named(route, c("timed_coords", "duration_min", "distance_km"))
})

# Test duration and distance are positive
test_that("duration and distance are positive numbers", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_gt(route$duration_min, 0)
  expect_gt(route$distance_km, 0)
})

# Test timed_coords structure and column names
test_that("timed_coords has correct columns", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_s3_class(route$timed_coords, "data.frame")
  expect_named(route$timed_coords, c("time_min", "dist_km", "lon", "lat"))
})

# Test timed_coords starts at 0 and ends at duration_min
test_that("timed_coords always starts at 0 and ends at duration_min", {
  skip_if(is.null(route), "OSRM API unavailable")
  times <- route$timed_coords$time_min
  expect_equal(times[1], 0)
  expect_equal(times[length(times)], route$duration_min)
})

# Test that the internal interval is 2 minutes
# (hardcoded for raintracker resolution)
test_that("timed_coords timestamps are spaced at the 2-minute interval", {
  skip_if(is.null(route), "OSRM API unavailable")
  times <- route$timed_coords$time_min
  # exclude the final gap — may be shorter if duration is not a multiple of 2
  diffs <- diff(times[-length(times)])
  expect_true(all(abs(diffs - 2) < 0.001))
})

# Test error for unroutable coordinates
test_that("coordinates with no bikeable route produce an error", {
  skip_on_cran()
  # 0,0 is in the ocean with no road network
  expect_error(bikeroute(0, 0, 0.001, 0.001))
})
