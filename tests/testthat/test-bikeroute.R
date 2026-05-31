# Tests for bikeroute().
# Fetches a real route once to avoid repeated API calls across all tests.
# All tests are skipped if OSRM is unreachable.

route <- tryCatch(bikeroute(52.3731, 4.8922, 52.3579, 4.8686),
                  error = function(e) NULL)

test_that("bikeroute returns a named list with correct elements", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_type(route, "list")
  expect_named(route, c("timed_coords", "duration_min", "distance_km"))
})

test_that("duration and distance are positive numbers", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_gt(route$duration_min, 0)
  expect_gt(route$distance_km, 0)
})

test_that("timed_coords has correct columns", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_s3_class(route$timed_coords, "data.frame")
  expect_named(route$timed_coords, c("time_min", "dist_km", "lon", "lat"))
})

test_that("timed_coords starts at 0 and ends at full distance and duration", {
  skip_if(is.null(route), "OSRM API unavailable")
  dists <- route$timed_coords$dist_km
  times <- route$timed_coords$time_min
  expect_equal(dists[1], 0)
  expect_equal(times[1], 0)
  expect_equal(dists[length(dists)], route$distance_km, tolerance = 0.01)
  expect_equal(times[length(times)], route$duration_min)
})

test_that("timed_coords distance marks are spaced at 1 km intervals", {
  skip_if(is.null(route), "OSRM API unavailable")
  dists <- route$timed_coords$dist_km
  # exclude the final gap — last mark is the exact route end, not a km boundary
  diffs <- diff(dists[-length(dists)])
  expect_true(all(abs(diffs - 1) < 0.001))
})

test_that("duration_min is consistent with distance_km at default speed", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_equal(route$duration_min, round(route$distance_km / 15 * 60, 1))
})

test_that("coordinates with no bikeable route produce an error", {
  skip_on_cran()
  # 0,0 is in the ocean with no road network
  expect_error(bikeroute(0, 0, 0.001, 0.001))
})
