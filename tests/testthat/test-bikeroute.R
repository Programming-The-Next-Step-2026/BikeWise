# Fetch routes once to avoid repeated API calls across all tests.
# If OSRM is unreachable, both will be NULL and tests will be skipped.
route  <- tryCatch(bikeroute(52.3731, 4.8922, 52.3579, 4.8686),
                   error = function(e) NULL)
route5 <- tryCatch(
  bikeroute(52.3731, 4.8922, 52.3579, 4.8686, interval_min = 5),
  error = function(e) NULL
)

# Test return structure
test_that("bikeroute returns a named list with correct elements", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_type(route, "list")
  expect_named(route,
               c("coordinates", "timed_coords", "duration_min", "distance_km"))
})

# Test coordinates data frame
test_that("coordinates is a data frame with lon and lat columns", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_s3_class(route$coordinates, "data.frame")
  expect_named(route$coordinates, c("lon", "lat"))
  expect_gt(nrow(route$coordinates), 1)
})

# Test coordinates fall within valid WGS84 bounds
test_that("coordinates fall within valid WGS84 bounds", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_true(all(route$coordinates$lat >= -90  & route$coordinates$lat <= 90))
  expect_true(all(route$coordinates$lon >= -180 & route$coordinates$lon <= 180))
})

# Test duration and distance are positive
test_that("duration and distance are positive numbers", {
  skip_if(is.null(route), "OSRM API unavailable")
  expect_gt(route$duration_min, 0)
  expect_gt(route$distance_km, 0)
})

# Test Haversine distance aligns with OSRM distance
test_that("Haversine total distance is within 1% of OSRM distance", {
  skip_if(is.null(route), "OSRM API unavailable")
  coords       <- route$coordinates
  n            <- nrow(coords)
  lat_rad      <- coords$lat * pi / 180
  lon_rad      <- coords$lon * pi / 180
  phi1         <- lat_rad[-n]
  phi2         <- lat_rad[-1]
  dphi         <- phi2 - phi1
  dlam         <- lon_rad[-1] - lon_rad[-n]
  a            <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  haversine_km <- sum(c(0, 2 * 6371 * asin(sqrt(a))))
  expect_lt(abs(haversine_km - route$distance_km) / route$distance_km, 0.01)
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

# Test default interval spacing
test_that("timed_coords timestamps are evenly spaced at the default interval", {
  skip_if(is.null(route), "OSRM API unavailable")
  times <- route$timed_coords$time_min
  # exclude the final gap, which may be shorter if duration is not a multiple
  diffs <- diff(times[-length(times)])
  expect_true(all(abs(diffs - 3) < 0.001))
})

# Test custom interval spacing
test_that("custom interval_min changes the spacing of timed_coords", {
  skip_if(is.null(route5), "OSRM API unavailable")
  times <- route5$timed_coords$time_min
  diffs <- diff(times[-length(times)])
  expect_true(all(abs(diffs - 5) < 0.001))
})

# Test error for unroutable coordinates
test_that("coordinates with no bikeable route produce an error", {
  skip_on_cran()
  # 0,0 is in the ocean with no road network
  expect_error(bikeroute(0, 0, 0.001, 0.001))
})
