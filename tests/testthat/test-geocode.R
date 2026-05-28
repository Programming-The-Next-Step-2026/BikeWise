# Tests for the geocode internal helper (lives in save_location.R).
# All tests require an internet connection and are skipped
# if Nominatim is unreachable.

# Geocode a well-known address once to avoid repeated API calls
result_dam <- tryCatch(BikeWise:::geocode("Dam Square, Amsterdam"),
                       error = function(e) NULL)

# Test that the result is a named list with lat and lon
test_that("geocode returns a named list with lat and lon", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_type(result_dam, "list")
  expect_named(result_dam, c("lat", "lon"))
})

# Test that both coordinates are numeric values
test_that("geocode returns numeric coordinates", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_type(result_dam$lat, "double")
  expect_type(result_dam$lon, "double")
})

# Test that coordinates fall within valid worldwide bounds
test_that("geocode returns coordinates within valid bounds", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_true(result_dam$lat >= -90  && result_dam$lat <= 90)
  expect_true(result_dam$lon >= -180 && result_dam$lon <= 180)
})

# Test that a well-known address lands in roughly the right place
test_that("geocode returns roughly correct coordinates for a known address", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_equal(result_dam$lat, 52.37, tolerance = 0.5)
  expect_equal(result_dam$lon, 4.89,  tolerance = 0.5)
})

# Test that a nonsense address throws a clear, informative error
test_that("geocode throws an informative error for an unrecognisable address", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_error(BikeWise:::geocode("xkzqwpfmvb12345"), "Address not found")
})
