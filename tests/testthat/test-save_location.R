# Tests for save_location() and its internal geocode() helper.
# geocode() is mocked to avoid real Nominatim calls; sheet functions are also
# mocked so no Google Sheets connection is needed.

# A fake locations sheet with one existing row for alice/home
fake_locations <- data.frame(
  user         = "alice",
  label        = "home",
  address      = "Old Street 1, Amsterdam",
  lat          = 52.30,
  lon          = 4.80,
  display_name = "Home",
  stringsAsFactors = FALSE
)

# Test that save_location returns the geocoded coordinates
test_that("save_location returns a named list with lat and lon", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(...) invisible(NULL),
    .package    = "BikeWise"
  )
  result <- save_location("alice", "work", "Dam Square, Amsterdam")
  expect_type(result, "list")
  expect_named(result, c("lat", "lon"))
  expect_equal(result$lat, 52.37)
  expect_equal(result$lon, 4.89)
})

# Test that preset labels get their display_name from PRESET_TITLES automatically
test_that("save_location sets display_name from PRESET_TITLES for known labels", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package    = "BikeWise"
  )
  save_location("alice", "work", "Dam Square, Amsterdam")
  new_row <- written[written$label == "work", ]
  expect_equal(new_row$display_name, "Work")
})

# Test that a provided display_name overrides the preset (used for custom labels)
test_that("save_location uses the provided display_name when given", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package    = "BikeWise"
  )
  save_location("alice", "custom1", "Science Park 904", display_name = "My Lab")
  new_row <- written[written$label == "custom1", ]
  expect_equal(new_row$display_name, "My Lab")
})

# Test that saving an existing label replaces the old row rather than adding a duplicate
test_that("save_location overwrites an existing entry for the same user and label", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package    = "BikeWise"
  )
  save_location("alice", "home", "New Home Street 5, Amsterdam")
  # exactly one row for alice/home after the upsert
  expect_equal(sum(written$user == "alice" & written$label == "home"), 1)
})

# Test that the coordinates written to the sheet match what geocode returned
test_that("save_location writes the geocoded coordinates to the sheet", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 51.50, lon = -0.12),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package    = "BikeWise"
  )
  save_location("alice", "work", "London Bridge, London")
  new_row <- written[written$label == "work", ]
  expect_equal(new_row$lat, 51.50)
  expect_equal(new_row$lon, -0.12)
})
