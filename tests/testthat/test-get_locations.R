# Tests for get_locations().
# Google Sheets and sheet_id() are mocked so no real connection is needed.
# The local CSV backend is tested by mocking R_user_dir() to point at a
# temporary directory that is cleaned up after each test.

# A fake locations sheet with two rows for alice and one for bob
fake_locations <- data.frame(
  user         = c("alice", "alice", "bob"),
  label        = c("home",  "work",  "home"),
  address      = c("Addr A", "Addr B", "Addr C"),
  lat          = c(52.30,   52.40,   52.50),
  lon          = c(4.80,    4.90,    5.00),
  display_name = c("Home",  "Work",  "Home")
)

# Test that calling without a label returns all of that user's rows as a data frame
test_that("get_locations returns a data frame of all locations for a user", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_locations,
    .package   = "BikeWise"
  )
  result <- get_locations("alice")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$user == "alice"))
})

# Test that providing a label returns just the coordinates for that location
test_that("get_locations returns a named lat/lon list when a label is given", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_locations,
    .package   = "BikeWise"
  )
  result <- get_locations("alice", "home")
  expect_type(result, "list")
  expect_named(result, c("lat", "lon"))
  expect_equal(result$lat, 52.30)
  expect_equal(result$lon, 4.80)
})

# Test that a label which does not exist for that user throws a clear error
test_that("get_locations errors with an informative message for an unknown label", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_locations,
    .package   = "BikeWise"
  )
  expect_error(
    get_locations("alice", "sports"),
    regexp = "No saved location"
  )
})

# Test that labels saved for one user are not visible to another
test_that("get_locations only returns rows belonging to the requested user", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_locations,
    .package   = "BikeWise"
  )
  result <- get_locations("bob")
  expect_equal(nrow(result), 1)
  expect_true(all(result$user == "bob"))
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that calling without a label returns all of that user's rows as a data frame
test_that("get_locations returns a data frame of all locations (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  result <- get_locations("alice", example = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$user == "alice"))
})

# Test that providing a label returns just the coordinates for that location
test_that("get_locations returns a named lat/lon list for a label (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  result <- get_locations("alice", "home", example = TRUE)
  expect_type(result, "list")
  expect_named(result, c("lat", "lon"))
  expect_equal(result$lat, 52.30)
  expect_equal(result$lon, 4.80)
})

# Test that a label which does not exist for that user throws a clear error
test_that("get_locations errors for an unknown label (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  expect_error(
    get_locations("alice", "sports", example = TRUE),
    regexp = "No saved location"
  )
})

# Test that labels saved for one user are not visible to another
test_that("get_locations only returns rows for the requested user (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  result <- get_locations("bob", example = TRUE)
  expect_equal(nrow(result), 1)
  expect_true(all(result$user == "bob"))
})
