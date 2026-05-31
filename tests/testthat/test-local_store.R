# Tests for the local CSV storage helpers in local_store.R.
# R_user_dir is mocked in every test so nothing is written to the real
# user data folder — only to a temporary directory that is cleaned up
# automatically after each test.

# ── Path helpers ──────────────────────────────────────────────────────────────

# Test that the users path points to the right filename
test_that("local_users_path returns a path ending in example_users.csv", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  expect_true(endsWith(local_users_path(), "example_users.csv"))
})

# Test that the locations path points to the right filename
test_that("local_locations_path ends with example_locations.csv", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  expect_true(endsWith(local_locations_path(), "example_locations.csv"))
})

# Test that the storage folder is created when it does not exist yet
test_that("local_store_dir creates the folder if it does not exist yet", {
  tmp    <- withr::local_tempdir()
  target <- file.path(tmp, "new_subdir")
  local_mocked_bindings(
    R_user_dir = function(...) target,
    .package = "BikeWise"
  )
  expect_false(dir.exists(target))
  local_store_dir()
  expect_true(dir.exists(target))
})

# ── Load helpers ──────────────────────────────────────────────────────────────

# Test that the users table has the right columns on a first-ever call
test_that("load_local_users returns correct columns on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  result <- load_local_users()
  expect_s3_class(result, "data.frame")
  expected_cols <- c("username", "password_hash", "rain_tolerance",
                     "cycling_speed")
  expect_named(result, expected_cols)
})

# Test that the CSV file is written to disk on the first call
test_that("load_local_users creates the CSV file on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  load_local_users()
  expect_true(file.exists(file.path(tmp, "example_users.csv")))
})

# Test that the locations table has the right columns on a first-ever call
test_that("load_local_locations returns correct columns on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  result <- load_local_locations()
  expect_s3_class(result, "data.frame")
  expect_named(result, c("user", "label", "address", "lat", "lon"))
})

# Test that the CSV file is written to disk on the first call
test_that("load_local_locations creates the CSV file on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  load_local_locations()
  expect_true(file.exists(file.path(tmp, "example_locations.csv")))
})

# ── Migration ─────────────────────────────────────────────────────────────────

# Test that load_local_users renames the old rain_preference column to
# rain_tolerance for existing installs that pre-date the rename
test_that("load_local_users migrates rain_preference column to rain_tolerance",
{
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  old_users <- data.frame(
    username        = "alice",
    password_hash   = "somehash",
    rain_preference = "moderate"
  )
  write.csv(old_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- load_local_users()
  expect_true("rain_tolerance" %in% names(result))
  expect_false("rain_preference" %in% names(result))
  expect_equal(result$rain_tolerance[result$username == "alice"], "moderate")
})

# Test that load_local_locations drops the display_name column for existing
# installs that pre-date its removal
test_that("load_local_locations drops display_name column from old installs", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  old_locs <- data.frame(
    user         = "alice",
    label        = "home",
    address      = "Addr A",
    lat          = 52.30,
    lon          = 4.80,
    display_name = "Home"
  )
  write.csv(old_locs, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  result <- load_local_locations()
  expect_false("display_name" %in% names(result))
})
