# Tests for rain_preference().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table with one user whose preference is already set
fake_users <- data.frame(
  username        = "alice",
  password_hash   = "somehash",
  rain_preference = "moderate"
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that the getter returns whatever is stored in the sheet
test_that("rain_preference returns the stored preference when called without a value", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_equal(rain_preference("alice"), "moderate")
})

# Test that the setter writes the updated preference back to the sheet
test_that("rain_preference updates the sheet when called with a new value", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package    = "BikeWise"
  )
  rain_preference("alice", preference = "light")
  expect_equal(written$rain_preference[written$username == "alice"], "light")
})

# Test that the setter returns NULL invisibly
test_that("rain_preference returns NULL invisibly when setting a preference", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package    = "BikeWise"
  )
  result <- rain_preference("alice", preference = "heavy")
  expect_null(result)
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that the getter returns whatever is stored in the CSV
test_that("rain_preference returns the stored preference from CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(rain_preference("alice", example = TRUE), "moderate")
})

# Test that the setter writes the updated preference back to the CSV
test_that("rain_preference writes updated preference to CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  rain_preference("alice", preference = "light", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_equal(written$rain_preference[written$username == "alice"], "light")
})

# Test that the setter returns NULL invisibly
test_that("rain_preference setter returns NULL invisibly (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- rain_preference("alice", preference = "heavy", example = TRUE)
  expect_null(result)
})
