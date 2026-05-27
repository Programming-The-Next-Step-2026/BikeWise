# Tests for rain_preference().
# Google Sheets and sheet_id() are mocked so no real connection is needed.

# A fake users sheet with one user whose preference is already set
fake_users <- data.frame(
  username        = "alice",
  password_hash   = "somehash",
  rain_preference = "moderate",
  stringsAsFactors = FALSE
)

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

# Test that the setter returns invisibly (no visible return value)
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
