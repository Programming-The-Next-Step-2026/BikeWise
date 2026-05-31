# Tests for rain_tolerance().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table with one user whose tolerance is already set
fake_users <- data.frame(
  username        = "alice",
  password_hash   = "somehash",
  rain_tolerance  = "moderate"
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that the getter returns whatever is stored in the sheet
test_that("rain_tolerance returns stored tolerance without a value arg", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_equal(rain_tolerance("alice"), "moderate")
})

# Test that the setter writes the updated tolerance back to the sheet
test_that("rain_tolerance updates the sheet when called with a new value", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  rain_tolerance("alice", tolerance = "light")
  expect_equal(written$rain_tolerance[written$username == "alice"], "light")
})

# Test that the setter returns NULL invisibly
test_that("rain_tolerance returns NULL invisibly when setting a tolerance", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package    = "BikeWise"
  )
  result <- rain_tolerance("alice", tolerance = "heavy")
  expect_null(result)
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that the getter returns whatever is stored in the CSV
test_that("rain_tolerance returns the stored tolerance from CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(rain_tolerance("alice", example = TRUE), "moderate")
})

# Test that the setter writes the updated tolerance back to the CSV
test_that("rain_tolerance writes updated tolerance to CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  rain_tolerance("alice", tolerance = "light", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_equal(written$rain_tolerance[written$username == "alice"], "light")
})

# Test that the setter returns NULL invisibly
test_that("rain_tolerance setter returns NULL invisibly (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- rain_tolerance("alice", tolerance = "heavy", example = TRUE)
  expect_null(result)
})

# ── Encryption (Google Sheets backend) ───────────────────────────────────────

test_that("rain_tolerance getter decrypts stored tolerance from sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_users <- data.frame(
    username       = "alice",
    password_hash  = "somehash",
    rain_tolerance = encrypt_value("moderate")
  )
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) encrypted_users,
    .package   = "BikeWise"
  )
  expect_equal(rain_tolerance("alice"), "moderate")
})

test_that("rain_tolerance setter encrypts value before writing to sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  rain_tolerance("alice", tolerance = "light")
  stored <- written$rain_tolerance[written$username == "alice"]
  expect_false(stored == "light")
  expect_equal(decrypt_value(stored), "light")
})
