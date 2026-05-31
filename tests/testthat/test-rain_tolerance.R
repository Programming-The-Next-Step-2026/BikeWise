# Tests for rain_tolerance().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table — alice has a tolerance set, bob does not
fake_users <- data.frame(
  username       = c("alice", "bob"),
  password_hash  = c("somehash", "otherhash"),
  rain_tolerance = c("moderate", NA_character_)
)

# ── Input validation ──────────────────────────────────────────────────────────

test_that("rain_tolerance stops for an invalid tolerance value", {
  expect_error(
    rain_tolerance("alice", tolerance = "extreme", example = TRUE),
    "tolerance must be one of"
  )
})

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

test_that("rain_tolerance returns stored tolerance without a value arg", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_equal(rain_tolerance("alice"), "moderate")
})

test_that("rain_tolerance returns NA when no tolerance has been saved", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_true(is.na(rain_tolerance("bob")))
})

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

test_that("rain_tolerance only updates the target user", {
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
  expect_true(is.na(written$rain_tolerance[written$username == "bob"]))
})

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

test_that("rain_tolerance returns the stored tolerance from CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(rain_tolerance("alice", example = TRUE), "moderate")
})

test_that("rain_tolerance returns NA when no tolerance is set (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_true(is.na(rain_tolerance("bob", example = TRUE)))
})

test_that("rain_tolerance writes updated tolerance to CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  rain_tolerance("alice", tolerance = "light", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_equal(written$rain_tolerance[written$username == "alice"], "light")
})

test_that("rain_tolerance only updates the target user in CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  rain_tolerance("alice", tolerance = "light", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_true(is.na(written$rain_tolerance[written$username == "bob"]))
})

test_that("rain_tolerance setter returns NULL invisibly (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
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
