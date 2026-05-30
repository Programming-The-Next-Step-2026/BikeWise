# Tests for cycling_speed().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table — one user with a speed set, one without
fake_users <- data.frame(
  username        = c("alice", "bob"),
  password_hash   = c("somehash", "otherhash"),
  rain_preference = c("moderate", NA_character_),
  cycling_speed   = c(20, NA_real_)
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that the getter returns whatever speed is stored in the sheet
test_that("cycling_speed returns stored speed without a value arg", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_equal(cycling_speed("alice"), 20)
})

# Test that the getter returns NA for a user with no speed saved
test_that("cycling_speed returns NA when no speed has been saved", {
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_true(is.na(cycling_speed("bob")))
})

# Test that the setter writes the updated speed back to the sheet
test_that("cycling_speed updates the sheet when called with a new value", {
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
  cycling_speed("alice", speed_kmh = 25)
  expect_equal(written$cycling_speed[written$username == "alice"], 25)
})

# Test that the setter only updates the right user's speed
test_that("cycling_speed only updates the target user", {
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
  cycling_speed("alice", speed_kmh = 25)
  expect_true(is.na(written$cycling_speed[written$username == "bob"]))
})

# Test that the setter returns NULL invisibly
test_that("cycling_speed returns NULL invisibly when setting a speed", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package    = "BikeWise"
  )
  result <- cycling_speed("alice", speed_kmh = 18)
  expect_null(result)
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that the getter returns the stored speed from CSV
test_that("cycling_speed returns stored speed from CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(cycling_speed("alice", example = TRUE), 20)
})

# Test that the getter returns NA for a user with no speed saved
test_that("cycling_speed returns NA from CSV when no speed set (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_true(is.na(cycling_speed("bob", example = TRUE)))
})

# Test that the setter writes the updated speed to the CSV
test_that("cycling_speed writes updated speed to CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  cycling_speed("alice", speed_kmh = 25, example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_equal(written$cycling_speed[written$username == "alice"], 25)
})

# Test that the setter returns NULL invisibly
test_that("cycling_speed setter returns NULL invisibly (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- cycling_speed("alice", speed_kmh = 18, example = TRUE)
  expect_null(result)
})

# ── Migration ─────────────────────────────────────────────────────────────────

# Test that load_local_users adds cycling_speed column when it is missing
# (handles existing installs that pre-date this column)
test_that("load_local_users adds cycling_speed column to old CSVs", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  # write a legacy CSV without cycling_speed
  old_users <- data.frame(
    username        = "alice",
    password_hash   = "somehash",
    rain_preference = "moderate"
  )
  write.csv(old_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- load_local_users()
  expect_true("cycling_speed" %in% names(result))
  expect_true(is.na(result$cycling_speed[result$username == "alice"]))
})
