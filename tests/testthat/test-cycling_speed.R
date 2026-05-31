# Tests for cycling_speed().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table — one user with a speed set, one without
fake_users <- data.frame(
  username       = c("alice", "bob"),
  password_hash  = c("somehash", "otherhash"),
  rain_tolerance = c("moderate", NA_character_),
  cycling_speed  = c(20, NA_real_)
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

test_that("cycling_speed returns stored speed without a value arg", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_equal(cycling_speed("alice"), 20)
})

test_that("cycling_speed returns NA when no speed has been saved", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) fake_users,
    .package   = "BikeWise"
  )
  expect_true(is.na(cycling_speed("bob")))
})

test_that("cycling_speed updates the sheet when called with a new value", {
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
  cycling_speed("alice", speed_kmh = 25)
  expect_equal(written$cycling_speed[written$username == "alice"], 25)
})

test_that("cycling_speed only updates the target user", {
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
  cycling_speed("alice", speed_kmh = 25)
  expect_true(is.na(written$cycling_speed[written$username == "bob"]))
})

test_that("cycling_speed returns NULL invisibly when setting a speed", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
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

test_that("cycling_speed returns stored speed from CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(cycling_speed("alice", example = TRUE), 20)
})

test_that("cycling_speed returns NA from CSV when no speed set (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_true(is.na(cycling_speed("bob", example = TRUE)))
})

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

test_that("cycling_speed only updates the target user in CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  cycling_speed("alice", speed_kmh = 25, example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_true(is.na(written$cycling_speed[written$username == "bob"]))
})

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

test_that("load_local_users adds cycling_speed column to old CSVs", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  old_users <- data.frame(
    username       = "alice",
    password_hash  = "somehash",
    rain_tolerance = "moderate"
  )
  write.csv(old_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- load_local_users()
  expect_true("cycling_speed" %in% names(result))
  expect_true(is.na(result$cycling_speed[result$username == "alice"]))
})

# ── Encryption (Google Sheets backend) ───────────────────────────────────────

test_that("cycling_speed getter decrypts and returns numeric from sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_users <- data.frame(
    username       = "alice",
    password_hash  = "somehash",
    rain_tolerance = "moderate",
    cycling_speed  = encrypt_value(20)
  )
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) encrypted_users,
    .package   = "BikeWise"
  )
  result <- cycling_speed("alice")
  expect_equal(result, 20)
  expect_type(result, "double")
})

test_that("cycling_speed setter encrypts value before writing to sheet", {
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
  cycling_speed("alice", speed_kmh = 25)
  stored <- as.character(
    written$cycling_speed[written$username == "alice"]
  )
  expect_false(stored == "25")
  expect_equal(as.numeric(decrypt_value(stored)), 25)
})
