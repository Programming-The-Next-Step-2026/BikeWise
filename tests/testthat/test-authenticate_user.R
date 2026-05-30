# Tests for authenticate_user().
# The Google Sheets backend is tested by mocking sheet_id(), read_sheet(), and
# write_sheet(). The local CSV backend is tested by mocking R_user_dir() to
# point at a temporary directory that is cleaned up after each test.

# A fake users table with one existing user whose password is "secret"
fake_users <- data.frame(
  username        = "alice",
  password_hash   = digest::digest("secret", algo = "sha256"),
  rain_preference = "moderate",
  cycling_speed   = NA_real_
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that a brand-new username gets an account created
test_that("authenticate_user returns 'created' for a new username", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package = "BikeWise"
  )
  expect_equal(authenticate_user("bob", "mypassword"), "created")
})

# Test that the correct password gives access to an existing account
test_that("authenticate_user returns 'authenticated' for a correct password", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package = "BikeWise"
  )
  expect_equal(authenticate_user("alice", "secret"), "authenticated")
})

# Test that the wrong password is rejected
test_that("authenticate_user returns 'wrong_password' for wrong password", {
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(...) invisible(NULL),
    .package = "BikeWise"
  )
  expect_equal(authenticate_user("alice", "notmypassword"), "wrong_password")
})

# Test that creating a new account actually writes the new row to the sheet
test_that("authenticate_user writes a new row when creating an account", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package = "BikeWise"
  )
  authenticate_user("bob", "mypassword")
  expect_true("bob" %in% written$username)
})

# Test that the stored password is hashed, not plain-text
test_that("authenticate_user stores a SHA-256 hash, not the plain password", {
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    read_sheet  = function(...) fake_users,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package = "BikeWise"
  )
  authenticate_user("bob", "mypassword")
  new_row <- written[written$username == "bob", ]
  expect_false(new_row$password_hash == "mypassword")
  expect_equal(new_row$password_hash,
               digest::digest("mypassword", algo = "sha256"))
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that a brand-new username gets an account created in the local CSV
test_that("authenticate_user returns 'created' for a new username (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  expect_equal(authenticate_user("bob", "mypassword", example = TRUE),
               "created")
})

# Test that the correct password gives access to an existing account
# in the local CSV
test_that("authenticate_user authenticates correct password in CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(authenticate_user("alice", "secret", example = TRUE),
               "authenticated")
})

# Test that the wrong password is rejected in the local CSV
test_that("authenticate_user rejects wrong password via CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  write.csv(fake_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  expect_equal(authenticate_user("alice", "notmypassword", example = TRUE),
               "wrong_password")
})

# Test that creating a new account actually writes the new row to the CSV
test_that("authenticate_user writes new row to CSV on creation (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  authenticate_user("bob", "mypassword", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  expect_true("bob" %in% written$username)
})

# Test that the stored password is hashed, not plain-text
test_that("authenticate_user stores hash not plain password in CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    .package = "BikeWise"
  )
  authenticate_user("bob", "mypassword", example = TRUE)
  written <- read.csv(file.path(tmp, "example_users.csv"))
  new_row <- written[written$username == "bob", ]
  expect_false(new_row$password_hash == "mypassword")
  expect_equal(new_row$password_hash,
               digest::digest("mypassword", algo = "sha256"))
})
