# Tests for authenticate_user().
# Google Sheets and sheet_id() are mocked so no real connection is needed.

# A fake users sheet with one existing user whose password is "secret"
fake_users <- data.frame(
  username        = "alice",
  password_hash   = digest::digest("secret", algo = "sha256"),
  rain_preference = "moderate",
  stringsAsFactors = FALSE
)

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
test_that("authenticate_user returns 'wrong_password' for an incorrect password", {
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
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
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
    write_sheet = function(data, ...) { written <<- data; invisible(NULL) },
    .package = "BikeWise"
  )
  authenticate_user("bob", "mypassword")
  new_row <- written[written$username == "bob", ]
  expect_false(new_row$password_hash == "mypassword")
  expect_equal(new_row$password_hash,
               digest::digest("mypassword", algo = "sha256"))
})
