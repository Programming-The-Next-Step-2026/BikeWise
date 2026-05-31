# Tests for get_locations().
# Google Sheets and sheet_id() are mocked so no real connection is needed.
# The local CSV backend is tested by mocking R_user_dir() to point at a
# temporary directory that is cleaned up after each test.

# A fake locations sheet with two rows for alice and one for bob
fake_locations <- data.frame(
  user    = c("alice", "alice", "bob"),
  label   = c("home",  "work",  "home"),
  address = c("Addr A", "Addr B", "Addr C"),
  lat     = c(52.30,   52.40,   52.50),
  lon     = c(4.80,    4.90,    5.00)
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that get_locations returns all of that user's rows as a data frame
test_that("get_locations returns a data frame of all locations for a user", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
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

# Test that labels saved for one user are not visible to another
test_that("get_locations only returns rows belonging to the requested user", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
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

# Test that get_locations returns all of that user's rows as a data frame
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

# ── Encryption (Google Sheets backend) ───────────────────────────────────────

# All coordinate fields are encrypted in the sheet — verify decryption
test_that("get_locations decrypts all fields returned from the sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_locs <- data.frame(
    user    = "alice",
    label   = encrypt_value("home"),
    address = encrypt_value("Dam Square"),
    lat     = encrypt_value(52.37),
    lon     = encrypt_value(4.89),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) encrypted_locs,
    .package   = "BikeWise"
  )
  result <- get_locations("alice")
  expect_equal(result$lat, 52.37)
  expect_equal(result$lon, 4.89)
})

# Multiple encrypted rows should all decrypt and return as a valid data frame
test_that("get_locations returns decrypted data frame for all user rows", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_locs <- data.frame(
    user    = c("alice", "alice"),
    label   = c(encrypt_value("home"), encrypt_value("work")),
    address = c(encrypt_value("Addr A"), encrypt_value("Addr B")),
    lat     = c(encrypt_value(52.30), encrypt_value(52.40)),
    lon     = c(encrypt_value(4.80), encrypt_value(4.90)),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    sheet_id   = function() "dummy",
    read_sheet = function(...) encrypted_locs,
    .package   = "BikeWise"
  )
  result <- get_locations("alice")
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$label), c("home", "work"))
  expect_type(result$lat, "double")
})
