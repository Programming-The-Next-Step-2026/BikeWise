# Tests for save_location() and its internal geocode() helper.
# geocode() is mocked to avoid real Nominatim calls. The Google Sheets backend
# is tested by mocking sheet_id(), read_sheet(), and write_sheet(). The local
# CSV backend is tested by mocking R_user_dir() to point at a temporary
# directory that is cleaned up after each test.

# A fake locations table with one existing row for alice/home
fake_locations <- data.frame(
  user    = "alice",
  label   = "home",
  address = "Old Street 1, Amsterdam",
  lat     = 52.30,
  lon     = 4.80
)

# ── Google Sheets backend (example = FALSE) ───────────────────────────────────

# Test that save_location returns the geocoded coordinates
test_that("save_location returns lat and lon", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(...) invisible(NULL),
    .package    = "BikeWise"
  )
  result <- save_location("alice", "work", "Dam Square, Amsterdam")
  expect_type(result, "list")
  expect_named(result, c("lat", "lon"))
  expect_equal(result$lat, 52.37)
  expect_equal(result$lon, 4.89)
})

# Test that saving an existing label replaces the old row (no duplicate added)
test_that("save_location replaces existing entry for same user/label", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  save_location("alice", "home", "New Home Street 5, Amsterdam")
  expect_equal(sum(written$user == "alice" & written$label == "home"), 1)
})

# Test that the coordinates written to the sheet match what geocode returned
test_that("save_location writes geocoded coordinates to the sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 51.50, lon = -0.12),
    read_sheet  = function(...) fake_locations,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  save_location("alice", "work", "London Bridge, London")
  new_row <- written[written$label == "work", ]
  expect_equal(new_row$lat, 51.50)
  expect_equal(new_row$lon, -0.12)
})

# ── Local CSV backend (example = TRUE) ───────────────────────────────────────

# Test that save_location returns the geocoded coordinates from the CSV backend
test_that("save_location returns lat and lon (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    geocode    = function(address) list(lat = 52.37, lon = 4.89),
    .package   = "BikeWise"
  )
  result <- save_location("alice", "work", "Dam Square, Amsterdam",
                          example = TRUE)
  expect_type(result, "list")
  expect_named(result, c("lat", "lon"))
  expect_equal(result$lat, 52.37)
  expect_equal(result$lon, 4.89)
})

# Test that saving an existing label replaces the old row (no duplicate added)
test_that("save_location replaces existing entry for same label (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    geocode    = function(address) list(lat = 52.37, lon = 4.89),
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  save_location("alice", "home", "New Home Street 5, Amsterdam", example = TRUE)
  written <- read.csv(file.path(tmp, "example_locations.csv"))
  expect_equal(sum(written$user == "alice" & written$label == "home"), 1)
})

# Test that the coordinates written to the CSV match what geocode returned
test_that("save_location writes geocoded coordinates to the CSV (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    geocode    = function(address) list(lat = 51.50, lon = -0.12),
    .package   = "BikeWise"
  )
  save_location("alice", "work", "London Bridge, London", example = TRUE)
  written <- read.csv(file.path(tmp, "example_locations.csv"))
  new_row <- written[written$label == "work", ]
  expect_equal(new_row$lat, 51.50)
  expect_equal(new_row$lon, -0.12)
})

# ── Encryption (Google Sheets backend) ───────────────────────────────────────

# Sensitive fields must be unreadable in the sheet without the key
test_that("save_location encrypts sensitive fields before writing to sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) {
      list(lat = 52.37, lon = 4.89)
    },
    read_sheet  = function(...) data.frame(
      user = character(), label = character(), address = character(),
      lat = numeric(), lon = numeric()
    ),
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  save_location("alice", "home", "Dam Square")
  expect_false(written$label == "home")
  expect_false(written$address == "Dam Square")
  expect_equal(decrypt_value(written$label), "home")
  expect_equal(as.numeric(decrypt_value(as.character(written$lat))), 52.37)
})

# Deduplication must work against the encrypted label column in the sheet
test_that("save_location deduplicates correctly when labels are encrypted", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_existing <- data.frame(
    user    = "alice",
    label   = encrypt_value("home"),
    address = encrypt_value("Old Street"),
    lat     = encrypt_value(52.30),
    lon     = encrypt_value(4.80),
    stringsAsFactors = FALSE
  )
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) encrypted_existing,
    write_sheet = function(data, ...) {
      written <<- data
      invisible(NULL)
    },
    .package    = "BikeWise"
  )
  save_location("alice", "home", "New Street")
  decrypted_labels <- vapply(
    as.character(written$label), decrypt_value, character(1)
  )
  expect_equal(
    sum(written$user == "alice" & decrypted_labels == "home"), 1
  )
})
