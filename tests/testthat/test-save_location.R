# Tests for save_location() and its geocode() helper.
# Geocode section: live Nominatim calls, skipped if the API is unreachable.
# save_location sections mock geocode() to avoid network calls; the Sheets
# backend mocks sheet_id(), read_sheet(), and write_sheet(); the CSV backend
# mocks R_user_dir() to an isolated tmpdir cleaned up after each test.

# ── geocode helper ────────────────────────────────────────────────────────────

# geocode once — avoids repeated API calls across tests
result_dam <- tryCatch(BikeWise:::geocode("Dam Square, Amsterdam"),
                       error = function(e) NULL)

test_that("geocode returns a named list with lat and lon", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_type(result_dam, "list")
  expect_named(result_dam, c("lat", "lon"))
})

test_that("geocode returns numeric coordinates", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_type(result_dam$lat, "double")
  expect_type(result_dam$lon, "double")
})

test_that("geocode returns coordinates within valid worldwide bounds", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_true(result_dam$lat >= -90  && result_dam$lat <= 90)
  expect_true(result_dam$lon >= -180 && result_dam$lon <= 180)
})

test_that("geocode returns roughly correct coordinates for a known address", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_equal(result_dam$lat, 52.37, tolerance = 0.5)
  expect_equal(result_dam$lon, 4.89,  tolerance = 0.5)
})

test_that("geocode throws an informative error for an unrecognisable address", {
  skip_if(is.null(result_dam), "Nominatim API unavailable")
  expect_error(BikeWise:::geocode("xkzqwpfmvb12345"), "Address not found")
})

# ── Google Sheets backend (example = FALSE) ──────────────────────────────────

# A fake locations table — one existing alice/home row for deduplication tests
fake_locations <- data.frame(
  user    = "alice",
  label   = "home",
  address = "Old Street 1, Amsterdam",
  lat     = 52.30,
  lon     = 4.80
)

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

test_that("save_location preserves other labels for the same user (example)", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    R_user_dir = function(...) tmp,
    geocode    = function(address) list(lat = 52.37, lon = 4.89),
    .package   = "BikeWise"
  )
  write.csv(fake_locations, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  save_location("alice", "work", "Dam Square, Amsterdam", example = TRUE)
  written <- read.csv(file.path(tmp, "example_locations.csv"))
  expect_equal(sum(written$user == "alice"), 2)
  expect_equal(sum(written$label == "home"), 1)
})

# ── Encryption (Google Sheets backend) ───────────────────────────────────────

test_that("save_location encrypts sensitive fields before writing to sheet", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  written <- NULL
  local_mocked_bindings(
    sheet_id    = function() "dummy",
    geocode     = function(address) list(lat = 52.37, lon = 4.89),
    read_sheet  = function(...) {
      data.frame(
        user = character(), label = character(), address = character(),
        lat = numeric(), lon = numeric()
      )
    },
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

test_that("save_location deduplicates correctly when labels are encrypted", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key")
  encrypted_existing <- data.frame(
    user    = "alice",
    label   = encrypt_value("home"),
    address = encrypt_value("Old Street"),
    lat     = encrypt_value(52.30),
    lon     = encrypt_value(4.80)
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

# ── Validation ───────────────────────────────────────────────────────────────

test_that("save_location throws an error for an invalid label", {
  expect_error(
    save_location("alice", "office", "Dam Square, Amsterdam"),
    "label must be one of"
  )
})
