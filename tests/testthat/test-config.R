# Tests for config.R: encryption helpers, sheet_id, and local CSV storage.
# withr::local_envvar() sets temporary env vars so the real environment is
# never modified. withr::local_tempdir() is used for CSV tests so nothing
# is written to the real user data folder.

# ── No-op when key is not set ─────────────────────────────────────────────────

test_that("encrypt_value returns string unchanged when no key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  expect_equal(encrypt_value("moderate"), "moderate")
})

test_that("encrypt_value returns numeric unchanged when no key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  expect_equal(encrypt_value(15), 15)
})

test_that("decrypt_value returns value unchanged when no key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  expect_equal(decrypt_value("moderate"), "moderate")
})

test_that("encrypt_value returns NA unchanged when no key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  expect_true(is.na(encrypt_value(NA)))
})

test_that("decrypt_value returns NA unchanged when no key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "")
  expect_true(is.na(decrypt_value(NA)))
})

# ── Round-trip with key set ───────────────────────────────────────────────────

test_that("encrypt then decrypt recovers the original string", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key-abc123")
  expect_equal(decrypt_value(encrypt_value("moderate")), "moderate")
})

test_that("encrypt then decrypt recovers a numeric as its string form", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key-abc123")
  expect_equal(decrypt_value(encrypt_value(15)), "15")
})

test_that("encrypt_value returns NA unchanged even when key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key-abc123")
  expect_true(is.na(encrypt_value(NA)))
})

test_that("decrypt_value returns NA unchanged even when key is set", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key-abc123")
  expect_true(is.na(decrypt_value(NA)))
})

# ── Ciphertext is randomised ──────────────────────────────────────────────────

test_that("encrypting the same value twice gives different ciphertext", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "test-key-abc123")
  ct1 <- encrypt_value("moderate")
  ct2 <- encrypt_value("moderate")
  expect_false(identical(ct1, ct2))
})

# ── Wrong key cannot decrypt ──────────────────────────────────────────────────

test_that("decrypt_value with wrong key does not return the original", {
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "key-A")
  ct <- encrypt_value("moderate")
  withr::local_envvar(BIKEWISE_ENCRYPTION_KEY = "key-B")
  expect_error(decrypt_value(ct))
})

# ── sheet_id ──────────────────────────────────────────────────────────────────

test_that("sheet_id returns the env var value when set", {
  withr::local_envvar(BIKEWISE_SHEET_ID = "my-sheet-id")
  expect_equal(sheet_id(), "my-sheet-id")
})

test_that("sheet_id stops when BIKEWISE_SHEET_ID is not set", {
  withr::local_envvar(BIKEWISE_SHEET_ID = "")
  expect_error(sheet_id(), "BIKEWISE_SHEET_ID is not set")
})

# ── Local storage — path helpers ──────────────────────────────────────────────

test_that("local_users_path returns a path ending in example_users.csv", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  expect_true(endsWith(local_users_path(), "example_users.csv"))
})

test_that("local_locations_path ends with example_locations.csv", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  expect_true(endsWith(local_locations_path(), "example_locations.csv"))
})

test_that("local_store_dir creates the folder if it does not exist yet", {
  tmp    <- withr::local_tempdir()
  target <- file.path(tmp, "new_subdir")
  local_mocked_bindings(
    R_user_dir = function(...) target,
    .package = "BikeWise"
  )
  expect_false(dir.exists(target))
  local_store_dir()
  expect_true(dir.exists(target))
})

# ── Local storage — first-call initialisation ─────────────────────────────────

test_that("load_local_users returns correct columns on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  result <- load_local_users()
  expect_s3_class(result, "data.frame")
  expect_named(result, c("username", "password_hash", "rain_tolerance",
                         "cycling_speed"))
})

test_that("load_local_users creates the CSV file on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  load_local_users()
  expect_true(file.exists(file.path(tmp, "example_users.csv")))
})

test_that("load_local_locations returns correct columns on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  result <- load_local_locations()
  expect_s3_class(result, "data.frame")
  expect_named(result, c("user", "label", "address", "lat", "lon"))
})

test_that("load_local_locations creates the CSV file on first call", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  load_local_locations()
  expect_true(file.exists(file.path(tmp, "example_locations.csv")))
})

# ── Local storage — migrations ────────────────────────────────────────────────

test_that("load_local_users migrates rain_preference to rain_tolerance", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  old_users <- data.frame(
    username        = "alice",
    password_hash   = "somehash",
    rain_preference = "moderate"
  )
  write.csv(old_users, file.path(tmp, "example_users.csv"), row.names = FALSE)
  result <- load_local_users()
  expect_true("rain_tolerance" %in% names(result))
  expect_false("rain_preference" %in% names(result))
  expect_equal(result$rain_tolerance[result$username == "alice"], "moderate")
})

test_that("load_local_users adds cycling_speed column when missing", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
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

test_that("load_local_locations drops display_name column from old installs", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  old_locs <- data.frame(
    user         = "alice",
    label        = "home",
    address      = "Addr A",
    lat          = 52.30,
    lon          = 4.80,
    display_name = "Home"
  )
  write.csv(old_locs, file.path(tmp, "example_locations.csv"),
            row.names = FALSE)
  result <- load_local_locations()
  expect_false("display_name" %in% names(result))
})
