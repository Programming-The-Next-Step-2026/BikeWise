# Tests for encrypt_value(), decrypt_value(), and sheet_id() in config.R.
# withr::local_envvar() sets a temporary key for each test so the real
# environment is never modified.

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

# ── load_local_users migrations ───────────────────────────────────────────────

test_that("load_local_users renames rain_preference to rain_tolerance", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  old <- data.frame(
    username        = "alice",
    password_hash   = "abc",
    rain_preference = "moderate",
    cycling_speed   = NA_real_
  )
  write.csv(old, file.path(tmp, "example_users.csv"), row.names = FALSE)
  users <- load_local_users()
  expect_true("rain_tolerance" %in% names(users))
  expect_false("rain_preference" %in% names(users))
})

test_that("load_local_users adds cycling_speed when column is missing", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(R_user_dir = function(...) tmp, .package = "BikeWise")
  old <- data.frame(
    username       = "alice",
    password_hash  = "abc",
    rain_tolerance = "moderate"
  )
  write.csv(old, file.path(tmp, "example_users.csv"), row.names = FALSE)
  users <- load_local_users()
  expect_true("cycling_speed" %in% names(users))
  expect_true(is.na(users$cycling_speed))
})
