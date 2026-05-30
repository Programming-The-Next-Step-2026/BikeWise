# Tests for encrypt_value() and decrypt_value() in config.R.
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
