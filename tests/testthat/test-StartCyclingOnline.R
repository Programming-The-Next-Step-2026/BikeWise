# Tests for StartCyclingOnline() environment variable validation.
# The function should stop with a clear error before attempting any
# Google auth if required env vars are missing.

test_that("StartCyclingOnline stops when all three env vars are missing", {
  withr::local_envvar(
    BIKEWISE_SHEET_ID        = "",
    BIKEWISE_SERVICE_ACCOUNT = "",
    BIKEWISE_ENCRYPTION_KEY  = ""
  )
  expect_error(StartCyclingOnline(), "BIKEWISE_SHEET_ID")
  expect_error(StartCyclingOnline(), "BIKEWISE_SERVICE_ACCOUNT")
  expect_error(StartCyclingOnline(), "BIKEWISE_ENCRYPTION_KEY")
})

test_that("StartCyclingOnline stops when only some env vars are missing", {
  withr::local_envvar(
    BIKEWISE_SHEET_ID        = "some-id",
    BIKEWISE_SERVICE_ACCOUNT = "",
    BIKEWISE_ENCRYPTION_KEY  = "some-key"
  )
  err <- expect_error(StartCyclingOnline())
  expect_match(err$message, "BIKEWISE_SERVICE_ACCOUNT")
  expect_no_match(err$message, "BIKEWISE_SHEET_ID")
})

test_that("StartCyclingOnline error message references the vignette", {
  withr::local_envvar(
    BIKEWISE_SHEET_ID        = "",
    BIKEWISE_SERVICE_ACCOUNT = "",
    BIKEWISE_ENCRYPTION_KEY  = ""
  )
  expect_error(StartCyclingOnline(), "online-setup")
})

test_that("StartCyclingOnline calls runApp with the bikewise-online app path", {
  withr::local_envvar(
    BIKEWISE_SHEET_ID        = "some-id",
    BIKEWISE_SERVICE_ACCOUNT = "some-account",
    BIKEWISE_ENCRYPTION_KEY  = "some-key"
  )
  called_with <- NULL
  local_mocked_bindings(
    gs4_auth = function(...) invisible(NULL),
    runApp   = function(appDir, ...) {
      called_with <<- appDir
      invisible(NULL)
    },
    .package = "BikeWise"
  )
  StartCyclingOnline()
  expect_match(called_with, "apps/bikewise-online")
})
