# Tests for StartCycling().
# The app is not actually launched — runApp() is mocked to capture the call.

test_that("StartCycling calls runApp with the bikewise-local app path", {
  called_with <- NULL
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      called_with <<- appDir
      invisible(NULL)
    },
    .package = "BikeWise"
  )
  StartCycling()
  expect_match(called_with, "apps/bikewise-local")
})
