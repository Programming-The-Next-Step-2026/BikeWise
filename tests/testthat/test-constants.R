test_that("sheet_id() errors with a clear message when env var is not set", {
  withr::with_envvar(c(BIKEWISE_SHEET_ID = ""), {
    expect_error(
      BikeWise:::sheet_id(),
      regexp = "BIKEWISE_SHEET_ID is not set"
    )
  })
})

test_that("sheet_id() returns the env var value when set", {
  withr::with_envvar(c(BIKEWISE_SHEET_ID = "test-sheet-id"), {
    expect_equal(BikeWise:::sheet_id(), "test-sheet-id")
  })
})
