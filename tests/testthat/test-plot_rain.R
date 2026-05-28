# Tests for plot_rain().
# No API calls — all tests run offline.

# A minimal route_rain_summary fixture with light rain throughout
fake_summary <- data.frame(
  time_min   = c(0,     5,     10),
  dist_km    = c(0,     1,      2),
  lon        = c(4.89, 4.90,  4.91),
  lat        = c(52.37, 52.36, 52.35),
  rain_mm_h  = c(0.5,  1.0,   0.8),
  rain_level = c("light", "light", "light")
)

# Test that a valid summary produces a ggplot object
test_that("plot_rain returns a ggplot object for valid input", {
  p <- plot_rain(fake_summary)
  expect_s3_class(p, "gg")
})

# Test that NULL input returns a blank ggplot rather than an error
test_that("plot_rain returns a ggplot object when route_rain_summary is NULL", {
  p <- plot_rain(NULL)
  expect_s3_class(p, "gg")
})

# Test that a threshold line is drawn for non-heavy tolerances
test_that("plot_rain adds a threshold line for non-heavy tolerance", {
  p <- plot_rain(fake_summary, tolerance = "moderate")
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomHline" %in% layer_classes)
})

# Test that no threshold line is drawn when the user cycles in any weather
test_that("plot_rain omits the threshold line for heavy tolerance", {
  p <- plot_rain(fake_summary, tolerance = "heavy")
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_false("GeomHline" %in% layer_classes)
})

# Test that light tolerance also gets a threshold line
test_that("plot_rain adds a threshold line for light tolerance", {
  p <- plot_rain(fake_summary, tolerance = "light")
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomHline" %in% layer_classes)
})
