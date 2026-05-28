# Integration tests for the BikeWise Shiny app using shinytest2.
#
# Setup (once, in your R console):
#   install.packages("shinytest2")
#   shinytest2::install_chromote()
#   devtools::install()          # package must be installed, not just loaded
#
# All tests are skipped automatically when shinytest2 is not available.

# Shared guard ─────────────────────────────────────────────────────────────────

skip_shiny <- function() {
  skip_if_not_installed("shinytest2")
  skip_if(
    system.file("shiny-examples/bikewise", package = "BikeWise") == "",
    "BikeWise not installed; run devtools::install() first"
  )
}

# Paths used by the example app's local CSV backend ───────────────────────────

store_dir <- tools::R_user_dir("BikeWise", "data")
users_csv <- file.path(store_dir, "example_users.csv")
locs_csv  <- file.path(store_dir, "example_locations.csv")

# CSV snapshot/restore ─────────────────────────────────────────────────────────
# Saves the current state of both CSV files and restores them when the
# calling test exits, so tests never pollute each other's data.

snapshot_csvs <- function(envir = parent.frame()) {
  users_snap <- if (file.exists(users_csv)) read.csv(users_csv) else NULL
  locs_snap  <- if (file.exists(locs_csv))  read.csv(locs_csv)  else NULL
  withr::defer(
    {
      if (is.null(users_snap)) {
        if (file.exists(users_csv)) file.remove(users_csv)
      } else {
        write.csv(users_snap, users_csv, row.names = FALSE)
      }
      if (is.null(locs_snap)) {
        if (file.exists(locs_csv)) file.remove(locs_csv)
      } else {
        write.csv(locs_snap, locs_csv, row.names = FALSE)
      }
    },
    envir = envir
  )
}

# CSV write helpers ────────────────────────────────────────────────────────────

write_user <- function(username, password, pref = "moderate") {
  dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)
  hash  <- digest::digest(password, algo = "sha256")
  users <- if (file.exists(users_csv)) {
    read.csv(users_csv)
  } else {
    data.frame(username        = character(),
               password_hash   = character(),
               rain_preference = character())
  }
  write.csv(
    rbind(users, data.frame(username = username, password_hash = hash,
                            rain_preference = pref)),
    users_csv, row.names = FALSE
  )
}

write_location <- function(user, label, lat, lon) {
  dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)
  locs <- if (file.exists(locs_csv)) {
    read.csv(locs_csv)
  } else {
    data.frame(user = character(), label = character(),
               address = character(), lat = numeric(),
               lon = numeric(), display_name = character())
  }
  write.csv(
    rbind(locs, data.frame(user = user, label = label,
                           address = "Test Address", lat = lat, lon = lon,
                           display_name = label)),
    locs_csv, row.names = FALSE
  )
}

# Helper: unique username per test run ─────────────────────────────────────────

test_user <- function(suffix = "") {
  paste0("st_", format(Sys.time(), "%H%M%S"), "_",
         sample(1000L:9999L, 1L), suffix)
}

# ── Tests ─────────────────────────────────────────────────────────────────────

# Test that a brand-new user lands on the rain preference screen after login
test_that("new user login navigates to rain preference screen", {
  skip_shiny()
  snapshot_csvs()

  app <- shinytest2::AppDriver$new(
    system.file("shiny-examples/bikewise", package = "BikeWise"),
    name = "new-user-login"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = test_user("n"), password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="tol_none"', html))
})

# Test that an existing user with the correct password lands on pick_start
test_that("existing user login navigates to pick_start screen", {
  skip_shiny()
  snapshot_csvs()

  username <- test_user("e")
  write_user(username, "pass123")

  app <- shinytest2::AppDriver$new(
    system.file("shiny-examples/bikewise", package = "BikeWise"),
    name = "exist-user-login"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass123")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# Test that a wrong password shows an error notification and stays on login
test_that("wrong password shows an error notification on the login screen", {
  skip_shiny()
  snapshot_csvs()

  username <- test_user("w")
  write_user(username, "correct")

  app <- shinytest2::AppDriver$new(
    system.file("shiny-examples/bikewise", package = "BikeWise"),
    name = "wrong-password"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "wrong")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl("shiny-notification-error", html))
})

# Test that clicking a rain tolerance button after login goes to pick_start
test_that("selecting rain tolerance navigates to pick_start screen", {
  skip_shiny()
  snapshot_csvs()

  app <- shinytest2::AppDriver$new(
    system.file("shiny-examples/bikewise", package = "BikeWise"),
    name = "select-tolerance"
  )
  on.exit(app$stop(), add = TRUE)

  # Log in as a new user — lands on rain_preference
  app$set_inputs(username = test_user("t"), password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  # Choose a tolerance → should navigate to pick_start
  app$click("tol_moderate")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# Test that clicking a pre-saved location on pick_start goes to pick_end
test_that("clicking a saved location navigates to pick_end screen", {
  skip_shiny()
  snapshot_csvs()

  username <- test_user("l")
  write_user(username, "pass", pref = "moderate")
  write_location(username, "home", 52.37, 4.89)

  app <- shinytest2::AppDriver$new(
    system.file("shiny-examples/bikewise", package = "BikeWise"),
    name = "saved-location"
  )
  on.exit(app$stop(), add = TRUE)

  # Existing user with a saved home → lands on pick_start
  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  # Home button is green (saved) → click it → should go to pick_end
  app$click("from_home")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="to_home"', html))
})
