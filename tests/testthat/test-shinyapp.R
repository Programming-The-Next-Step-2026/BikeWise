# Integration tests for the BikeWise Shiny app using shinytest2.
#
# Setup (once, in your R console):
#   install.packages("shinytest2")  # nolint: commented_code_linter.
#   shinytest2::install_chromote()  # nolint: commented_code_linter.
#   devtools::install()             # nolint: commented_code_linter.
# The package must be installed (not just loaded) for shinytest2 to work.
#
# All tests are skipped automatically when shinytest2 is not available.
# Online tests also require BIKEWISE_TEST_ONLINE=true plus valid credentials.


# ── Guards ────────────────────────────────────────────────────────────────────

skip_local <- function() {
  skip_if_not_installed("shinytest2")
  skip_if(
    system.file("apps/bikewise-local", package = "BikeWise") == "",
    "BikeWise not installed; run devtools::install() first"
  )
}

skip_online <- function() {
  skip_if_not_installed("shinytest2")
  skip_if(
    system.file("apps/bikewise-online", package = "BikeWise") == "",
    "BikeWise not installed; run devtools::install() first"
  )
  skip_if(
    Sys.getenv("BIKEWISE_TEST_ONLINE") != "true",
    "online tests require BIKEWISE_TEST_ONLINE=true and valid credentials"
  )
}


# ── CSV backend helpers (local app only) ──────────────────────────────────────

store_dir <- tools::R_user_dir("BikeWise", "data")
users_csv <- file.path(store_dir, "example_users.csv")
locs_csv  <- file.path(store_dir, "example_locations.csv")

# Saves both CSVs before a test and restores them on exit —
# no cross-test pollution.
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

write_user <- function(username, password, rain_tolerance = "moderate") {
  dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)
  hash  <- digest::digest(password, algo = "sha256")
  users <- if (file.exists(users_csv)) {
    read.csv(users_csv)
  } else {
    data.frame(username       = character(),
               password_hash  = character(),
               rain_tolerance = character(),
               cycling_speed  = numeric())
  }
  write.csv(
    rbind(users, data.frame(username = username, password_hash = hash,
                            rain_tolerance = rain_tolerance,
                            cycling_speed = NA_real_)),
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
               lon = numeric())
  }
  write.csv(
    rbind(locs, data.frame(user = user, label = label,
                           address = "Test Address", lat = lat, lon = lon)),
    locs_csv, row.names = FALSE
  )
}

# Unique username per test run — prevents bleed-through if snapshot_csvs fails.
test_user <- function(suffix = "") {
  paste0("st_", format(Sys.time(), "%H%M%S"), "_",
         sample(1000L:9999L, 1L), suffix)
}


# ── bikewise-local tests ──────────────────────────────────────────────────────

# New user gets the rain tolerance onboarding screen after their first login.
test_that("local: new user login navigates to rain tolerance screen", {
  skip_local()
  snapshot_csvs()

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-new-user"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = test_user("n"), password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="tol_none"', html))
})

# Existing user skips onboarding and lands directly on pick_start.
test_that("local: existing user login navigates to pick_start screen", {
  skip_local()
  snapshot_csvs()

  username <- test_user("e")
  write_user(username, "pass123")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-exist-user"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass123")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# Wrong password stays on login and surfaces an error notification.
test_that("local: wrong password shows error notification on login screen", {
  skip_local()
  snapshot_csvs()

  username <- test_user("w")
  write_user(username, "correct")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-wrong-password"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "wrong")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl("shiny-notification-error", html))
})

# Rain tolerance onboarding choice advances the flow to pick_start.
test_that("local: selecting rain tolerance navigates to pick_start screen", {
  skip_local()
  snapshot_csvs()

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-select-tolerance"
  )
  on.exit(app$stop(), add = TRUE)

  # New user lands on rain_tolerance — choose moderate and proceed.
  app$set_inputs(username = test_user("t"), password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  app$click("tol_moderate")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# A green (pre-saved) start location skips the modal and opens pick_end.
test_that("local: saved start location navigates to pick_end screen", {
  skip_local()
  snapshot_csvs()

  username <- test_user("l")
  write_user(username, "pass", rain_tolerance = "moderate")
  write_location(username, "home", 52.37, 4.89)

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-saved-start"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  # Home is pre-saved (green) — should jump straight to pick_end.
  app$click("from_home")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="to_home"', html))
})

# Back button on pick_end clears the end point and returns to pick_start.
test_that("local: back button on pick_end returns to pick_start screen", {
  skip_local()
  snapshot_csvs()

  username <- test_user("b")
  write_user(username, "pass", rain_tolerance = "moderate")
  write_location(username, "home", 52.37, 4.89)

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-back-btn"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()
  app$click("from_home")
  app$wait_for_idle()

  app$click("back_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# Logout wipes session state and returns the user to the login screen.
# Reaching logout_btn requires the route screen, which calls bikeroute() —
# skip when there is no internet connection.
test_that("local: logout navigates back to login screen", {
  skip_local()
  skip_if_offline()
  snapshot_csvs()

  username <- test_user("o")
  write_user(username, "pass", rain_tolerance = "moderate")
  write_location(username, "home", 52.37, 4.89)
  write_location(username, "work", 52.36, 4.88)

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-logout"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()
  app$click("from_home")
  app$wait_for_idle()
  app$click("to_work")
  app$wait_for_idle()

  app$click("logout_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="login_btn"', html))
})

# Settings cog navigates from pick_start to the settings screen.
test_that("local: settings button opens settings screen", {
  skip_local()
  snapshot_csvs()

  username <- test_user("s")
  write_user(username, "pass", rain_tolerance = "moderate")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-settings"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()

  app$click("settings_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="settings_back_btn"', html))
})

# Out-of-range speed is rejected with a warning; settings page stays open.
test_that("local: invalid speed input shows warning notification", {
  skip_local()
  snapshot_csvs()

  username <- test_user("sp")
  write_user(username, "pass", rain_tolerance = "moderate")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-local", package = "BikeWise"),
    name = "local-bad-speed"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = username, password = "pass")
  app$click("login_btn")
  app$wait_for_idle()
  app$click("settings_btn")
  app$wait_for_idle()

  # 0 is below the 1–100 accepted range.
  app$set_inputs(settings_speed_input = 0)
  app$click("settings_speed_save")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl("shiny-notification-warning", html))
})


# ── bikewise-online tests ─────────────────────────────────────────────────────
# These tests hit the live Google Sheets backend — they require valid
# credentials and BIKEWISE_TEST_ONLINE=true to run.

# Existing user skips onboarding and lands directly on pick_start.
# Requires BIKEWISE_TEST_USER / BIKEWISE_TEST_PASS to be set in the environment.
test_that("online: existing user login navigates to pick_start screen", {
  skip_online()
  skip_if(Sys.getenv("BIKEWISE_TEST_USER") == "",
          "set BIKEWISE_TEST_USER and BIKEWISE_TEST_PASS to run this test")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-online", package = "BikeWise"),
    name = "online-exist-user"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = Sys.getenv("BIKEWISE_TEST_USER"),
                 password = Sys.getenv("BIKEWISE_TEST_PASS"))
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl('id="from_home"', html))
})

# Wrong password stays on login and surfaces an error notification.
test_that("online: wrong password shows error notification on login screen", {
  skip_online()
  skip_if(Sys.getenv("BIKEWISE_TEST_USER") == "",
          "set BIKEWISE_TEST_USER to run this test")

  app <- shinytest2::AppDriver$new(
    system.file("apps/bikewise-online", package = "BikeWise"),
    name = "online-wrong-password"
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(username = Sys.getenv("BIKEWISE_TEST_USER"),
                 password = "definitelywrong_xyz")
  app$click("login_btn")
  app$wait_for_idle()

  html <- app$get_html("body")
  expect_true(grepl("shiny-notification-error", html))
})
