# Internal helpers for reading and writing the local CSV files that are used
# instead of Google Sheets when example = TRUE

# Return (and create if needed) the folder where the example CSVs are stored
#' @importFrom tools R_user_dir
#' @noRd
local_store_dir <- function() {
  dir <- R_user_dir("BikeWise", which = "data")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

# Full path to the users CSV file
#' @noRd
local_users_path <- function() {
  file.path(local_store_dir(), "example_users.csv")
}

# Full path to the locations CSV file
#' @noRd
local_locations_path <- function() {
  file.path(local_store_dir(), "example_locations.csv")
}

# Read the users CSV; if it does not exist yet,
# create an empty one and return it
#' @noRd
load_local_users <- function() {
  path <- local_users_path()
  if (!file.exists(path)) {
    empty <- data.frame(
      username        = character(),
      password_hash   = character(),
      rain_preference = character(),
      cycling_speed   = numeric()
    )
    write.csv(empty, path, row.names = FALSE)
    return(empty)
  }
  users <- read.csv(path)
  # add cycling_speed column if missing — handles existing installs
  if (!"cycling_speed" %in% names(users)) {
    users$cycling_speed <- NA_real_
  }
  users
}

# Read the locations CSV; if it does not exist yet,
# create an empty one and return it
#' @noRd
load_local_locations <- function() {
  path <- local_locations_path()
  if (!file.exists(path)) {
    empty <- data.frame(
      user         = character(),
      label        = character(),
      address      = character(),
      lat          = numeric(),
      lon          = numeric(),
      display_name = character()
    )
    write.csv(empty, path, row.names = FALSE)
    return(empty)
  }
  read.csv(path)
}
