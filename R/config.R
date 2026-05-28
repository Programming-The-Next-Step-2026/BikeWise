PRESET_TITLES <- c(
  home      = "Home",
  work      = "Work",
  education = "Education",
  friends   = "Friends",
  sports    = "Sports",
  music     = "Music"
)

LABELS <- c("home", "work", "education", "friends", "sports", "music",
            "custom1", "custom2")

LABEL_ICONS <- c(
  home      = "home",
  work      = "briefcase",
  education = "graduation-cap",
  friends   = "users",
  sports    = "running",
  music     = "music",
  custom1   = "pencil",
  custom2   = "pencil"
)

LABEL_TITLES <- c(
  home      = "Home",
  work      = "Work",
  education = "Education",
  friends   = "Friends",
  sports    = "Sports",
  music     = "Music",
  custom1   = "Custom 1",
  custom2   = "Custom 2"
)

# mm/h value at which each tolerance level is breached
RAIN_THRESHOLDS <- c(none = 0.1, light = 2.5, moderate = 10, heavy = 10)

#' @noRd
sheet_id <- function() {
  id <- Sys.getenv("BIKEWISE_SHEET_ID")
  if (nchar(id) == 0) {
    stop(
      "BIKEWISE_SHEET_ID is not set."
    )
  }
  id
}
