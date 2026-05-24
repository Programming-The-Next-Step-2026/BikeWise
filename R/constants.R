PRESET_TITLES <- c(
  home      = "Home",
  work      = "Work",
  education = "Education",
  friends   = "Friends",
  sports    = "Sports",
  music     = "Music"
)

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
