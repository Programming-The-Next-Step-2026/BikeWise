#' Get saved locations for a user
#'
#' @param user A username string identifying the user.
#' @param label Optional. If provided, returns only the coordinates for that
#'   label as a named list with \code{lat} and \code{lon}. If omitted, returns
#'   all saved locations as a data frame.
#' @param example If \code{TRUE}, reads from a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCyclingOnline()}.
#'
#' @return When \code{label} is omitted: a data frame with columns
#'   \code{user}, \code{label}, \code{address}, \code{lat}, \code{lon}, and
#'   \code{display_name}. When \code{label} is provided: a named list with
#'   \code{lat} and \code{lon}, ready to pass directly into
#'   \code{bikeroute()}.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' get_locations("alice")
#' home <- get_locations("alice", "home")
#' work <- get_locations("alice", "work")
#' bikeroute(home$lat, home$lon, work$lat, work$lon)
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @export
get_locations <- function(user, label = NULL, example = FALSE) {

  # read from local CSV or Google Sheet depending on mode
  data <- if (example) {
    load_local_locations()
  } else {
    result_raw <- read_sheet(sheet_id(), sheet = "locations")
    result_raw <- result_raw[result_raw$user == user, ]
    result_raw$label <- vapply(
      result_raw$label, decrypt_value, character(1)
    )
    result_raw$address <- vapply(
      result_raw$address, decrypt_value, character(1)
    )
    result_raw$lat <- as.numeric(vapply(
      as.character(result_raw$lat), decrypt_value, character(1)
    ))
    result_raw$lon <- as.numeric(vapply(
      as.character(result_raw$lon), decrypt_value, character(1)
    ))
    result_raw$display_name <- vapply(
      result_raw$display_name, decrypt_value, character(1)
    )
    return(if (is.null(label)) result_raw else {
      match <- result_raw[result_raw$label == label, ]
      if (nrow(match) == 0) stop(
        "No saved location \"", label, "\" for user \"", user, "\". ",
        "Add it first with save_location()."
      )
      list(lat = match$lat[1], lon = match$lon[1])
    })
  }

  # filter to user's rows
  result <- data[data$user == user, ]

  # no label provided: return all locations as a data frame
  if (is.null(label)) {
    return(result)
  }

  # label provided: return just the coordinates for that location
  match <- result[result$label == label, ]
  if (nrow(match) == 0) {
    stop(
      "No saved location \"", label, "\" for user \"", user, "\". ",
      "Add it first with save_location()."
    )
  }
  list(lat = match$lat[1], lon = match$lon[1])
}
