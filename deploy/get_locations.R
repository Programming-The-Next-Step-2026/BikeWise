#' Get saved locations for a user
#'
#' @param user A username string identifying the user.
#' @param label Optional. If provided, returns only the coordinates for that
#'   label as a named list with \code{lat} and \code{lon}. If omitted, returns
#'   all saved locations as a data frame.
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
#' bikeroute(home$lat, home$lon, work$lat, work$lon)
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @export
get_locations <- function(user, label = NULL) {

  # read full locations sheet and filter to this user
  data   <- read_sheet(sheet_id(), sheet = "locations")
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
