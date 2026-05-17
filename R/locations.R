valid_labels <- c("home", "work", "education", "friends", "sports", "music")

#' @noRd
sheet_id <- function() {
  id <- Sys.getenv("BIKEWISE_SHEET_ID")
  if (nchar(id) == 0) {
    stop(
      "BIKEWISE_SHEET_ID is not set. ",
      "Add it to your .Renviron file: BIKEWISE_SHEET_ID=<your-sheet-id>"
    )
  }
  id
}


#' Save a location for a user
#'
#' @param user A username string identifying the user.
#' @param label The name of the location. One of \code{"home"},
#'   \code{"work"}, \code{"education"}, \code{"friends"}, \code{"sports"},
#'   or \code{"music"}.
#' @param address A plain text address (e.g. \code{"Dam Square, Amsterdam"}).
#'   Geocoded automatically — no coordinates needed.
#'
#' @return The saved coordinates invisibly as a named list with \code{lat}
#'   and \code{lon}.
#'
#' @details Geocodes the address using \code{geocode()} and writes the result
#'   to the BikeWise Google Sheet. If a location with the same label already
#'   exists for this user, it is overwritten. Requires the environment variable
#'   \code{BIKEWISE_SHEET_ID} to be set and a valid Google account authenticated
#'   via \code{googlesheets4::gs4_auth()}.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' save_location("alice", "home", "Keizersgracht 1, Amsterdam")
#' save_location("alice", "work", "Science Park 904, Amsterdam")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @noRd
save_location <- function(user, label, address) {
  if (!label %in% valid_labels) {
    stop(
      "\"", label, "\" is not a valid label. ",
      "Choose one of: ", paste(valid_labels, collapse = ", "), "."
    )
  }

  coords <- geocode(address)

  # Read existing data, remove old entry for this user + label if present
  existing <- read_sheet(sheet_id(), sheet = "locations")
  existing <- existing[!(existing$user == user & existing$label == label), ]

  new_row <- data.frame(
    user    = user,
    label   = label,
    address = address,
    lat     = coords$lat,
    lon     = coords$lon
  )

  write_sheet(rbind(existing, new_row), ss = sheet_id(), sheet = "locations")

  invisible(coords)
}


#' Get all saved locations for a user
#'
#' @param user A username string identifying the user.
#'
#' @return A data frame with columns \code{user}, \code{label},
#'   \code{address}, \code{lat}, and \code{lon}. Returns an empty data
#'   frame if the user has no saved locations yet.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' get_locations("alice")
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @noRd
get_locations <- function(user) {
  data <- read_sheet(sheet_id(), sheet = "locations")
  data[data$user == user, ]
}


#' Get the coordinates of one saved location for a user
#'
#' @param user A username string identifying the user.
#' @param label The name of the location. One of \code{"home"},
#'   \code{"work"}, \code{"education"}, \code{"friends"}, \code{"sports"},
#'   or \code{"music"}.
#'
#' @return A named list with \code{lat} and \code{lon}, ready to pass
#'   directly into \code{bikeroute()}.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' home <- get_location("alice", "home")
#' bikeroute(home$lat, home$lon, work$lat, work$lon)
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @noRd
get_location <- function(user, label) {
  locs  <- get_locations(user)
  match <- locs[locs$label == label, ]

  if (nrow(match) == 0) {
    stop(
      "No saved location \"", label, "\" for user \"", user, "\". ",
      "Add it first with save_location()."
    )
  }

  list(lat = match$lat[1], lon = match$lon[1])
}
