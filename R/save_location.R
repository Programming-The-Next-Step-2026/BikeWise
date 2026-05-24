# Convert a street address to lat/lon using the Nominatim API
#' @importFrom httr2 request req_url_query req_headers req_perform
#' @importFrom httr2 resp_body_json
#' @noRd
geocode <- function(address) {

  # create query
  resp <- request("https://nominatim.openstreetmap.org/search") |>
    
    # needed info, including address input
    req_url_query(q = address, format = "json", limit = 1) |>
    
    # needed user, as otherwise blocked request
    req_headers(`User-Agent` = "BikeWise R package") |>
    req_perform()

  # return JSON response as list
  results <- resp |> resp_body_json()

  # give warning if address not found
  if (length(results) == 0) {
    stop(
      "Address not found: \"", address, "\". "
    )
  }

  # return latitude and longitude as list
  list(
    lat = as.numeric(results[[1]]$lat),
    lon = as.numeric(results[[1]]$lon)
  )
}

#' Save a location for a user
#'
#' @param user A username string identifying the user.
#' @param label The name of the location. One of the preset labels
#'   (\code{"home"}, \code{"work"}, etc.) or \code{"custom1"} /
#'   \code{"custom2"} for user-defined locations.
#' @param address A plain text address (e.g. \code{"Dam Square, Amsterdam"}).
#'   Geocoded automatically — no coordinates needed.
#' @param display_name Optional display name for the location card. Only used
#'   when \code{label} is \code{"custom1"} or \code{"custom2"}.
#'
#' @return The saved coordinates as a named list with \code{lat} and
#'   \code{lon}.
#'
#' @details Geocodes the address and writes the result to the BikeWise Google
#'   Sheet. If a location with the same label already exists for this user, it
#'   is overwritten. Requires the environment variable
#'   \code{BIKEWISE_SHEET_ID} to be set and a valid Google account
#'   authenticated via \code{googlesheets4::gs4_auth()}.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' save_location("alice", "home", "Keizersgracht 1, Amsterdam")
#' save_location("alice", "custom1", "Science Park 904, Amsterdam",
#'               display_name = "Work Lab")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
save_location <- function(user, label, address, display_name = NULL) {

  # get coordinates using geocode function
  coords <- geocode(address)

  # use preset title if no display name provided; NULL for custom labels
  if (is.null(display_name)) {
    display_name <- PRESET_TITLES[[label]]
  }

  # from the sheet get the whole location column
  existing <- read_sheet(sheet_id(), sheet = "locations")

  # don't select the row with the user and the provided label (if existent)
  existing <- existing[!(existing$user == user & existing$label == label), ]

  # create a new row based on the input
  new_row <- data.frame(
    user         = user,
    label        = label,
    address      = address,
    lat          = coords$lat,
    lon          = coords$lon,
    display_name = display_name
  )

  # rewrite sheet with new row included
  write_sheet(rbind(existing, new_row), ss = sheet_id(), sheet = "locations")

  # return coordinates
  return(coords)
}
