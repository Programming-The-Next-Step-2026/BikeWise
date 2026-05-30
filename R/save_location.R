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
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCyclingOnline()}.
#'
#' @return The saved coordinates as a named list with \code{lat} and
#'   \code{lon}.
#'
#' @details Geocodes the address and writes the result to the BikeWise Google
#'   Sheet, or to a local CSV when \code{example = TRUE}. If a location with
#'   the same label already exists for this user, it is overwritten. The
#'   Google Sheets backend requires \code{BIKEWISE_SHEET_ID} to be set and a
#'   valid Google account authenticated via \code{googlesheets4::gs4_auth()}.
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
save_location <- function(user, label, address, display_name = NULL,
                          example = FALSE) {

  # geocode the address
  coords <- geocode(address)

  # use preset title if no display name provided; NULL for custom labels
  if (is.null(display_name)) {
    display_name <- PRESET_TITLES[label]
  }

  # build the new row — same structure for both backends
  new_row <- data.frame(
    user         = user,
    label        = label,
    address      = address,
    lat          = coords$lat,
    lon          = coords$lon,
    display_name = display_name
  )

  if (example) {

    # load locations from local CSV and remove any existing row for this label
    existing <- load_local_locations()
    existing <- existing[!(existing$user == user & existing$label == label), ]
    write.csv(rbind(existing, new_row), local_locations_path(),
              row.names = FALSE)

  } else {

    # load from sheet; decrypt labels to find any existing row to overwrite
    existing <- read_sheet(sheet_id(), sheet = "locations")
    existing_labels <- vapply(
      as.character(existing$label), decrypt_value, character(1)
    )
    existing <- existing[
      !(existing$user == user & existing_labels == label), ]
    # encrypt sensitive fields before writing
    new_row_enc <- data.frame(
      user         = user,
      label        = encrypt_value(label),
      address      = encrypt_value(address),
      lat          = encrypt_value(coords$lat),
      lon          = encrypt_value(coords$lon),
      display_name = encrypt_value(as.character(display_name))
    )
    write_sheet(rbind(existing, new_row_enc),
                ss = sheet_id(), sheet = "locations")

  }

  # return coordinates
  coords
}
