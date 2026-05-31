#' Convert a street address to lat/lon using the Nominatim API.
#' @importFrom httr2 request req_url_query req_headers req_timeout req_perform
#' @importFrom httr2 resp_body_json
#' @noRd
geocode <- function(address) {

  results <- request("https://nominatim.openstreetmap.org/search") |>
    req_url_query(q = address, format = "json", limit = 1) |>
    # Nominatim blocks requests without a User-Agent header
    req_headers(`User-Agent` = "BikeWise R package") |>
    req_timeout(10) |>
    req_perform() |>
    resp_body_json()

  if (length(results) == 0) {
    stop("Address not found: \"", address, "\"")
  }

  list(
    lat = as.numeric(results[[1]]$lat),
    lon = as.numeric(results[[1]]$lon)
  )
}

#' Save a location for a user
#'
#' Geocodes the address via Nominatim and writes the coordinates to the user
#' store. If a location with the same label already exists for this user, it
#' is overwritten.
#'
#' @param user A character string identifying the user account.
#' @param label The name of the location. One of \code{"home"},
#'   \code{"work"}, \code{"education"}, \code{"friends"}, \code{"sports"},
#'   \code{"music"}, \code{"custom1"}, or \code{"custom2"}.
#' @param address A plain text address (e.g. \code{"Dam Square, Amsterdam"}).
#'   Geocoded automatically — no coordinates needed.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return The saved coordinates as a named list with \code{lat} and
#'   \code{lon}.
#'
#' @examples
#' \dontrun{
#' # save to local CSV — no authentication needed
#' save_location("alice", "home", "Keizersgracht 1, Amsterdam", example = TRUE)
#'
#' # save to Google Sheets
#' googlesheets4::gs4_auth()
#' save_location("alice", "home", "Keizersgracht 1, Amsterdam")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
save_location <- function(user, label, address, example = FALSE) {

  # fail early — invalid label creates an orphan entry the app cannot display
  if (!label %in% names(preset_titles)) {
    stop("label must be one of: ", paste(names(preset_titles), collapse = ", "))
  }

  coords <- geocode(address)

  # same structure for both backends
  new_row <- data.frame(
    user    = user,
    label   = label,
    address = address,
    lat     = coords$lat,
    lon     = coords$lon
  )

  if (example) {

    existing <- load_local_locations()
    existing <- existing[!(existing$user == user & existing$label == label), ]
    write.csv(rbind(existing, new_row), local_locations_path(),
              row.names = FALSE)

  } else {

    # decrypt stored labels — needed to find the existing row to replace
    existing <- read_sheet(sheet_id(), sheet = "locations")
    existing_labels <- vapply(
      as.character(existing$label), decrypt_value, character(1)
    )
    existing <- existing[!(existing$user == user & existing_labels == label), ]
    # label and address are personal data — encrypted at rest
    new_row_enc <- data.frame(
      user    = user,
      label   = encrypt_value(label),
      address = encrypt_value(address),
      lat     = encrypt_value(coords$lat),
      lon     = encrypt_value(coords$lon)
    )
    write_sheet(rbind(existing, new_row_enc),
                ss = sheet_id(), sheet = "locations")

  }

  coords
}
