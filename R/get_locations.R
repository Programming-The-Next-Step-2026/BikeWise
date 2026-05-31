#' Get saved locations for a user
#'
#' Returns the saved location records for a user from the backend store.
#' On the Google Sheets backend, all fields are decrypted before returning.
#'
#' @param user A character string identifying the user account.
#' @param example If \code{TRUE}, reads from a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return A data frame with columns \code{user}, \code{label},
#'   \code{address}, \code{lat}, and \code{lon}.
#'
#' @examples
#' \dontrun{
#' # returns all saved locations for the user
#' get_locations("alice", example = TRUE)
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @export
get_locations <- function(user, example = FALSE) {

  if (example) {
    locs <- load_local_locations()
    return(locs[locs$user == user, ])
  }

  locs <- read_sheet(sheet_id(), sheet = "locations")
  locs <- locs[locs$user == user, ]
  locs$label   <- vapply(locs$label,   decrypt_value, character(1))
  locs$address <- vapply(locs$address, decrypt_value, character(1))
  # lat and lon come back as numeric when unencrypted — coerce before vapply
  locs$lat <- as.numeric(vapply(
    as.character(locs$lat), decrypt_value, character(1)
  ))
  locs$lon <- as.numeric(vapply(
    as.character(locs$lon), decrypt_value, character(1)
  ))
  locs
}
