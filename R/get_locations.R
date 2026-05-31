#' Get saved locations for a user
#'
#' @param user A username string identifying the user.
#' @param example If \code{TRUE}, reads from a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCyclingOnline()}.
#'
#' @return A data frame with columns \code{user}, \code{label},
#'   \code{address}, \code{lat}, and \code{lon}.
#'
#' @examples
#' \dontrun{
#' googlesheets4::gs4_auth()
#' get_locations("alice")
#' }
#'
#' @importFrom googlesheets4 read_sheet
#' @export
get_locations <- function(user, example = FALSE) {

  # read from local CSV or Google Sheet depending on mode
  if (example) {
    data <- load_local_locations()
    return(data[data$user == user, ])
  }

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
  result_raw
}
