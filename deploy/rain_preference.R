#' Get or set the rain preference for a user
#'
#' @param username A username string.
#' @param preference If provided, updates the stored preference to this value.
#'   One of \code{"none"}, \code{"light"}, \code{"moderate"}, or
#'   \code{"heavy"}. If omitted, the current preference is returned.
#'
#' @return The current preference as a character string when called without
#'   \code{preference}. Called with \code{preference}, updates the value and
#'   returns \code{NULL} invisibly.
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
rain_preference <- function(username, preference = NULL) {
  users <- read_sheet(sheet_id(), sheet = "users")

  if (is.null(preference)) {

    # if no preference is provided, must be in sheet
    users$rain_preference[users$username == username]

  } else {

    # if preference is provided, adopt
    users$rain_preference[users$username == username] <- preference
    write_sheet(users, ss = sheet_id(), sheet = "users")
  }
}
