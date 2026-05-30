#' Get or set the rain tolerance for a user
#'
#' @param username A username string.
#' @param tolerance If provided, updates the stored tolerance to this value.
#'   One of \code{"none"}, \code{"light"}, \code{"moderate"}, or
#'   \code{"heavy"}. If omitted, the current tolerance is returned.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return The current tolerance as a character string when called without
#'   \code{tolerance}. Called with \code{tolerance}, updates the value and
#'   returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' rain_tolerance("alice")
#' rain_tolerance("alice", "moderate")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @noRd
rain_tolerance <- function(username, tolerance = NULL, example = FALSE) {

  if (example) {

    users <- load_local_users()

    if (is.null(tolerance)) {
      # return current tolerance
      users$rain_tolerance[users$username == username]
    } else {
      # update tolerance and write back to CSV
      users$rain_tolerance[users$username == username] <- tolerance
      write.csv(users, local_users_path(), row.names = FALSE)
      invisible(NULL)
    }

  } else {

    users <- read_sheet(sheet_id(), sheet = "users")

    if (is.null(tolerance)) {
      # decrypt and return current tolerance
      decrypt_value(users$rain_tolerance[users$username == username])
    } else {
      # encrypt and write back to sheet
      users$rain_tolerance[users$username == username] <-
        encrypt_value(tolerance)
      write_sheet(users, ss = sheet_id(), sheet = "users")
      invisible(NULL)
    }

  }
}
