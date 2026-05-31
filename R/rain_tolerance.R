#' Get or set the rain tolerance for a user
#'
#' Retrieves or updates the rain tolerance stored for a user. The tolerance
#' controls whether a rain-free departure window is sought by
#' \code{\link{raintracker}}.
#'
#' @param username A character string identifying the user account.
#' @param tolerance If provided, updates the stored tolerance to this value.
#'   One of \code{"none"}, \code{"light"}, \code{"moderate"}, or
#'   \code{"heavy"}. If omitted, the current tolerance is returned.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return The current tolerance as a character string when called without
#'   \code{tolerance}. Returns \code{NA} if no tolerance has been saved yet.
#'   Called with \code{tolerance}, updates the value and returns \code{NULL}
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' # get stored tolerance (returns NA if not yet set)
#' rain_tolerance("alice", example = TRUE)
#'
#' # set tolerance to moderate
#' rain_tolerance("alice", "moderate", example = TRUE)
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
rain_tolerance <- function(username, tolerance = NULL, example = FALSE) {

  # fail early — invalid tolerance would cause a silent error in plot_rain
  valid <- c("none", "light", "moderate", "heavy")
  if (!is.null(tolerance) && !tolerance %in% valid) {
    stop("tolerance must be one of: ", paste(valid, collapse = ", "))
  }

  if (example) {

    users <- load_local_users()

    if (is.null(tolerance)) {
      users$rain_tolerance[users$username == username]
    } else {
      users$rain_tolerance[users$username == username] <- tolerance
      write.csv(users, local_users_path(), row.names = FALSE)
      invisible(NULL)
    }

  } else {

    users <- read_sheet(sheet_id(), sheet = "users")

    if (is.null(tolerance)) {
      # sheet stores tolerance encrypted — decrypt before returning
      decrypt_value(users$rain_tolerance[users$username == username])
    } else {
      users$rain_tolerance[users$username == username] <-
        encrypt_value(tolerance)
      write_sheet(users, ss = sheet_id(), sheet = "users")
      invisible(NULL)
    }

  }
}
