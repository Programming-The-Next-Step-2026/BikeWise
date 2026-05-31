#' Get or set the cycling speed preference for a user
#'
#' Retrieves or updates the cycling speed stored for a user. The speed is
#' used to estimate route travel time in \code{\link{bikeroute}}.
#'
#' @param username A character string identifying the user account.
#' @param speed_kmh If provided, updates the stored speed to this value (km/h).
#'   If omitted, the current speed is returned.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return The current speed as a numeric when called without \code{speed_kmh}.
#'   Called with \code{speed_kmh}, updates the value and returns \code{NULL}
#'   invisibly. Returns \code{NA} if no speed has been saved yet.
#'
#' @examples
#' \dontrun{
#' # get stored speed (returns NA if not yet set)
#' cycling_speed("alice", example = TRUE)
#'
#' # set speed to 20 km/h
#' cycling_speed("alice", speed_kmh = 20, example = TRUE)
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
cycling_speed <- function(username, speed_kmh = NULL, example = FALSE) {

  if (example) {

    users <- load_local_users()

    if (is.null(speed_kmh)) {
      users$cycling_speed[users$username == username]
    } else {
      users$cycling_speed[users$username == username] <- speed_kmh
      write.csv(users, local_users_path(), row.names = FALSE)
      invisible(NULL)
    }

  } else {

    users <- read_sheet(sheet_id(), sheet = "users")

    if (is.null(speed_kmh)) {
      # sheet stores speed encrypted as string — decrypt then coerce to numeric
      stored <- users$cycling_speed[users$username == username]
      as.numeric(decrypt_value(as.character(stored)))
    } else {
      users$cycling_speed[users$username == username] <-
        encrypt_value(speed_kmh)
      write_sheet(users, ss = sheet_id(), sheet = "users")
      invisible(NULL)
    }

  }
}
