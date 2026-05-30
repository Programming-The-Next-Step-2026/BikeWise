#' Get or set the cycling speed preference for a user
#'
#' @param username A username string.
#' @param speed_kmh If provided, updates the stored speed to this value (km/h).
#'   If omitted, the current speed is returned.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCyclingOnline()}.
#'
#' @return The current speed as a numeric when called without \code{speed_kmh}.
#'   Called with \code{speed_kmh}, updates the value and returns \code{NULL}
#'   invisibly. Returns \code{NA} if no speed has been saved yet.
#'
#' @examples
#' \dontrun{
#' cycling_speed("alice")
#' cycling_speed("alice", 20)
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @noRd
cycling_speed <- function(username, speed_kmh = NULL, example = FALSE) {

  if (example) {

    users <- load_local_users()

    if (is.null(speed_kmh)) {
      # return current speed
      users$cycling_speed[users$username == username]
    } else {
      # update speed and write back to CSV
      users$cycling_speed[users$username == username] <- speed_kmh
      write.csv(users, local_users_path(), row.names = FALSE)
      invisible(NULL)
    }

  } else {

    users <- read_sheet(sheet_id(), sheet = "users")

    if (is.null(speed_kmh)) {
      # decrypt and return current speed as numeric
      stored <- users$cycling_speed[users$username == username]
      as.numeric(decrypt_value(as.character(stored)))
    } else {
      # encrypt and write back to sheet
      users$cycling_speed[users$username == username] <-
        encrypt_value(speed_kmh)
      write_sheet(users, ss = sheet_id(), sheet = "users")
      invisible(NULL)
    }

  }
}
