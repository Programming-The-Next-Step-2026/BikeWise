#' Get or set the rain preference for a user
#'
#' @param username A username string.
#' @param preference If provided, updates the stored preference to this value.
#'   One of \code{"none"}, \code{"light"}, \code{"moderate"}, or
#'   \code{"heavy"}. If omitted, the current preference is returned.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{run_example()}.
#'
#' @return The current preference as a character string when called without
#'   \code{preference}. Called with \code{preference}, updates the value and
#'   returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' rain_preference("alice")
#' rain_preference("alice", "moderate")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @export
rain_preference <- function(username, preference = NULL, example = FALSE) {

  if (example) {

    users <- load_local_users()

    if (is.null(preference)) {
      # return current preference
      users$rain_preference[users$username == username]
    } else {
      # update preference and write back to CSV
      users$rain_preference[users$username == username] <- preference
      write.csv(users, local_users_path(), row.names = FALSE)
      invisible(NULL)
    }

  } else {

    users <- read_sheet(sheet_id(), sheet = "users")

    if (is.null(preference)) {
      # return current preference
      users$rain_preference[users$username == username]
    } else {
      # update preference and write back to sheet
      users$rain_preference[users$username == username] <- preference
      write_sheet(users, ss = sheet_id(), sheet = "users")
      invisible(NULL)
    }

  }
}
