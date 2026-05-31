#' Log in or register a user
#'
#' Checks whether the username exists in the user store. If not, creates a
#' new account. If so, verifies the password against the stored hash.
#'
#' @param username A character string identifying the user account.
#' @param password A plain-text password string.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCycling()}.
#'
#' @return One of three character strings: \code{"created"} (new account made),
#'   \code{"authenticated"} (existing user, correct password), or
#'   \code{"wrong_password"} (existing user, wrong password).
#'
#' @examples
#' \dontrun{
#' # First call with a new username creates the account
#' authenticate_user("alice", "secret", example = TRUE)  # "created"
#'
#' # Same credentials on a second call authenticate the user
#' authenticate_user("alice", "secret", example = TRUE)  # "authenticated"
#'
#' # Wrong password returns "wrong_password"
#' authenticate_user("alice", "badpass", example = TRUE) # "wrong_password"
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @importFrom digest digest
#' @export
authenticate_user <- function(username, password, example = FALSE) {

  # encrypt password — same regardless of backend
  hash <- digest(password, algo = "sha256")

  users <- if (example) {
    load_local_users()
  } else {
    read_sheet(sheet_id(), sheet = "users")
  }

  if (!username %in% users$username) {
    new_row <- data.frame(
      username       = username,
      password_hash  = hash,
      rain_tolerance = NA_character_,
      cycling_speed  = NA_real_
    )
    if (example) {
      write.csv(rbind(users, new_row), local_users_path(), row.names = FALSE)
    } else {
      write_sheet(rbind(users, new_row), ss = sheet_id(), sheet = "users")
    }
    return("created")
  }

  stored <- users$password_hash[users$username == username]
  if (stored == hash) "authenticated" else "wrong_password"

}
