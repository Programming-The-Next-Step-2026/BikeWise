#' Log in or register a user
#'
#' Checks whether the username exists. If not, creates a new account. If so,
#' verifies the password. Returns a string describing the outcome to communicate
#' with shiny or caller.
#'
#' @param username A username string.
#' @param password A plain-text password.
#' @param example If \code{TRUE}, reads and writes a local CSV file instead of
#'   Google Sheets. Used automatically by \code{StartCyclingOnline()}.
#'
#' @return One of three character strings: \code{"created"} (new account made),
#'   \code{"authenticated"} (existing user, correct password), or
#'   \code{"wrong_password"} (existing user, wrong password).
#'
#' @examples
#' \dontrun{
#' authenticate_user("alice", "secret")
#' }
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @importFrom digest digest
#' @export
authenticate_user <- function(username, password, example = FALSE) {

  # encrypt password — same regardless of backend
  hash <- digest(password, algo = "sha256")

  if (example) {

    # load users from local CSV
    users <- load_local_users()

    # create new user if username does not exist yet
    if (!username %in% users$username) {
      new_row <- data.frame(
        username        = username,
        password_hash   = hash,
        rain_tolerance = NA_character_,
        cycling_speed   = NA_real_
      )
      write.csv(rbind(users, new_row), local_users_path(), row.names = FALSE)
      return("created")
    }

    # check whether password matches stored hash
    stored <- users$password_hash[users$username == username]
    if (stored == hash) "authenticated" else "wrong_password"

  } else {

    # load user sheet
    users <- read_sheet(sheet_id(), sheet = "users")

    # create new user if username does not exist yet
    if (!username %in% users$username) {
      new_row <- data.frame(
        username        = username,
        password_hash   = hash,
        rain_tolerance = NA_character_,
        cycling_speed   = NA_real_
      )
      write_sheet(rbind(users, new_row), ss = sheet_id(), sheet = "users")
      return("created")
    }

    # check whether password matches stored hash
    stored <- users$password_hash[users$username == username]
    if (stored == hash) "authenticated" else "wrong_password"

  }

}
