#' Log in or register a user
#'
#' Checks whether the username exists. If not, creates a new account. If so,
#' verifies the password. Returns a string describing the outcome so the caller
#' can respond appropriately.
#'
#' @param username A username string.
#' @param password A plain-text password.
#'
#' @return One of three character strings: \code{"created"} (new account made),
#'   \code{"authenticated"} (existing user, correct password), or
#'   \code{"wrong_password"} (existing user, wrong password).
#'
#' @importFrom googlesheets4 read_sheet write_sheet
#' @importFrom digest digest
#' @export
authenticate_user <- function(username, password) {

  # load user sheet
  users <- read_sheet(sheet_id(), sheet = "users")

  # encrypt password
  hash  <- digest(password, algo = "sha256")

  # create new user if username does not exist yet
  if (!username %in% users$username) {
    new_row <- data.frame(
      username        = username,
      password_hash   = hash,
      rain_preference = NA_character_
    )

    # add new username to sheet
    write_sheet(rbind(users, new_row), ss = sheet_id(), sheet = "users")
    return("created")
  }

  # check whether password matches actual password
  stored <- users$password_hash[users$username == username]
  if (stored == hash) {
    return("authenticated")
  } else {
    return("wrong_password")
  }

}
