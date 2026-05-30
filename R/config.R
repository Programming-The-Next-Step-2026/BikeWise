# ── Constants ─────────────────────────────────────────────────────────────────

PRESET_TITLES <- c(
  home      = "Home",
  work      = "Work",
  education = "Education",
  friends   = "Friends",
  sports    = "Sports",
  music     = "Music",
  custom1   = "Custom 1",
  custom2   = "Custom 2"
)

# mm/h value at which each tolerance level is breached
RAIN_THRESHOLDS <- c(none = 0.1, light = 2.5, moderate = 10)

# ── Google Sheets backend ─────────────────────────────────────────────────────

#' @noRd
sheet_id <- function() {
  id <- Sys.getenv("BIKEWISE_SHEET_ID")
  if (nchar(id) == 0) stop("BIKEWISE_SHEET_ID is not set.")
  id
}

# ── Encryption ────────────────────────────────────────────────────────────────

# Encrypt a single value with AES-256-CBC using BIKEWISE_ENCRYPTION_KEY.
# Returns the value unchanged when the env var is not set or the value is NA.
#' @importFrom openssl sha256 rand_bytes aes_cbc_encrypt base64_encode
#' @noRd
encrypt_value <- function(x) {
  key_str <- Sys.getenv("BIKEWISE_ENCRYPTION_KEY")
  if (nchar(key_str) == 0 || is.na(x)) return(x)
  key <- sha256(charToRaw(key_str))
  iv  <- rand_bytes(16)
  ct  <- aes_cbc_encrypt(charToRaw(as.character(x)), key = key, iv = iv)
  base64_encode(c(iv, ct))
}

# Decrypt a value that was encrypted by encrypt_value().
# Returns the value unchanged when the env var is not set or the value is NA.
#' @importFrom openssl sha256 aes_cbc_decrypt base64_decode
#' @noRd
decrypt_value <- function(x) {
  key_str <- Sys.getenv("BIKEWISE_ENCRYPTION_KEY")
  if (nchar(key_str) == 0 || is.na(x)) return(x)
  key <- sha256(charToRaw(key_str))
  raw <- base64_decode(x)
  rawToChar(aes_cbc_decrypt(raw[17:length(raw)], key = key, iv = raw[1:16]))
}

# ── Local storage ─────────────────────────────────────────────────────────────

# Return (and create if needed) the folder where the local CSVs are stored
#' @importFrom tools R_user_dir
#' @noRd
local_store_dir <- function() {
  dir <- R_user_dir("BikeWise", which = "data")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

#' @noRd
local_users_path <- function() {
  file.path(local_store_dir(), "example_users.csv")
}

#' @noRd
local_locations_path <- function() {
  file.path(local_store_dir(), "example_locations.csv")
}

# Read the users CSV; create an empty one if it does not exist yet.
# Migrates old column names for existing installs.
#' @noRd
load_local_users <- function() {
  path <- local_users_path()
  if (!file.exists(path)) {
    empty <- data.frame(
      username      = character(),
      password_hash = character(),
      rain_tolerance = character(),
      cycling_speed  = numeric()
    )
    write.csv(empty, path, row.names = FALSE)
    return(empty)
  }
  users <- read.csv(path)
  # migrate rain_preference → rain_tolerance for existing installs
  if ("rain_preference" %in% names(users) &&
        !"rain_tolerance" %in% names(users)) {
    names(users)[names(users) == "rain_preference"] <- "rain_tolerance"
  }
  # add cycling_speed column if missing — handles existing installs
  if (!"cycling_speed" %in% names(users)) {
    users$cycling_speed <- NA_real_
  }
  users
}

# Read the locations CSV; create an empty one if it does not exist yet.
#' @noRd
load_local_locations <- function() {
  path <- local_locations_path()
  if (!file.exists(path)) {
    empty <- data.frame(
      user         = character(),
      label        = character(),
      address      = character(),
      lat          = numeric(),
      lon          = numeric(),
      display_name = character()
    )
    write.csv(empty, path, row.names = FALSE)
    return(empty)
  }
  read.csv(path)
}
