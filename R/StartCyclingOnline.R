#' Launch the BikeWise app with Google Sheets backend
#'
#' Launches the BikeWise Shiny app using Google Sheets for data storage.
#' All sensitive data is encrypted before being written to the sheet.
#' Requires three environment variables: \code{BIKEWISE_SHEET_ID},
#' \code{BIKEWISE_SERVICE_ACCOUNT}, and \code{BIKEWISE_ENCRYPTION_KEY}.
#'
#' @details
#' For full setup instructions — including how to create the Google Sheet,
#' set up a service account, generate an encryption key, and deploy to
#' shinyapps.io — run:
#' \preformatted{vignette("online-setup", package = "BikeWise")}
#'
#' @return Called for its side effect; returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' StartCyclingOnline()
#' }
#'
#' @importFrom shiny runApp
#' @importFrom googlesheets4 gs4_auth
#' @export
StartCyclingOnline <- function() {

  # check required env vars are set
  missing <- c(
    if (nchar(Sys.getenv("BIKEWISE_SHEET_ID")) == 0)
      "BIKEWISE_SHEET_ID",
    if (nchar(Sys.getenv("BIKEWISE_SERVICE_ACCOUNT")) == 0)
      "BIKEWISE_SERVICE_ACCOUNT",
    if (nchar(Sys.getenv("BIKEWISE_ENCRYPTION_KEY")) == 0)
      "BIKEWISE_ENCRYPTION_KEY"
  )
  if (length(missing) > 0) {
    stop(
      "The following environment variables are not set: ",
      paste(missing, collapse = ", "), ".\n",
      "Add them to your .Renviron file and restart R.\n",
      "See ?StartCyclingOnline for setup instructions."
    )
  }

  # authenticate with Google Sheets using service account
  gs4_auth(path = Sys.getenv("BIKEWISE_SERVICE_ACCOUNT"))

  runApp(system.file("shiny-examples/bikewise", package = "BikeWise"))
}
