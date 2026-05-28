#' Launch the BikeWise Shiny app
#'
#' Opens the BikeWise app in your default browser. User accounts and saved
#' locations are stored in local CSV files on your machine — no Google account
#' is required. An internet connection is needed for the weather forecast and
#' route planning APIs.
#'
#' @return Called for its side effect; returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' StartCycling()
#' }
#'
#' @importFrom shiny runApp
#' @export
StartCycling <- function() {
  runApp(system.file("app", package = "BikeWise"))
}
