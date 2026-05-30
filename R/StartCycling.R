#' Launch the BikeWise app in offline mode
#'
#' Opens the BikeWise app in your default browser using local CSV files for
#' data storage. No Google account or environment variables are required.
#' An internet connection is still needed for the weather forecast and route
#' planning APIs.
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
