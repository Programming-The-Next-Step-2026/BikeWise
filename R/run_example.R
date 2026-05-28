#' Run a local example of the BikeWise app
#'
#' Launches the BikeWise Shiny app using local CSV files instead of Google
#' Sheets. No Google account or internet connection is needed beyond the
#' weather and routing APIs. User data is stored in two CSV files in your
#' local application data folder.
#'
#' @return Called for its side effect; returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' run_example()
#' }
#'
#' @importFrom shiny runApp
#' @export
run_example <- function() {
  runApp(system.file("shiny-examples/bikewise", package = "BikeWise"))
}
