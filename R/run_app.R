#' Launch the BikeWise Shiny app
#'
#' @return No return value. Starts the Shiny app in your browser.
#'
#' @examples
#' \dontrun{
#' run_bikewise()
#' }
#'
#' @importFrom shiny runApp
#' @export
run_bikewise <- function() {
  app_dir <- system.file("app", package = "BikeWise")
  runApp(app_dir)
}
