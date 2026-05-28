#' Launch the BikeWise Shiny app
#'
#' Launches the BikeWise Shiny app. Currently uses a local CSV as the data
#' backend (\code{example = TRUE}). A production backend (Supabase) will
#' replace this in a future version.
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
