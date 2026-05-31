#' Plot rain intensity along a route
#'
#' Produces a ggplot2 area chart of rain intensity (mm/h) over cycling
#' distance. The y-axis is anchored to the defined severity thresholds, and
#' a red dashed line marks the user's tolerance level.
#'
#' @param route_rain_summary A data frame as returned in the
#'   \code{route_rain_summary} element of \code{\link{raintracker}}.
#' @param tolerance The user's rain tolerance: one of \code{"none"},
#'   \code{"light"}, \code{"moderate"}, or \code{"heavy"}. A red dashed
#'   threshold line is drawn at the corresponding intensity; no line is
#'   drawn for \code{"heavy"}.
#'
#' @return A ggplot object showing rain intensity over distance. Returns a
#'   blank plot when \code{route_rain_summary} is \code{NULL}.
#'
#' @examples
#' \dontrun{
#' route <- bikeroute(52.3731, 4.8922, 52.3579, 4.8686)
#' result <- raintracker(route$timed_coords, Sys.time())
#' plot_rain(result$route_rain_summary, tolerance = "moderate")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_hline labs
#' @importFrom ggplot2 scale_y_continuous coord_cartesian theme_classic
#' @importFrom ggplot2 theme_void
#' @export
plot_rain <- function(route_rain_summary, tolerance = "moderate") {

  if (is.null(route_rain_summary)) {
    return(ggplot(data.frame(), aes(x = 0, y = 0)) + theme_void())
  }

  p <- ggplot(route_rain_summary, aes(x = dist_km, y = rain_mm_h)) +
    geom_area(fill = "steelblue", alpha = 0.4) +
    geom_line(color = "steelblue") +
    scale_y_continuous(
      breaks = c(0, rain_thresholds[["light"]], rain_thresholds[["moderate"]]),
      labels = c("None", "Light", "Moderate")
    ) +
    # cap at 12 mm/h — headroom above the moderate threshold
    coord_cartesian(ylim = c(0, 12)) +
    labs(x = "Distance (km)", y = NULL) +
    theme_classic()

  # heavy tolerance users ride through anything — no threshold line
  if (tolerance != "heavy") {
    p <- p + geom_hline(yintercept = rain_thresholds[[tolerance]],
                        color = "red", linetype = "dashed", linewidth = 0.8)
  }

  p

}
