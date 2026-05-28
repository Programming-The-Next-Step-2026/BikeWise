#' Plot rain intensity along a route
#'
#' @param route_rain_summary A data frame as returned in the
#'   \code{route_rain_summary} element of \code{raintracker()}.
#' @param tolerance The user's rain tolerance: one of \code{"none"},
#'   \code{"light"}, \code{"moderate"}, or \code{"heavy"}. Draws a red
#'   threshold line at the corresponding rain level.
#'
#' @return A ggplot object showing rain intensity over distance, with a red
#'   line marking the user's tolerance threshold. Returns a blank plot when
#'   \code{route_rain_summary} is \code{NULL}.
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_hline labs
#' @importFrom ggplot2 scale_y_continuous coord_cartesian theme_classic theme_void
#' @export
plot_rain <- function(route_rain_summary, tolerance = "moderate") {

  # return blank plot if no rain data
  if (is.null(route_rain_summary)) {
    return(ggplot(data.frame(), aes(x = 0, y = 0)) + theme_void())
  }

  # build area chart with fixed y-axis and severity labels
  p <- ggplot(route_rain_summary, aes(x = dist_km, y = rain_mm_h)) +
    geom_area(fill = "steelblue", alpha = 0.4) +
    geom_line(color = "steelblue") +
    scale_y_continuous(
      breaks = c(0, RAIN_THRESHOLDS[["light"]], RAIN_THRESHOLDS[["moderate"]]),
      labels = c("None", "Light", "Moderate")
    ) +
    coord_cartesian(ylim = c(0, 12)) +
    labs(x = "Distance (km)", y = NULL) +
    theme_classic()

  # heavy tolerance users ride through anything — no threshold line
  if (tolerance != "heavy") {
    p <- p + geom_hline(yintercept = RAIN_THRESHOLDS[[tolerance]],
                        color = "red", linetype = "dashed", linewidth = 0.8)
  }

  p

}
