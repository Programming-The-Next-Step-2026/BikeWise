#' Plan a cycling route between two coordinates
#'
#' Queries the OSRM cycling API for the route geometry, then computes travel
#' time and timed waypoints using Haversine distance and a constant speed.
#'
#' @param from_lat Latitude of your starting point (e.g. \code{52.3731}).
#' @param from_lon Longitude of your starting point (e.g. \code{4.8922}).
#' @param to_lat Latitude of your destination (e.g. \code{52.3579}).
#' @param to_lon Longitude of your destination (e.g. \code{4.8686}).
#' @param speed_kmh Assumed cycling speed in km/h used to estimate travel time
#'   (default: 15).
#'
#' @return A named list with three elements: \code{timed_coords} (the
#'   estimated position at each kilometre mark, as a data frame with columns
#'   \code{time_min}, \code{dist_km}, \code{lon}, and \code{lat}),
#'   \code{duration_min} (estimated cycling time in minutes), and
#'   \code{distance_km} (total route length in kilometres).
#'
#' @details Uses the public OSRM routing API with the cycling profile to find
#'   the route. Coordinates should be standard decimal latitude and longitude
#'   (e.g. \code{52.3731, 4.8922}). Travel time is estimated from the
#'   Haversine route distance divided by \code{speed_kmh}, independent of the
#'   OSRM duration. Estimated positions along the route are calculated by
#'   assuming constant speed and interpolating between waypoints.
#'
#' @examples
#' \donttest{
#' bikeroute(52.3731, 4.8922, 52.3579, 4.8686)
#' }
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom stats approx
#' @export
bikeroute <- function(from_lat, from_lon, to_lat, to_lon, speed_kmh = 15) {
  # matches raintracker's check_interval_km — lookups land on exact rows
  interval_km <- 1

  base_url <- "https://router.project-osrm.org/route/v1/bike"
  coord_str <- paste0(from_lon, ",", from_lat, ";", to_lon, ",", to_lat)

  resp <- request(paste0(base_url, "/", coord_str)) |>
    req_url_query(
      overview   = "full",
      geometries = "geojson",
      steps      = "false"
    ) |>
    req_perform()

  body <- resp |> resp_body_json()

  if (body$code != "Ok") {
    stop("OSRM error: ", body$code)
  }

  route       <- body$routes[[1]]
  coords_list <- route$geometry$coordinates

  coords_df <- data.frame(
    lon = sapply(coords_list, `[[`, 1),
    lat = sapply(coords_list, `[[`, 2)
  )

  # Haversine distance between each consecutive pair of waypoints
  n    <- nrow(coords_df)
  phi1 <- coords_df$lat[-n] * pi / 180
  phi2 <- coords_df$lat[-1] * pi / 180
  dphi <- phi2 - phi1
  dlam <- (coords_df$lon[-1] - coords_df$lon[-n]) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  seg_km <- c(0, 2 * 6371 * asin(sqrt(a)))  # arc length, Earth radius = 6371 km

  cum_km <- cumsum(seg_km)

  # both derived from Haversine — keeps duration and distance consistent
  distance_km  <- round(cum_km[n], 2)
  duration_min <- round(cum_km[n] / speed_kmh * 60, 1)

  # build km marks along the route, forcing the final point to be included
  dist_marks <- unique(c(seq(0, cum_km[n], by = interval_km), cum_km[n]))

  # derive time at each distance mark (constant speed assumption)
  time_at_d <- dist_marks / cum_km[n] * duration_min

  # deduplicate cum_km first — OSRM can return consecutive identical waypoints
  keep <- !duplicated(cum_km)
  timed_df <- data.frame(
    time_min = time_at_d,
    dist_km  = dist_marks,
    lon      = approx(cum_km[keep], coords_df$lon[keep], xout = dist_marks)$y,
    lat      = approx(cum_km[keep], coords_df$lat[keep], xout = dist_marks)$y
  )

  list(
    timed_coords = timed_df,
    duration_min = duration_min,
    distance_km  = distance_km
  )
}
