#' Plan a cycling route between two coordinates
#'
#' @param from_lat Latitude of your starting point (e.g. \code{52.3731}).
#' @param from_lon Longitude of your starting point (e.g. \code{4.8922}).
#' @param to_lat Latitude of your destination.
#' @param to_lon Longitude of your destination.
#' @param speed_kmh Assumed cycling speed in km/h used to estimate travel time
#'   (default: 15).
#'
#' @return A named list with three elements: \code{timed_coords} (your
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
#' bikeroute(52.3731, 4.8922, 52.3579, 4.8686)
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom stats approx
#' @export
bikeroute <- function(from_lat, from_lon, to_lat, to_lon, speed_kmh = 15) {
  # matches raintracker's default check_interval_km so lookups land exactly on rows
  interval_km <- 1

  base_url <- "https://router.project-osrm.org/route/v1/bike"
  coords   <- paste0(from_lon, ",", from_lat, ";", to_lon, ",", to_lat)

  # HTTP request to OSRM API
  resp <- request(paste0(base_url, "/", coords)) |>
    req_url_query(
      overview   = "full",
      geometries = "geojson",
      steps      = "false"
    ) |>
    req_perform()

  # Returns a JSON object that is then transformed into an R list object
  body <- resp |> resp_body_json()

  # Returns an error if no route exists between two points
  if (body$code != "Ok") {
    stop("OSRM error: ", body$code)
  }

  # Collect coordinates in df
  route        <- body$routes[[1]]
  coords_list  <- route$geometry$coordinates

  coords_df <- data.frame(
    lon = sapply(coords_list, `[[`, 1),
    lat = sapply(coords_list, `[[`, 2)
  )


  # Compute cumulative distance (km) along the route using Haversine.
  # Determine seg_km[i]; the great-circle distance between waypoint i-1 and i.
  n      <- nrow(coords_df)
  seg_km <- numeric(n)

  # skip i=1: no previous point to measure from
  for (i in 2:n) {

    # convert previous and current lat to radians
    phi1      <- coords_df$lat[i - 1] * pi / 180
    phi2      <- coords_df$lat[i]     * pi / 180

    # lat and lon differences in radians
    dphi      <- phi2 - phi1
    dlam      <- (coords_df$lon[i]   - coords_df$lon[i - 1]) * pi / 180

    # Haversine intermediate value
    a         <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2

    # arc length in km, Earth radius = 6371 km
    seg_km[i] <- 2 * 6371 * asin(sqrt(a))
  }

  # cumsum() turns per-segment distances into a running total from the start.
  cum_km <- cumsum(seg_km)

  # Both time and distance derived from our own Haversine calculation for consistency
  distance_km  <- round(cum_km[n], 2)
  duration_min <- round(cum_km[n] / speed_kmh * 60, 1)

  # Build km marks along the route, forcing the final point to be included
  dist_marks <- unique(c(seq(0, cum_km[n], by = interval_km), cum_km[n]))

  # Derive time at each distance mark (constant speed assumption)
  time_at_d <- dist_marks / cum_km[n] * duration_min

  # Linearly interpolate lon and lat at each target distance using approx()
  # Deduplicate cum_km first — OSRM can return consecutive identical waypoints
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
