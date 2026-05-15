#' Get a bike route between two coordinates
#'
#' @param from_lat Latitude of the starting point.
#' @param from_lon Longitude of the starting point.
#' @param to_lat Latitude of the destination.
#' @param to_lon Longitude of the destination.
#' @param interval_min Time interval in minutes between timed position
#'   snapshots. The start (t = 0) and end (t = total duration) are always
#'   included regardless of the interval. Defaults to 3.
#'
#' @return A list with four elements: \code{coordinates} (a data frame of all
#'   route waypoints with columns \code{lon} and \code{lat}),
#'   \code{timed_coords} (a data frame with columns \code{time_min},
#'   \code{lon}, and \code{lat} giving the estimated position at each interval,
#'   always including the first and last point), \code{duration_min} (estimated
#'   cycling time in minutes), and \code{distance_km} (route length in
#'   kilometres).
#'
#' @details Queries the public OSRM API using the bike profile. Coordinates
#'   must be in WGS84 decimal degrees. Timed positions are computed by
#'   interpolating along the route geometry assuming constant speed, using the
#'   Haversine formula to measure distances between waypoints.
#'
#' @examples
#' bikeroute(52.3731, 4.8922, 52.3579, 4.8686)
#' bikeroute(52.3731, 4.8922, 52.3579, 4.8686, interval_min = 5)
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @export
bikeroute <- function(from_lat, from_lon, to_lat, to_lon, interval_min = 3) {
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

  # Collect coordinates in df, cycling duration,and total distance
  route        <- body$routes[[1]]
  coords_list  <- route$geometry$coordinates
  duration_min <- round(route$duration / 60, 1)
  distance_km  <- round(route$distance / 1000, 2)

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


  # Build sequence of timestamps, forcing the final timestamp to be included
  timestamps <- unique(c(seq(0, duration_min, by = interval_min), duration_min))

  # Convert timestamps to distances along the route (constant speed)
  dist_at_t <- timestamps * (cum_km[n] / duration_min)

  # Linearly interpolate lon and lat at each target distance using approx()
  timed_df <- data.frame(
    time_min = timestamps,
    lon      = approx(cum_km, coords_df$lon, xout = dist_at_t)$y,
    lat      = approx(cum_km, coords_df$lat, xout = dist_at_t)$y
  )

  # Save coordinates, timed positions, duration and distance in list
  list(
    coordinates  = coords_df,
    timed_coords = timed_df,
    duration_min = duration_min,
    distance_km  = distance_km
  )
}
