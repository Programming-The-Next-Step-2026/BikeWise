# Convert mm/h to named rain severity using Buienradar thresholds
#' @noRd
classify_rain <- function(mm_h) {
  if (mm_h < rain_thresholds[["none"]]) {
    "none"
  } else if (mm_h < rain_thresholds[["light"]]) {
    "light"
  } else if (mm_h <= rain_thresholds[["moderate"]]) {
    "moderate"
  } else {
    "heavy"
  }
}

# Check if rain severity meets or exceeds the user's threshold
#' @noRd
exceeds_threshold <- function(level, threshold) {
  rain_order <- c(none = 0, light = 1, moderate = 2, heavy = 3)
  rain_order[[level]] > rain_order[[threshold]]
}

#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @noRd
fetch_rain_forecast <- function(lat, lon) {
  resp <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude      = round(lat, 4),
      longitude     = round(lon, 4),
      minutely_15   = "precipitation",
      forecast_days = 1,
      timezone      = "Europe/Amsterdam"
    ) |>
    req_perform()

  body  <- resp |> resp_body_json()
  times <- as.POSIXct(
    unlist(body$minutely_15$time),
    format = "%Y-%m-%dT%H:%M",
    tz     = "Europe/Amsterdam"
  )

  # Open-Meteo returns mm per 15-min interval; multiply by 4 for mm/h
  mm_h <- as.numeric(unlist(body$minutely_15$precipitation)) * 4

  data.frame(time = times, mm_h = mm_h)
}


#' Check whether it is safe to cycle now, or suggest when to leave
#'
#' @param timed_df Timed position data for your route, as returned in the
#'   \code{timed_coords} element of \code{bikeroute()}.
#' @param start_time When you plan to leave. Either a POSIXct datetime or a
#'   character string (e.g. \code{"2026-05-16 08:00"}).
#' @param threshold How much rain is too much to cycle in. One of
#'   \code{"light"}, \code{"moderate"}, or \code{"heavy"}. Defaults to
#'   \code{"moderate"}.
#' @return A named list with three elements: \code{safe_to_go} (TRUE if the
#'   route looks rain-free at the suggested departure time),
#'   \code{suggested_departure} (when to leave as a POSIXct, or NA if no dry
#'   window was found today), and \code{route_rain_summary} (a data frame with
#'   columns \code{time_min}, \code{dist_km}, \code{lon}, \code{lat},
#'   \code{rain_mm_h}, and \code{rain_level} — one row per checkpoint, showing
#'   conditions at \code{suggested_departure} when safe, or at
#'   \code{start_time} when no dry window was found).
#'
#' @details Checks the weather at regular intervals along your route using the
#'   free Open-Meteo API (no key required), which provides 15-minute
#'   precipitation forecasts over a 24-hour window. If rain is too heavy
#'   somewhere along the route, the function works out when the rain clears at
#'   that spot and shifts the suggested departure time forward accordingly,
#'   repeating until the full route is clear. If no dry window is found within
#'   24 hours, \code{suggested_departure} is returned as \code{NA}.
#'
#'
#' @examples
#' route <- bikeroute(52.3731, 4.8922, 52.3579, 4.8686)
#' raintracker(route$timed_coords, start_time = Sys.time())
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @export
raintracker <- function(timed_df,
                        start_time,
                        threshold = "moderate") {

  if (!inherits(start_time, "POSIXct")) {
    start_time <- as.POSIXct(start_time, tz = Sys.timezone())
  }

  # Scale interval to route length, targeting ~30 checkpoints; floor at 1 km
  check_interval_km <- max(1, max(timed_df$dist_km) / 30)

  # Build checkpoints: rows closest to each km multiple of check_interval_km,
  # always including the first and last row of timed_df
  max_time <- max(timed_df$time_min)

  # For each km mark along the route, find the row in timed_df closest to it
  km_marks <- seq(0, max(timed_df$dist_km), by = check_interval_km)
  idx <- sapply(km_marks,
               function(km) which.min(abs(timed_df$dist_km - km)))

  # Always include the final row so the end of the route is checked
  idx <- sort(unique(c(idx, nrow(timed_df))))

  # Index timed_df at determined checkpoints
  checkpoints <- timed_df[idx, ]

  # Pre-fetch forecasts for all checkpoints once; reused across retry attempts
  forecasts <- lapply(seq_len(nrow(checkpoints)), function(i) {
    fetch_rain_forecast(checkpoints$lat[i], checkpoints$lon[i])
  })

  # Snapshot of rain at start_time — used for the plot regardless of retry outcome
  initial_summary <- do.call(rbind, lapply(seq_len(nrow(checkpoints)), function(i) {
    cp       <- checkpoints[i, ]
    abs_time <- start_time + cp$time_min * 60
    forecast <- forecasts[[i]]
    diffs    <- abs(as.numeric(difftime(forecast$time, abs_time, units = "secs")))
    mm_h     <- forecast$mm_h[which.min(diffs)]
    data.frame(time_min  = cp$time_min, dist_km   = cp$dist_km,
               lon       = cp$lon,      lat       = cp$lat,
               rain_mm_h = round(mm_h, 2), rain_level = classify_rain(mm_h))
  }))

  current_start <- start_time

  # Check for rain or find next rainfree timepoint
  repeat {
    # Reset all rain indeces
    severity_log <- vector("list", nrow(checkpoints))
    rain_found   <- FALSE

    # Check rain at checkpoint at its appropriate time
    for (i in seq_len(nrow(checkpoints))) {
      # Choose checkpoint location and time
      cp         <- checkpoints[i, ]
      abs_time   <- current_start + cp$time_min * 60

      forecast <- forecasts[[i]]

      # Look at forecast closest to time at checkpoint
      diffs    <- abs(as.numeric(
        difftime(forecast$time, abs_time, units = "secs")
      ))
      mm_h  <- forecast$mm_h[which.min(diffs)]

      # Classify rain severity
      level <- classify_rain(mm_h)

      # Store result for severity summary (full dataframe created later)
      severity_log[[i]] <- data.frame(
        time_min   = cp$time_min,
        dist_km    = cp$dist_km,
        lon        = cp$lon,
        lat        = cp$lat,
        rain_mm_h  = round(mm_h, 2),
        rain_level = level
      )

      # Check whether rain is acceptable to the user, if not find new timeslot
      if (exceeds_threshold(level, threshold)) {
        # Too rainy mid-route: find when rain clears at this location
        rain_found <- TRUE
        future    <- forecast[forecast$time >= abs_time, ]
        safe_rows <- future[!sapply(
          future$mm_h,
          function(x) exceeds_threshold(classify_rain(x), threshold)
        ), ]

        # Adjust time if needed
        if (nrow(safe_rows) > 0) {
          # Shift departure by the delay until rain clears
          delay_secs    <- as.numeric(
            difftime(safe_rows$time[1], abs_time, units = "secs")
          )
          current_start <- current_start + delay_secs

        } else {
          # No safe slot in the 24-hour window: give up
          return(list(
            safe_to_go          = FALSE,
            suggested_departure = NA,
            route_rain_summary  = initial_summary
          ))
        }
        # Restart checkpoint loop with updated departure time
        break
      }
    }

    if (!rain_found) {
      # Full route is clear at current_start
      return(list(
        safe_to_go          = TRUE,
        suggested_departure = current_start,
        route_rain_summary  = do.call(rbind, severity_log)
      ))
    }
  }

}
