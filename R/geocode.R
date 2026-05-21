#' Convert a street address to coordinates
#'
#' @param address A plain text address, e.g. \code{"Dam Square, Amsterdam"} or
#'   \code{"Keizersgracht 1, 1015 Amsterdam"}. The more specific the address,
#'   the more reliable the result — including a city or country helps a lot.
#'
#' @return A named list with \code{lat} (latitude) and \code{lon} (longitude),
#'   ready to pass directly into \code{bikeroute()}.
#'
#' @details Uses the free Nominatim geocoding API from OpenStreetMap — no key
#'   required. If the address cannot be found, an informative error is returned
#'   so the user knows to try a more specific address.
#'
#' @examples
#' home <- geocode("Dam Square, Amsterdam")
#' work <- geocode("Science Park 904, Amsterdam")
#' bikeroute(home$lat, home$lon, work$lat, work$lon)
#'
#' @importFrom httr2 request req_url_query req_headers req_perform
#' @importFrom httr2 resp_body_json
#' @noRd
geocode <- function(address) {
  resp <- request("https://nominatim.openstreetmap.org/search") |>
    req_url_query(q = address, format = "json", limit = 1) |>
    req_headers(`User-Agent` = "BikeWise R package") |>
    req_perform()

  results <- resp |> resp_body_json()

  if (length(results) == 0) {
    stop(
      "Address not found: \"", address, "\". ",
      "Try adding more detail, such as the city or country."
    )
  }

  list(
    lat = as.numeric(results[[1]]$lat),
    lon = as.numeric(results[[1]]$lon)
  )
}
