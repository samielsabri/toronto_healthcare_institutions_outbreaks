# Geocoding facilities with the City of Toronto's own address register
# ("Address Points (Municipal) - Toronto One Address Repository", Open
# Government Licence - Toronto). Matching is exact on the normalized address
# (the same normalize_address() used to define facilities), so it is
# reproducible and needs no external geocoding service or API key.

#' Parse '{"type": "Point", "coordinates": [lon, lat]}' strings.
parse_point_geometry <- function(geometry) {
  m <- stringr::str_match(geometry, "\\[\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)")
  tibble::tibble(lon = as.numeric(m[, 2]), lat = as.numeric(m[, 3]))
}

#' Read the address register, keeping one point per normalized address.
#'
#' An address can have several points (land parcel, building entrances); the
#' "Land" point is preferred, otherwise the points are averaged.
read_address_points <- function(path) {
  ap <- readr::read_csv(path, col_select = c("ADDRESS_POINT_ID", "ADDRESS_FULL", "ADDRESS_CLASS_DESC",
                                             "PLACE_NAME", "geometry"),
                        col_types = readr::cols(.default = "c"), progress = FALSE)
  ap <- dplyr::bind_cols(ap, parse_point_geometry(ap$geometry))
  ap$addr_key <- normalize_address(ap$ADDRESS_FULL)
  ap |>
    dplyr::filter(!is.na(.data$lon), !is.na(.data$lat)) |>
    dplyr::group_by(.data$addr_key) |>
    dplyr::summarise(
      n_points = dplyr::n(),
      address_point_id = dplyr::first(.data$ADDRESS_POINT_ID[order(.data$ADDRESS_CLASS_DESC != "Land")]),
      lon = if (any(.data$ADDRESS_CLASS_DESC == "Land")) .data$lon[.data$ADDRESS_CLASS_DESC == "Land"][1] else mean(.data$lon),
      lat = if (any(.data$ADDRESS_CLASS_DESC == "Land")) .data$lat[.data$ADDRESS_CLASS_DESC == "Land"][1] else mean(.data$lat),
      place_name = dplyr::first(stats::na.omit(dplyr::na_if(.data$PLACE_NAME, "None"))),
      .groups = "drop"
    )
}

#' Address key used to look up a facility address in the register.
#' A number range ("10-14 WILLIAM MORGAN DR") is looked up by its first number.
geocode_key <- function(facility_address) {
  key <- stringr::str_replace(facility_address, "^(\\d+[A-Z]?)-\\d+[A-Z]?\\s", "\\1 ")
  # The register abbreviates Terrace as "Ter" (our facility keys use "TERR")
  stringr::str_replace(key, "\\bTERR\\b", "TER")
}

#' Hand-checked register lookups for addresses written differently in the
#' outbreak data (e.g. "Queens Dr" for the register's "Queen's Dr").
load_geocode_manual <- function(path = "data/reference/geocode_manual.csv") {
  readr::read_csv(path, col_types = readr::cols(.default = "c"))
}

#' Match facility addresses to the register.
#' @return one row per facility address: lon, lat, address_point_id, match.
geocode_facilities <- function(facility_addresses, address_points, manual = NULL) {
  out <- tibble::tibble(facility_address = sort(unique(facility_addresses))) |>
    dplyr::mutate(addr_key = geocode_key(.data$facility_address)) |>
    dplyr::left_join(address_points, by = "addr_key") |>
    dplyr::mutate(match = dplyr::if_else(is.na(.data$lon), "unmatched", "exact"))
  if (!is.null(manual) && nrow(manual)) {
    # Hand-checked lookups: facility address -> the register address to use
    man <- manual |>
      dplyr::inner_join(address_points, by = c(register_address = "addr_key")) |>
      dplyr::select("facility_address", "lon", "lat", "address_point_id", "n_points", "place_name")
    idx <- match(out$facility_address, man$facility_address)
    hit <- !is.na(idx) & out$match == "unmatched"
    out[hit, c("lon", "lat", "address_point_id", "n_points", "place_name")] <-
      man[idx[hit], c("lon", "lat", "address_point_id", "n_points", "place_name")]
    out$match[hit] <- "manual"
  }
  out |> dplyr::select("facility_address", "lon", "lat", "match", "address_point_id",
                       register_place_name = "place_name")
}

#' Add the City neighbourhood (158-neighbourhood model) containing each point.
add_neighbourhood <- function(geo, neighbourhoods_sf) {
  pts <- sf::st_as_sf(geo[!is.na(geo$lon), ], coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  joined <- sf::st_join(pts, neighbourhoods_sf, join = sf::st_within) |> sf::st_drop_geometry()
  dplyr::left_join(geo, joined[, c("facility_address", "neighbourhood_id", "neighbourhood")],
                   by = "facility_address")
}
