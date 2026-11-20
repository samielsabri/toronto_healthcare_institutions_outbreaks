# Facility map for the current season (decision D11): one marker per facility
# with at least one outbreak this season, coloured by setting. Facilities with
# an outbreak open at the data-as-of date get a dark ring. Marker size is the
# same for every facility, so the map shows where outbreaks are, not a ranking
# (there are no bed-count denominators to adjust for facility size).

#' One row per facility with an outbreak in the current season.
facility_map_data <- function(clean, facilities, current) {
  this <- clean |>
    dplyr::filter(.data$in_scope, .data$season == current$season) |>
    add_episodes()
  this |>
    dplyr::group_by(.data$facility_id) |>
    dplyr::summarise(
      outbreaks = dplyr::n(),
      episodes = dplyr::n_distinct(.data$episode_id),
      open_now = sum(.data$is_open),
      latest_onset = max(.data$began),
      agents = paste(sort(unique(agent_mix_label(.data$agent_mix_group))), collapse = ", "),
      types = paste(sort(unique(.data$outbreak_type)), collapse = ", "),
      units_open = paste(unique(.data$institution_name[.data$is_open]), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::inner_join(
      dplyr::select(facilities, "facility_id", "facility_name", "facility_address", "setting_group",
                    "lon", "lat", "neighbourhood"),
      by = "facility_id") |>
    dplyr::filter(!is.na(.data$lon)) |>
    dplyr::mutate(setting_group = factor(.data$setting_group, levels = names(SETTING_COLOURS))) |>
    dplyr::arrange(.data$open_now > 0)  # open-now markers drawn last (on top)
}

#' Popup HTML for each facility (all text escaped).
facility_popup <- function(d) {
  esc <- htmltools::htmlEscape
  paste0(
    "<div style='min-width:220px;font-family:system-ui,sans-serif;font-size:12px;'>",
    "<b style='font-size:13px;'>", esc(d$facility_name), "</b><br>",
    "<span style='color:#52514e;'>", esc(as.character(d$setting_group)), " &middot; ",
    esc(stringr::str_to_title(d$facility_address)), "<br>", esc(d$neighbourhood), "</span>",
    "<hr style='margin:5px 0;'>",
    "<table style='width:100%;'>",
    "<tr><td>Outbreaks this season</td><td style='text-align:right;'><b>", d$outbreaks, "</b></td></tr>",
    "<tr><td>Separate episodes</td><td style='text-align:right;'><b>", d$episodes, "</b></td></tr>",
    "<tr><td>Open now</td><td style='text-align:right;'><b>", d$open_now, "</b></td></tr>",
    "<tr><td>Most recent onset</td><td style='text-align:right;'>", format(d$latest_onset, "%e %b %Y"), "</td></tr>",
    "</table>",
    "<div style='margin-top:4px;color:#52514e;'>Type: ", esc(d$types), "<br>Pathogens: ", esc(d$agents), "</div>",
    ifelse(d$open_now > 0,
           paste0("<div style='margin-top:4px;'><b>Open:</b> ", esc(d$units_open), "</div>"), ""),
    "</div>"
  )
}

#' Leaflet map of facilities with an outbreak this season.
facility_map <- function(d) {
  pal <- leaflet::colorFactor(unname(SETTING_COLOURS), levels = names(SETTING_COLOURS))
  m <- leaflet::leaflet(d, options = leaflet::leafletOptions(minZoom = 9)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(lng = -79.38, lat = 43.71, zoom = 11)
  for (s in names(SETTING_COLOURS)) {
    ds <- d[d$setting_group == s, ]
    if (!nrow(ds)) next
    m <- m |>
      leaflet::addCircleMarkers(
        data = ds, lng = ~lon, lat = ~lat, group = s,
        radius = 7, fillColor = ~pal(setting_group), fillOpacity = 0.9,
        color = ifelse(ds$open_now > 0, "#0b0b0b", "#fcfcfb"),
        weight = ifelse(ds$open_now > 0, 3, 1.5), opacity = 1,
        label = ~facility_name, popup = facility_popup(ds)
      )
  }
  m |>
    leaflet::addLegend("bottomright", colors = unname(SETTING_COLOURS), labels = names(SETTING_COLOURS),
                       title = "Setting", opacity = 0.9) |>
    leaflet::addLegend("bottomright", colors = "#fcfcfb", labels = "Outbreak open now (dark ring)",
                       opacity = 1, className = "info legend legend-open") |>
    leaflet::addLayersControl(overlayGroups = names(SETTING_COLOURS),
                              options = leaflet::layersControlOptions(collapsed = FALSE),
                              position = "topright")
}
