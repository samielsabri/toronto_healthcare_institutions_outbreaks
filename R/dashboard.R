# Builds the dashboard: one self-contained HTML file with the data, Chart.js
# and Leaflet inlined (dashboard/template.html), in the same style as the
# project's other surveillance dashboards.
#
# `assemble_dashboard_data()` collects everything the page shows into one
# list; all statistics (cleaning, comparison periods, MEM thresholds,
# Kaplan-Meier curves, episodes, recurrence) are computed here in R, where
# they are tested. The page only filters, counts and draws.

DASHBOARD_TEMPLATE <- "dashboard/template.html"

# One fixed, validated categorical palette (colour-blind safe on adjacent
# pairs); colours follow the entity, so COVID-19 is the same blue everywhere.
SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
UNKNOWN_COLOUR <- "#898781"
AGENT_COLOURS <- stats::setNames(c(SERIES_COLOURS, UNKNOWN_COLOUR), AGENT_MIX_LEVELS)
SETTING_COLOURS <- stats::setNames(SERIES_COLOURS[1:3], c("LTCH", "Retirement home", "Hospital"))
SETTING_CODES <- c("LTCH", "Retirement home", "Hospital")
TYPE_CODES <- c("Respiratory", "Enteric", "Other")
PERIOD_CODES <- c("partial season" = "part", "pre-pandemic" = "pre", "pandemic" = "pan",
                  "post-pandemic" = "post")

#' Monday of an ISO week.
iso_week_monday <- function(iso_year, iso_week) {
  jan4 <- as.Date(sprintf("%d-01-04", iso_year))
  jan4 - (lubridate::wday(jan4, week_start = 1) - 1) + 7 * (iso_week - 1)
}

#' For each season and season week (1-52, week 53 merged into 52 as in D7):
#' first and last day, ISO week label and comparison period.
season_week_calendar <- function(start_years) {
  purrr::map_dfr(start_years, function(sy) {
    n53 <- iso_weeks_in_year(sy) == 53
    tibble::tibble(season_start_year = sy, week = 1:52) |>
      dplyr::mutate(
        iso_year = ifelse(.data$week <= 18, sy, sy + 1L),
        iso_week = iso_week_of_season_week(.data$week),
        start = iso_week_monday(.data$iso_year, .data$iso_week),
        # week 18 absorbs ISO week 53 in 53-week seasons, so it spans 14 days
        end = .data$start + ifelse(n53 & .data$week == 18, 13, 6),
        period = comparison_period(sy, .data$week)
      )
  })
}

#' Kaplan-Meier curve points (up to `max_days`) and median per group.
km_curves <- function(data, group, colours, max_days = 60) {
  fit <- survival::survfit(stats::as.formula(paste("survival::Surv(duration_days, event) ~", group)),
                           data = data)
  med <- km_medians(data, group)
  lapply(names(colours)[names(colours) %in% med$group], function(g) {
    s <- summary(fit[which(sub("^[^=]+=", "", names(fit$strata)) == g)], times = 0:max_days, extend = TRUE)
    m <- med[med$group == g, ]
    list(name = g, colour = unname(colours[[g]]), n = m$outbreaks, open = m$still_open,
         median = m$median_days, lo = m$ci_low, hi = m$ci_high,
         t = s$time, s = round(s$surv, 4))
  })
}

#' Everything the dashboard shows, as one JSON-able list.
assemble_dashboard_data <- function(clean, facilities, info) {
  current <- current_season_info(info$as_of)
  d <- clean |>
    dplyr::filter(.data$in_scope) |>
    add_episodes() |>
    dplyr::arrange(.data$began, .data$institution_name)

  starts <- seq(min(d$season_start_year), current$season_start_year)
  seasons <- season_label(starts)
  cal <- season_week_calendar(starts)
  cal_by_season <- split(cal, cal$season_start_year)

  fac <- facilities |>
    dplyr::filter(.data$facility_id %in% d$facility_id) |>
    dplyr::arrange(.data$facility_id)
  fac_idx <- match(d$facility_id, fac$facility_id) - 1L

  agent_cols <- paste0("agent_", names(AGENT_GROUPS))
  flags <- Reduce(`+`, lapply(seq_along(agent_cols), function(i) as.integer(d[[agent_cols[i]]]) * 2L^(i - 1)))
  ep_idx <- match(d$episode_id, unique(d$episode_id)) - 1L

  # Season onset (decision D10)
  onset <- lapply(c(influenza = "influenza", rsv = "rsv"), function(a) {
    w <- onset_weekly(clean, a, current)
    th <- fit_mem_thresholds(w, current)
    st <- onset_status(w, th, current)
    list(thresholds = th[c("epidemic", "medium", "high", "very_high")], seasons = th$seasons,
         status = list(value = onset_status_value(st), title = onset_status_title(st, ONSET_AGENTS[[a]]),
                       started = st$started,
                       first_week = if (st$started) st$first_week else NULL))
  })

  # Duration (Kaplan-Meier, post-pandemic seasons)
  dur <- duration_data(clean)
  resp <- dplyr::filter(dur, .data$outbreak_type == "Respiratory")
  ent <- dplyr::filter(dur, .data$outbreak_type == "Enteric")
  agent_km <- AGENT_COLOURS[c("COVID-19", "Influenza", "RSV", "Rhinovirus/enterovirus",
                              "Other respiratory virus", "Unknown")]
  km <- list(
    resp_setting = km_curves(resp, "setting", SETTING_COLOURS),
    resp_agent = km_curves(dplyr::filter(resp, .data$agent %in% names(agent_km)) |> droplevels(),
                           "agent", agent_km),
    ent_setting = km_curves(ent, "setting", SETTING_COLOURS)
  )

  # Recurrence (decision D9), complete seasons only
  rs <- repeat_share(facility_season_counts(clean)) |>
    dplyr::filter(.data$season %in% season_label(2016:(current$season_start_year - 1L)))

  list(
    built = format(Sys.Date()),
    as_of = format(info$as_of),
    refreshed = format(info$refreshed, "%Y-%m-%d"),
    current = list(season = current$season, season_idx = match(current$season, seasons) - 1L,
                   week = current$season_week),
    seasons = seasons,
    calendar = lapply(cal_by_season, function(x) list(
      start = format(x$start), end = format(x$end), iso = x$iso_week,
      period = unname(PERIOD_CODES[x$period]))) |> unname(),
    settings = SETTING_CODES,
    setting_colours = unname(SETTING_COLOURS[SETTING_CODES]),
    types = TYPE_CODES,
    agent_groups = unname(AGENT_GROUPS),
    mix_levels = AGENT_MIX_LEVELS,
    mix_colours = unname(AGENT_COLOURS[AGENT_MIX_LEVELS]),
    facilities = list(
      id = fac$facility_id, name = fac$facility_name,
      address = stringr::str_to_title(fac$facility_address),
      setting = match(fac$setting_group, SETTING_CODES) - 1L,
      lon = round(fac$lon, 5), lat = round(fac$lat, 5), hood = fac$neighbourhood
    ),
    outbreaks = list(
      fac = fac_idx,
      unit = d$institution_name,
      setting = match(d$setting_group, SETTING_CODES) - 1L,
      type = match(d$outbreak_type, TYPE_CODES) - 1L,
      mix = match(agent_mix_label(d$agent_mix_group), AGENT_MIX_LEVELS) - 1L,
      flags = flags,
      began = format(d$began),
      over = ifelse(is.na(d$declared_over), NA_character_, format(d$declared_over)),
      season = match(d$season, seasons) - 1L,
      week = d$season_week_52,
      episode = ep_idx,
      a1 = d$agent1_raw, a2 = d$agent2_raw
    ),
    onset = onset,
    km = km,
    recurrence = list(season = rs$season, setting = as.character(rs$setting),
                      facilities = rs$facilities, repeats = rs$repeat_facilities)
  )
}

#' Basemap tiles: CARTO Positron with the key from CARTO_API_KEY (a GitHub
#' Actions secret, never committed), otherwise OpenStreetMap's standard tiles.
basemap_tiles <- function(key = Sys.getenv("CARTO_API_KEY")) {
  if (nzchar(key)) {
    list(url = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=",
                      utils::URLencode(key, reserved = TRUE)),
         attribution = paste('&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
                             'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'))
  } else {
    list(url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
         attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors')
  }
}

#' JSON for inlining in a <script>: "</" is escaped so the data can never
#' close the script element early.
to_inline_json <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  gsub("</", "<\\/", json, fixed = TRUE)
}

#' Replace the first occurrence of `placeholder` by `value`, literally (no
#' regex or backslash processing: the inlined libraries contain backslashes).
replace_once <- function(text, placeholder, value) {
  pos <- regexpr(placeholder, text, fixed = TRUE)
  if (pos < 0) stop("Template placeholder missing: ", placeholder, call. = FALSE)
  paste0(substr(text, 1, pos - 1), value,
         substr(text, pos + attr(pos, "match.length"), nchar(text)))
}

#' Write the self-contained dashboard HTML.
render_dashboard <- function(data, out = "_site/index.html", template = DASHBOARD_TEMPLATE,
                             vendor = "dashboard/vendor", tiles = basemap_tiles()) {
  read <- function(f) paste(readLines(file.path(vendor, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- paste(readLines(template, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  repl <- list(
    "/*__CHARTJS__*/" = gsub("</script", "<\\/script", read("chart.umd.min.js"), fixed = TRUE),
    "/*__LEAFLETJS__*/" = gsub("</script", "<\\/script", read("leaflet.js"), fixed = TRUE),
    "/*__LEAFLETCSS__*/" = read("leaflet.css"),
    "/*__TILES__*/null" = to_inline_json(tiles),
    "/*__DATA__*/null" = to_inline_json(data)
  )
  for (k in names(repl)) html <- replace_once(html, k, repl[[k]])
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, out, useBytes = TRUE)
  invisible(out)
}
