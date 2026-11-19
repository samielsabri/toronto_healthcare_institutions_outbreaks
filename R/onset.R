# Season-onset alerts (decision D10): the moving epidemic method (MEM) for
# influenza and RSV, with the mem package's default settings, fitted on the
# post-pandemic seasons (2022/23 to the last complete season). COVID-19 has no
# stable seasonal pattern yet, so it is shown against the post-pandemic range
# without a formal alert.
#
# An outbreak counts towards an agent if that agent was detected at all
# (decision D4). Counts are respiratory outbreaks in all in-scope settings.

ONSET_AGENTS <- c(influenza = "Influenza", rsv = "RSV", covid19 = "COVID-19")

agent_flag <- function(clean, agent) {
  switch(agent,
         influenza = clean$agent_influenza_a | clean$agent_influenza_b,
         rsv = clean$agent_rsv,
         covid19 = clean$agent_covid19,
         stop("Unknown onset agent: ", agent, call. = FALSE))
}

#' Weekly outbreaks involving one agent, by season and season week, with zeros.
onset_weekly <- function(clean, agent, current) {
  d <- clean[clean$in_scope & clean$outbreak_type == "Respiratory" & agent_flag(clean, agent), ]
  counts <- dplyr::count(d, .data$season_start_year, week = .data$season_week_52)
  starts <- seq(min(clean$season_start_year), current$season_start_year)
  tidyr::expand_grid(season_start_year = starts, week = 1:52) |>
    dplyr::filter(.data$season_start_year < current$season_start_year | .data$week <= current$season_week) |>
    dplyr::left_join(counts, by = c("season_start_year", "week")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L),
                  season = season_label(.data$season_start_year),
                  period = comparison_period(.data$season_start_year, .data$week))
}

#' Fit MEM on the post-pandemic seasons before the current one.
#' @return list(epidemic, post_epidemic, medium, high, very_high, seasons)
fit_mem_thresholds <- function(weekly, current) {
  train <- weekly |>
    dplyr::filter(.data$period == "post-pandemic", .data$season_start_year < current$season_start_year) |>
    dplyr::select("season", "week", "n") |>
    tidyr::pivot_wider(names_from = "season", values_from = "n") |>
    dplyr::arrange(.data$week)
  fit <- mem::memmodel(as.data.frame(train[, -1]))
  list(epidemic = unname(fit$epidemic.thresholds[1]),
       post_epidemic = unname(fit$epidemic.thresholds[2]),
       medium = unname(fit$intensity.thresholds[1]),
       high = unname(fit$intensity.thresholds[2]),
       very_high = unname(fit$intensity.thresholds[3]),
       seasons = names(train)[-1])
}

#' MEM intensity level of a weekly count.
intensity_level <- function(n, th) {
  as.character(cut(n, c(-Inf, th$epidemic, th$medium, th$high, th$very_high, Inf),
                   labels = c("Below epidemic threshold", "Low", "Medium", "High", "Very high"),
                   right = TRUE))
}

#' Current alert status from this season's complete weeks.
#'
#' The week containing the data-as-of date is incomplete (data to Wednesday),
#' so it is excluded. MEM's default rule: the season starts in the first week
#' whose count is above the epidemic threshold.
onset_status <- function(weekly, th, current) {
  cur <- weekly |>
    dplyr::filter(.data$season_start_year == current$season_start_year,
                  .data$week < current$season_week) |>
    dplyr::arrange(.data$week)
  above <- cur$n > th$epidemic
  first <- if (any(above)) cur$week[which(above)[1]] else NA_integer_
  run <- 0L
  for (a in rev(above)) if (a) run <- run + 1L else break
  latest <- utils::tail(cur, 1)
  list(
    started = !is.na(first),
    first_week = first,
    weeks_above_now = run,
    latest_week = latest$week,
    latest_n = latest$n,
    latest_level = intensity_level(latest$n, th)
  )
}

#' Short value-box value: the intensity of the latest complete week.
onset_status_value <- function(st) {
  if (!st$started) return("Not started")
  if (st$weeks_above_now == 0) return("Below threshold")
  sub("Below epidemic threshold", "Below threshold", st$latest_level)
}

#' Value-box title with the detail (first crossing, weeks above now).
onset_status_title <- function(st, label) {
  if (!st$started) return(paste(label, "season: below the epidemic threshold all season"))
  first <- sprintf("W%02d", iso_week_of_season_week(st$first_week))
  if (st$weeks_above_now == 0) {
    return(sprintf("%s season: first crossed the threshold in %s, now below it", label, first))
  }
  sprintf("%s season: above the threshold %d week%s running (first crossed in %s)",
          label, st$weeks_above_now, ifelse(st$weeks_above_now == 1, "", "s"), first)
}
