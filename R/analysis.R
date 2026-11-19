# Analysis functions behind the dashboard. Each returns a tidy table; the
# plotting code in R/plots.R only draws them. All counts are of outbreaks as
# published (unit level), by week of onset, in-scope settings only.

SETTING_LEVELS <- c("All settings", "LTCH", "Retirement home", "Hospital")

#' The current season and week, from the data-as-of date.
current_season_info <- function(as_of) {
  list(
    season_start_year = season_start_year(as_of),
    season = season_label(season_start_year(as_of)),
    season_week = season_week_52(as_of),
    as_of = as.Date(as_of)
  )
}

#' Weekly counts of new outbreaks by season, season week (1-52), setting and
#' outbreak type, with explicit zeros. "All settings" is added as a setting.
#'
#' Weeks are filled with zeros only where the season was observed: every week
#' for past seasons, and up to the current week for the current season.
weekly_counts <- function(clean, current) {
  d <- dplyr::filter(clean, .data$in_scope, .data$outbreak_type %in% c("Respiratory", "Enteric"))
  d <- dplyr::bind_rows(d, dplyr::mutate(d, setting_group = "All settings"))
  counts <- dplyr::count(d, .data$season, .data$season_start_year, week = .data$season_week_52,
                         setting = .data$setting_group, type = .data$outbreak_type)
  # Every season from the first in the data to the current one, so a season
  # with no outbreaks at all still contributes zeros.
  starts <- seq(min(clean$season_start_year), current$season_start_year)
  seasons <- tibble::tibble(season = season_label(starts), season_start_year = starts)
  grid <- tidyr::expand_grid(seasons, week = 1:52, setting = SETTING_LEVELS,
                             type = c("Respiratory", "Enteric")) |>
    dplyr::filter(.data$season_start_year < current$season_start_year | .data$week <= current$season_week)
  grid |>
    dplyr::left_join(counts, by = c("season", "season_start_year", "week", "setting", "type")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L),
                  period = comparison_period(.data$season_start_year, .data$week),
                  setting = factor(.data$setting, levels = SETTING_LEVELS))
}

#' Range of weekly counts across the seasons of a comparison period.
#'
#' For each week: min, median and max over the seasons observed in that week,
#' and how many seasons contributed. The current season never contributes.
#' Pre-pandemic: 2016/17-2018/19 plus 2019/20 to season week 28 (decision D6),
#' so weeks 1-28 draw on 4 seasons and weeks 29-52 on 3.
baseline_range <- function(weekly, period_name, current) {
  weekly |>
    dplyr::filter(.data$period == period_name,
                  .data$season_start_year < current$season_start_year) |>
    dplyr::group_by(.data$week, .data$setting, .data$type) |>
    dplyr::summarise(lo = min(.data$n), median = stats::median(.data$n), hi = max(.data$n),
                     n_seasons = dplyr::n(),
                     seasons = paste(range(.data$season), collapse = " to "),
                     .groups = "drop") |>
    dplyr::mutate(baseline = period_name)
}

#' Display groups for the agent-mix chart (at most 6 colours + Unknown).
AGENT_MIX_LEVELS <- c("COVID-19", "Influenza", "RSV", "Rhinovirus/enterovirus",
                      "Other respiratory virus", "Multiple agents", "Unknown")

agent_mix_label <- function(agent_mix_group) {
  dplyr::case_when(
    agent_mix_group == "covid19" ~ "COVID-19",
    agent_mix_group %in% c("influenza_a", "influenza_b") ~ "Influenza",
    agent_mix_group == "rsv" ~ "RSV",
    agent_mix_group == "rhino_entero" ~ "Rhinovirus/enterovirus",
    agent_mix_group == "multiple" ~ "Multiple agents",
    agent_mix_group == "unknown" ~ "Unknown",
    TRUE ~ "Other respiratory virus"  # seasonal coronavirus, parainfluenza, hMPV, other
  )
}

#' Weekly respiratory outbreaks in a season by agent display group, with zeros.
agent_mix_weekly <- function(clean, season, max_week = 52) {
  clean |>
    dplyr::filter(.data$in_scope, .data$outbreak_type == "Respiratory", .data$season == !!season) |>
    dplyr::mutate(agent = factor(agent_mix_label(.data$agent_mix_group), levels = AGENT_MIX_LEVELS)) |>
    dplyr::count(week = .data$season_week_52, .data$agent, .drop = FALSE) |>
    tidyr::complete(week = 1:max_week, .data$agent, fill = list(n = 0L)) |>
    dplyr::filter(.data$week <= max_week)
}

#' Share of respiratory outbreaks by agent display group, per season.
agent_mix_by_season <- function(clean) {
  clean |>
    dplyr::filter(.data$in_scope, .data$outbreak_type == "Respiratory") |>
    dplyr::mutate(agent = factor(agent_mix_label(.data$agent_mix_group), levels = AGENT_MIX_LEVELS)) |>
    dplyr::count(.data$season, .data$agent, .drop = FALSE) |>
    dplyr::group_by(.data$season) |>
    dplyr::mutate(total = sum(.data$n), share = .data$n / .data$total) |>
    dplyr::ungroup()
}

#' Data for outbreak-duration analysis (Kaplan-Meier).
#'
#' Post-pandemic period (2022/23 onward) by default, so durations reflect
#' current outbreak-management practice; open outbreaks are censored at the
#' data-as-of date (event = 0).
duration_data <- function(clean, periods = "post-pandemic") {
  clean |>
    dplyr::filter(.data$in_scope, .data$period %in% periods,
                  .data$outbreak_type %in% c("Respiratory", "Enteric")) |>
    dplyr::mutate(
      setting = factor(.data$setting_group, levels = SETTING_LEVELS[-1]),
      agent = factor(agent_mix_label(.data$agent_mix_group), levels = AGENT_MIX_LEVELS)
    )
}

#' Median duration (days) with 95% CI from a Kaplan-Meier fit, per group.
km_medians <- function(data, group) {
  fit <- survival::survfit(stats::as.formula(paste("survival::Surv(duration_days, event) ~", group)),
                           data = data)
  tab <- summary(fit)$table
  tibble::tibble(
    group = sub("^[^=]+=", "", rownames(tab)),
    outbreaks = as.integer(tab[, "records"]),
    still_open = as.integer(tab[, "records"] - tab[, "events"]),
    median_days = unname(tab[, "median"]),
    ci_low = unname(tab[, "0.95LCL"]),
    ci_high = unname(tab[, "0.95UCL"])
  )
}

#' Outbreaks per facility per season (facilities with at least one outbreak).
facility_season_counts <- function(clean) {
  clean |>
    dplyr::filter(.data$in_scope) |>
    dplyr::count(.data$season, .data$season_start_year, .data$facility_id, .data$facility_name,
                 setting = .data$facility_setting_group, name = "outbreaks")
}

#' Distribution of outbreaks per facility in each season, by setting.
recurrence_distribution <- function(fsc) {
  fsc |>
    dplyr::mutate(bucket = cut(.data$outbreaks, c(0, 1, 2, 3, 5, Inf),
                               labels = c("1", "2", "3", "4-5", "6+"))) |>
    dplyr::count(.data$season, .data$season_start_year, .data$setting, .data$bucket, name = "facilities") |>
    dplyr::group_by(.data$season, .data$setting) |>
    dplyr::mutate(share = .data$facilities / sum(.data$facilities)) |>
    dplyr::ungroup()
}

#' Share of facilities with 2+ outbreaks in a season, by setting.
repeat_share <- function(fsc) {
  fsc |>
    dplyr::group_by(.data$season, .data$season_start_year, .data$setting) |>
    dplyr::summarise(facilities = dplyr::n(), repeat_facilities = sum(.data$outbreaks >= 2),
                     share_repeat = .data$repeat_facilities / .data$facilities, .groups = "drop")
}
