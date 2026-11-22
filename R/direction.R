# Week-on-week direction of new outbreaks (higher / similar / lower), using the
# decision rules Toronto Public Health publishes for institutional outbreaks
# in its Integrated Respiratory Diseases Dashboard (technical notes, tables 4
# and 5, October 2026):
#
# - any move from 0 is "higher"; any move to 0 is "lower";
# - below a cutoff of new outbreaks in the previous week, an absolute change
#   (2 outbreaks for influenza, 3 for COVID-19) is needed;
# - at or above the cutoff (5 for influenza, 15 for COVID-19), a relative
#   change of 10% or more.
#
# TPH does not assess RSV (it is reportable only as the cause of an
# institutional outbreak); the influenza rule is applied to it here.
#
# An outbreak counts towards an agent if that agent was detected at all
# (decision D4); respiratory outbreaks, all in-scope settings, by week of
# onset. Weeks are surveillance (MMWR) weeks, Sunday to Saturday.

DIRECTION_RULES <- list(
  influenza = list(cutoff = 5, abs_change = 2, rel_change = 0.10),
  covid19 = list(cutoff = 15, abs_change = 3, rel_change = 0.10),
  rsv = list(cutoff = 5, abs_change = 2, rel_change = 0.10)
)
DIRECTION_LEVELS <- c("Lower", "Similar", "Higher")

#' Direction of `n` (this week) relative to `prev` (previous week).
outbreak_direction <- function(n, prev, rule) {
  eps <- 1e-9  # so that exactly 10% counts as a 10% change
  change <- n - prev
  out <- dplyr::case_when(
    is.na(n) | is.na(prev) ~ NA_character_,
    prev == 0 & n == 0 ~ "Similar",
    prev == 0 ~ "Higher",
    n == 0 ~ "Lower",
    prev < rule$cutoff & change >= rule$abs_change ~ "Higher",
    prev < rule$cutoff & change <= -rule$abs_change ~ "Lower",
    prev < rule$cutoff ~ "Similar",
    change >= rule$rel_change * prev - eps ~ "Higher",
    change <= -rule$rel_change * prev + eps ~ "Lower",
    TRUE ~ "Similar"
  )
  out
}

#' Last day of the latest complete surveillance week on or before `as_of`
#' (a Saturday).
last_complete_week_end <- function(as_of) {
  as_of <- as.Date(as_of)
  as_of - (lubridate::wday(as_of, week_start = 7) %% 7)
}

#' Weekly new outbreaks involving `agent`, every surveillance week from the
#' start of the data to the latest complete week, with the previous week's
#' count and the direction.
weekly_direction <- function(clean, agent, as_of) {
  rule <- DIRECTION_RULES[[agent]]
  if (is.null(rule)) stop("No direction rule for agent: ", agent, call. = FALSE)
  last_end <- last_complete_week_end(as_of)
  d <- clean[clean$in_scope & clean$outbreak_type == "Respiratory" & agent_flag(clean, agent), ]
  wk <- lubridate::floor_date(d$began, unit = "week", week_start = 7)
  first <- lubridate::floor_date(epi_week_start_date(min(clean$season_start_year), 35L),
                                 unit = "week", week_start = 7)
  starts <- seq(first, last_end - 6, by = "week")
  n <- as.integer(table(factor(as.character(wk), levels = as.character(starts))))
  tibble::tibble(
    week_start = starts,
    week_end = starts + 6,
    epi_week = as.integer(lubridate::epiweek(starts)),
    season_start_year = season_start_year(starts),
    season_week_52 = season_week_52(starts),
    n = n,
    prev = dplyr::lag(n),
    direction = outbreak_direction(n, dplyr::lag(n), rule)
  )
}
