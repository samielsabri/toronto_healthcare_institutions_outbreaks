# Dates: data-as-of date, duration with censoring, respiratory season and
# epi-week columns, and the comparison periods from decision D6.

#' Data-as-of date for a refresh.
#'
#' The City refreshes on Thursdays with data as of Wednesday 2 pm, so the
#' as-of date is the most recent Wednesday strictly before the refresh date.
data_as_of <- function(refreshed) {
  d <- as.Date(refreshed)
  dow <- lubridate::wday(d, week_start = 1)  # Monday = 1 ... Wednesday = 3
  d - (((dow - 3 - 1) %% 7) + 1)
}

#' Add is_open, duration_days and event (decision: censor at data-as-of date).
#'
#' Closed outbreaks: duration = declared_over - began, event = 1.
#' Open outbreaks: duration = as_of - began (follow-up so far), event = 0.
add_duration <- function(df, as_of) {
  as_of <- as.Date(as_of)
  df$is_open <- is.na(df$declared_over)
  end <- dplyr::if_else(df$is_open, as_of, df$declared_over)
  df$duration_days <- as.integer(end - df$began)
  df$event <- as.integer(!df$is_open)
  df$data_as_of <- as_of
  df
}

#' Season start year: ISO week 35 to ISO week 34.
season_start_year <- function(date) {
  y <- lubridate::isoyear(date)
  w <- lubridate::isoweek(date)
  as.integer(ifelse(w >= 35, y, y - 1L))
}

#' Season label, e.g. 2025 -> "2025/26".
season_label <- function(start_year) {
  sprintf("%d/%02d", start_year, (start_year + 1L) %% 100L)
}

#' Number of ISO weeks in an ISO year (52 or 53).
iso_weeks_in_year <- function(year) {
  lubridate::isoweek(as.Date(paste0(year, "-12-28")))
}

#' Week of the season: ISO week 35 = 1 ... ISO week 34 = 52 (or 53).
season_week <- function(date) {
  w <- lubridate::isoweek(date)
  start <- season_start_year(date)
  as.integer(ifelse(w >= 35, w - 34L, w + iso_weeks_in_year(start) - 34L))
}

#' Season week on a fixed 52-week axis: ISO week 53 is counted as ISO week 52
#' (decision D7). ISO week 35 = 1, ISO week 52/53 = 18, ISO week 1 = 19,
#' ISO week 34 = 52.
season_week_52 <- function(date) {
  w <- pmin(lubridate::isoweek(date), 52L)
  as.integer(ifelse(w >= 35, w - 34L, w + 52L - 34L))
}

#' Comparison period (decision D6).
#'
#' - "pre-pandemic": 2016/17-2018/19, plus 2019/20 up to ISO week 10 of 2020
#'   (season week 28);
#' - "pandemic": 2019/20 from season week 29, to the end of 2021/22;
#' - "post-pandemic": 2022/23 onward;
#' - "partial season": 2015/16 (data start in January 2016).
comparison_period <- function(start_year, week) {
  dplyr::case_when(
    start_year <= 2015 ~ "partial season",
    start_year <= 2018 ~ "pre-pandemic",
    start_year == 2019 & week <= 28 ~ "pre-pandemic",
    start_year <= 2021 ~ "pandemic",
    TRUE ~ "post-pandemic"
  )
}

#' Add season and epi-week columns based on onset date (began).
#'
#' season_week_52 merges ISO week 53 into ISO week 52 for season-on-season
#' charts (decision D7), so every season has weeks 1-52. ISO week 53 falls at
#' the end of December, i.e. season week 19, not at the end of the season.
add_season <- function(df) {
  df$iso_year <- as.integer(lubridate::isoyear(df$began))
  df$iso_week <- as.integer(lubridate::isoweek(df$began))
  df$epi_week_start <- lubridate::floor_date(df$began, unit = "week", week_start = 1)
  df$season_start_year <- season_start_year(df$began)
  df$season <- season_label(df$season_start_year)
  df$season_week <- season_week(df$began)
  df$season_week_52 <- season_week_52(df$began)
  df$period <- comparison_period(df$season_start_year, df$season_week)
  df
}
