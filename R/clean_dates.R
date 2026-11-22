# Dates: data-as-of date, duration with censoring, respiratory season and
# surveillance-week columns, and the comparison periods from decision D6.

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
  if (any(df$began > as_of)) {
    stop("Some outbreaks began after the data-as-of date (", as_of,
         "); check the refresh time in data/raw/manifest.csv.", call. = FALSE)
  }
  df$is_open <- is.na(df$declared_over)
  end <- dplyr::if_else(df$is_open, as_of, df$declared_over)
  df$duration_days <- as.integer(end - df$began)
  df$event <- as.integer(!df$is_open)
  df$data_as_of <- as_of
  df
}

# Weeks are surveillance weeks as used by Toronto Public Health, Public Health
# Ontario and PHAC's FluWatch: CDC MMWR epidemiological weeks, Sunday to
# Saturday. Week 1 is the week containing 4 January; a year has 52 or 53 weeks.
# The respiratory season runs from week 35 to week 34.

#' Season start year: surveillance week 35 to week 34.
season_start_year <- function(date) {
  y <- lubridate::epiyear(date)
  w <- lubridate::epiweek(date)
  as.integer(ifelse(w >= 35, y, y - 1L))
}

#' Season label, e.g. 2025 -> "2025/26".
season_label <- function(start_year) {
  sprintf("%d/%02d", start_year, (start_year + 1L) %% 100L)
}

#' Number of surveillance weeks in a year (52 or 53): the week containing
#' 28 December is always the last week of its year.
epi_weeks_in_year <- function(year) {
  lubridate::epiweek(as.Date(paste0(year, "-12-28")))
}

#' Week of the season: surveillance week 35 = 1 ... week 34 = 52 (or 53).
season_week <- function(date) {
  w <- lubridate::epiweek(date)
  start <- season_start_year(date)
  as.integer(ifelse(w >= 35, w - 34L, w + epi_weeks_in_year(start) - 34L))
}

#' Season week on a fixed 52-week axis: week 53 is counted as week 52
#' (decision D7). Week 35 = 1, week 52/53 = 18, week 1 = 19, week 34 = 52.
season_week_52 <- function(date) {
  w <- pmin(lubridate::epiweek(date), 52L)
  as.integer(ifelse(w >= 35, w - 34L, w + 52L - 34L))
}

#' Sunday that starts surveillance week `week` of `year`.
epi_week_start_date <- function(year, week) {
  jan4 <- as.Date(sprintf("%d-01-04", year))
  jan4 - (lubridate::wday(jan4, week_start = 7) - 1) + 7 * (week - 1)
}

#' Comparison period (decision D6).
#'
#' - "pre-pandemic": 2016/17-2018/19, plus 2019/20 up to week 10 of 2020
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

#' Add season and surveillance-week columns based on onset date (began).
#'
#' season_week_52 merges week 53 into week 52 for season-on-season charts
#' (decision D7), so every season has weeks 1-52. Week 53 falls at the end of
#' December, i.e. season week 19, not at the end of the season.
add_season <- function(df) {
  df$epi_year <- as.integer(lubridate::epiyear(df$began))
  df$epi_week <- as.integer(lubridate::epiweek(df$began))
  df$epi_week_start <- lubridate::floor_date(df$began, unit = "week", week_start = 7)
  df$season_start_year <- season_start_year(df$began)
  df$season <- season_label(df$season_start_year)
  df$season_week <- season_week(df$began)
  df$season_week_52 <- season_week_52(df$began)
  df$period <- comparison_period(df$season_start_year, df$season_week)
  df
}

#' Surveillance week number for a season week on the 52-week axis
#' (1 = week 35; 18 = week 52, which also holds week 53 under decision D7).
epi_week_of_season_week <- function(w) ((w + 34 - 1) %% 52) + 1
