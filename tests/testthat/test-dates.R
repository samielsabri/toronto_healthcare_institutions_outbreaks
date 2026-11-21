test_that("data-as-of is the Wednesday before the refresh", {
  expect_equal(data_as_of(as.Date("2026-11-12")), as.Date("2026-11-11"))  # Thursday
  expect_equal(data_as_of(as.Date("2026-11-13")), as.Date("2026-11-11"))  # Friday
  expect_equal(data_as_of(as.Date("2026-11-11")), as.Date("2026-11-04"))  # Wednesday: previous week
  expect_equal(data_as_of(as.POSIXct("2026-11-12 16:05:45", tz = "UTC")), as.Date("2026-11-11"))
})

test_that("open outbreaks are censored at the as-of date", {
  df <- tibble::tibble(began = as.Date(c("2026-11-01", "2026-10-01")),
                       declared_over = as.Date(c(NA, "2026-10-15")))
  out <- add_duration(df, as.Date("2026-11-11"))
  expect_equal(out$is_open, c(TRUE, FALSE))
  expect_equal(out$duration_days, c(10L, 14L))
  expect_equal(out$event, c(0L, 1L))
  expect_error(add_duration(df, as.Date("2026-10-30")), "after the data-as-of date")
})

test_that("seasons run from ISO week 35 to ISO week 34", {
  d <- as.Date(c("2025-08-24", "2025-08-25", "2026-01-01", "2026-08-23", "2026-08-24"))
  # 2025-08-24 is ISO week 34; 2025-08-25 is ISO week 35
  expect_equal(season_start_year(d), c(2024L, 2025L, 2025L, 2025L, 2026L))
  expect_equal(season_label(c(2024L, 2025L, 2099L)), c("2024/25", "2025/26", "2099/00"))
  expect_equal(season_week(d), c(52L, 1L, 19L, 52L, 1L))  # ISO wk 1 = season wk 19
})

test_that("53-week seasons: ISO week 53 is season week 19, and D7 merges it into 18", {
  d <- as.Date(c("2020-12-21", "2020-12-28", "2021-01-04", "2021-08-29"))
  # ISO weeks 52, 53, 1, 34 of the 2020/21 season
  expect_equal(lubridate::isoweek(d), c(52, 53, 1, 34))
  expect_equal(season_week(d), c(18L, 19L, 20L, 53L))
  expect_equal(season_week_52(d), c(18L, 18L, 19L, 52L))
})

test_that("comparison periods follow decision D6", {
  expect_equal(comparison_period(c(2015L, 2016L, 2018L, 2019L, 2019L, 2021L, 2022L, 2026L),
                                 c(10L, 10L, 52L, 28L, 29L, 1L, 1L, 1L)),
               c("partial season", "pre-pandemic", "pre-pandemic", "pre-pandemic",
                 "pandemic", "pandemic", "post-pandemic", "post-pandemic"))
  # ISO week 10 of 2020 is season week 28 of 2019/20; week 11 is week 29
  expect_equal(season_week(as.Date(c("2020-03-02", "2020-03-09"))), c(28L, 29L))
})

test_that("freshness info reads the refresh time and derives the as-of date", {
  info <- freshness_info(root_path("data", "raw", "manifest.csv"))
  expect_s3_class(info$refreshed, "POSIXct")
  expect_equal(lubridate::wday(info$as_of, week_start = 1), 3)  # a Wednesday
  expect_true(info$as_of < as.Date(info$refreshed))
  expect_match(freshness_banner_html(info), format(info$as_of, "%Y-%m-%d"))
})

test_that("season weeks map back to ISO week labels", {
  expect_equal(iso_week_of_season_week(c(1, 18, 19, 52)), c(35, 52, 1, 34))
})
