current <- list(season_start_year = 2026L, season = "2026/27", season_week = 8L)
th <- list(epidemic = 2, medium = 5, high = 10, very_high = 20)

wk <- function(n) tibble::tibble(season_start_year = 2026L, week = seq_along(n), n = n)

test_that("onset is the first complete week above the epidemic threshold", {
  st <- onset_status(wk(c(0, 1, 3, 4, 6, 7, 8, 99)), th, current)  # week 8 is partial: ignored
  expect_true(st$started)
  expect_equal(st$first_week, 3L)
  expect_equal(st$weeks_above_now, 5L)
  expect_equal(st$latest_week, 7L)
  expect_equal(st$latest_level, "Medium")
  expect_equal(onset_status_value(st), "Medium")
  expect_match(onset_status_title(st, "Influenza"), "5 weeks running \\(first crossed in W37\\)")
})

test_that("no onset while every complete week is at or below the threshold", {
  st <- onset_status(wk(c(0, 1, 2, 2, 1, 0, 2, 50)), th, current)
  expect_false(st$started)
  expect_equal(onset_status_value(st), "Not started")
})

test_that("a season that has dropped back below the threshold says so", {
  st <- onset_status(wk(c(3, 4, 1, 0, 0, 0, 1, 0)), th, current)
  expect_true(st$started)
  expect_equal(st$weeks_above_now, 0L)
  expect_equal(onset_status_value(st), "Below threshold")
  expect_match(onset_status_title(st, "RSV"), "first crossed the threshold in W35, now below it")
})

test_that("intensity levels follow the MEM thresholds", {
  expect_equal(intensity_level(c(1, 3, 6, 15, 25), th),
               c("Below epidemic threshold", "Low", "Medium", "High", "Very high"))
})

test_that("MEM fits on the real data and gives ordered thresholds", {
  clean <- nanoparquet::read_parquet(root_path("data", "clean", "outbreaks.parquet"))
  cur <- current_season_info(freshness_info(root_path("data", "raw", "manifest.csv"))$as_of)
  for (agent in c("influenza", "rsv")) {
    t <- fit_mem_thresholds(onset_weekly(clean, agent, cur), cur)
    expect_equal(t$seasons[1], "2022/23", info = agent)
    expect_false(cur$season %in% t$seasons, info = agent)
    expect_true(t$epidemic > 0 && t$epidemic < t$medium && t$medium < t$high && t$high < t$very_high,
                info = agent)
  }
})
