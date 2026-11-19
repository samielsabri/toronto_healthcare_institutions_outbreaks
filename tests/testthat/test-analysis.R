toy <- function(season_start, week, setting = "LTCH", type = "Respiratory", n = 1) {
  tibble::tibble(season_start_year = season_start, season = season_label(season_start),
                 season_week_52 = week, setting_group = setting, facility_setting_group = setting,
                 outbreak_type = type, in_scope = TRUE)[rep(1, n), ]
}

current <- list(season_start_year = 2026L, season = "2026/27", season_week = 10L)

test_that("weekly counts fill zeros, add 'All settings', and stop at the current week", {
  d <- dplyr::bind_rows(toy(2026, 3, n = 2), toy(2026, 3, setting = "Hospital"), toy(2025, 50))
  w <- weekly_counts(d, current)
  cur <- dplyr::filter(w, season == "2026/27", type == "Respiratory")
  expect_equal(max(cur$week), 10L)
  expect_equal(cur$n[cur$week == 3 & cur$setting == "LTCH"], 2L)
  expect_equal(cur$n[cur$week == 3 & cur$setting == "All settings"], 3L)
  expect_equal(cur$n[cur$week == 4 & cur$setting == "LTCH"], 0L)
  past <- dplyr::filter(w, season == "2025/26", type == "Respiratory", setting == "LTCH")
  expect_equal(nrow(past), 52L)
})

test_that("pre-pandemic range uses 2019/20 only up to season week 28 (D6)", {
  d <- dplyr::bind_rows(toy(2016, 28, n = 1), toy(2017, 28, n = 5), toy(2018, 28, n = 2),
                        toy(2019, 28, n = 9), toy(2019, 30, n = 50), toy(2016, 30, n = 1))
  w <- weekly_counts(d, current)
  r <- baseline_range(w, "pre-pandemic", current) |> dplyr::filter(setting == "LTCH", type == "Respiratory")
  wk28 <- r[r$week == 28, ]
  expect_equal(c(wk28$lo, wk28$hi, wk28$n_seasons), c(1, 9, 4))
  wk30 <- r[r$week == 30, ]
  expect_equal(c(wk30$lo, wk30$hi, wk30$n_seasons), c(0, 1, 3))  # 2019/20's 50 excluded
})

test_that("the current season never enters the post-pandemic range", {
  d <- dplyr::bind_rows(toy(2022, 5, n = 3), toy(2025, 5, n = 4), toy(2026, 5, n = 100))
  w <- weekly_counts(d, current)
  r <- baseline_range(w, "post-pandemic", current) |> dplyr::filter(week == 5, setting == "LTCH", type == "Respiratory")
  expect_equal(r$hi, 4)
  expect_equal(r$lo, 0)
  expect_equal(r$n_seasons, 4L)  # 2022/23-2025/26; 2023/24 and 2024/25 had none, so count as zero
})

test_that("agent display groups fold to the fixed levels", {
  expect_equal(agent_mix_label(c("influenza_a", "influenza_b", "hmpv", "multiple", "unknown")),
               c("Influenza", "Influenza", "Other respiratory virus", "Multiple agents", "Unknown"))
  expect_true(all(agent_mix_label(c(names(AGENT_GROUPS), "multiple")) %in% AGENT_MIX_LEVELS))
})

test_that("D9: wards declared together are one episode; a later outbreak is a repeat", {
  d <- tibble::tibble(
    in_scope = TRUE, facility_id = c("a", "a", "a", "a", "a", "b"), facility_name = "X",
    facility_setting_group = "LTCH",
    outbreak_type = c("Respiratory", "Respiratory", "Respiratory", "Enteric", "Respiratory", "Respiratory"),
    began = as.Date(c("2025-11-01", "2025-11-03", "2025-11-12", "2025-11-02", "2026-01-20", "2025-11-01"))
  )
  ep <- add_episodes(d)
  a_resp <- ep[ep$facility_id == "a" & ep$outbreak_type == "Respiratory", ]
  # 1 Nov, 3 Nov, 12 Nov chain within 14 days -> one episode; 20 Jan -> new episode
  expect_equal(length(unique(a_resp$episode_id)), 2)
  # enteric outbreak at the same time is its own episode
  expect_equal(length(unique(ep$episode_id[ep$facility_id == "a"])), 3)
  fsc <- facility_season_counts(d)
  expect_equal(fsc$outbreaks[fsc$facility_id == "a"], 3L)       # 2 respiratory + 1 enteric
  expect_equal(fsc$unit_outbreaks[fsc$facility_id == "a"], 5L)
  rs <- repeat_share(fsc)
  expect_equal(rs$share_repeat, 0.5)                              # a repeats, b does not
})

test_that("an episode belongs to the season of its first onset", {
  d <- tibble::tibble(in_scope = TRUE, facility_id = "a", facility_name = "X",
                      facility_setting_group = "LTCH", outbreak_type = "Respiratory",
                      began = as.Date(c("2025-08-20", "2025-08-28")))  # ISO wk 34, then wk 35
  fsc <- facility_season_counts(d)
  expect_equal(fsc$season, "2024/25")
  expect_equal(fsc$outbreaks, 1L)
})

test_that("KM medians return one row per group with censoring counted", {
  d <- tibble::tibble(duration_days = c(5, 10, 15, 20, 8), event = c(1, 1, 1, 0, 1),
                      setting = factor(c("LTCH", "LTCH", "LTCH", "Hospital", "Hospital")))
  m <- km_medians(d, "setting")
  expect_equal(nrow(m), 2)
  expect_equal(m$still_open[m$group == "Hospital"], 1L)
  expect_equal(m$median_days[m$group == "LTCH"], 10)
})
