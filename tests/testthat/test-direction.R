flu <- DIRECTION_RULES$influenza
covid <- DIRECTION_RULES$covid19

test_that("moves from or to zero always count", {
  expect_equal(outbreak_direction(c(1, 0, 0), c(0, 1, 0), flu), c("Higher", "Lower", "Similar"))
})

test_that("influenza: below 5 the previous week, a change of 2 is needed", {
  expect_equal(outbreak_direction(c(5, 4, 2, 3), c(3, 3, 4, 4), flu),
               c("Higher", "Similar", "Lower", "Similar"))
})

test_that("influenza: from 5 up, a change of 10% is needed (exactly 10% counts)", {
  expect_equal(outbreak_direction(c(11, 9, 10, 33, 27), c(10, 10, 10, 30, 30), flu),
               c("Higher", "Lower", "Similar", "Higher", "Lower"))
  # TPH dashboard, week 45 of 2026/27: 7 -> 5 influenza outbreaks, assessed as lower
  expect_equal(outbreak_direction(5, 7, flu), "Lower")
})

test_that("COVID-19: below 15 a change of 3 is needed, from 15 up 10%", {
  expect_equal(outbreak_direction(c(8, 7, 2, 17, 16), c(5, 5, 5, 15, 15), covid),
               c("Higher", "Similar", "Lower", "Higher", "Similar"))
})

test_that("the latest complete week ends on the Saturday before the as-of Wednesday", {
  expect_equal(last_complete_week_end(as.Date("2026-11-18")), as.Date("2026-11-14"))
  expect_equal(last_complete_week_end(as.Date("2026-11-14")), as.Date("2026-11-14"))
})

test_that("weekly_direction counts outbreaks involving the agent by Sunday-Saturday week", {
  d <- tibble::tibble(
    in_scope = TRUE, outbreak_type = "Respiratory",
    began = as.Date(c("2026-11-01", "2026-11-07", "2026-11-08", "2026-11-15")),
    agent_influenza_a = c(TRUE, TRUE, TRUE, TRUE), agent_influenza_b = FALSE,
    agent_rsv = FALSE, agent_covid19 = FALSE
  )
  d$season_start_year <- season_start_year(d$began)
  w <- weekly_direction(d, "influenza", as.Date("2026-11-18"))
  expect_equal(max(w$week_end), as.Date("2026-11-14"))  # 15 Nov is in an incomplete week
  last2 <- utils::tail(w, 2)
  expect_equal(last2$epi_week, c(44L, 45L))
  expect_equal(last2$n, c(2L, 1L))
  expect_equal(last2$direction, c("Higher", "Similar"))
})
