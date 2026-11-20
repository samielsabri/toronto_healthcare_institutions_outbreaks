test_that("map data: one row per facility with this season's outbreaks, open counts add up", {
  clean <- nanoparquet::read_parquet(root_path("data", "clean", "outbreaks.parquet"))
  fac <- readr::read_csv(root_path("data", "clean", "facilities.csv"), show_col_types = FALSE)
  cur <- current_season_info(freshness_info(root_path("data", "raw", "manifest.csv"))$as_of)
  d <- facility_map_data(clean, fac, cur)
  this <- clean[clean$in_scope & clean$season == cur$season, ]
  expect_equal(anyDuplicated(d$facility_id), 0L)
  expect_equal(nrow(d), dplyr::n_distinct(this$facility_id))
  expect_equal(sum(d$outbreaks), nrow(this))
  expect_equal(sum(d$open_now), sum(this$is_open))
  expect_true(all(d$episodes <= d$outbreaks))
  expect_false(anyNA(d$lon))
})

test_that("popups escape facility names", {
  d <- tibble::tibble(facility_name = "<script>x</script>", setting_group = factor("LTCH"),
                      facility_address = "1 MAIN ST", neighbourhood = "N", outbreaks = 1L, episodes = 1L,
                      open_now = 0L, latest_onset = as.Date("2026-11-01"), types = "Respiratory", agents = "RSV",
                      units_open = "")
  expect_false(grepl("<script>", facility_popup(d), fixed = TRUE))
})
