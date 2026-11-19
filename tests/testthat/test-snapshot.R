make_raw_dir <- function(refreshed, files) {
  dir <- tempfile("raw")
  dir.create(dir)
  rows <- list()
  for (year in names(files)) {
    f <- paste0("ob_report_", year, ".csv")
    writeLines(files[[year]], file.path(dir, f))
    rows[[year]] <- data.frame(resource_name = paste0("ob_report_", year), year = year, file = f,
                               package_last_refreshed = refreshed)
  }
  utils::write.csv(do.call(rbind, rows), file.path(dir, "manifest.csv"), row.names = FALSE)
  dir
}

csv <- function(...) c("_id,Institution Name", paste0(seq_along(c(...)), ",", c(...)))

test_that("the current-year file is saved under the refresh date, once", {
  snap <- tempfile("snap")
  raw <- make_raw_dir("2026-11-12T16:05:45Z", list(`2025` = csv("A"), `2026` = csv("B")))
  first <- save_snapshots(raw, snap)
  expect_true(file.exists(file.path(snap, "2026-11-12.csv")))
  expect_true(file.exists(file.path(snap, "prior_years", "2026-11-12_ob_report_2025.csv")))
  expect_equal(first$data_as_of, c("2026-11-11", "2026-11-11"))

  # Re-running for the same refresh saves nothing
  expect_equal(nrow(save_snapshots(raw, snap)), 0)
  expect_equal(nrow(read_snapshot_manifest(snap)), 2)
})

test_that("next week: new current-year snapshot; prior years only if changed", {
  snap <- tempfile("snap")
  save_snapshots(make_raw_dir("2026-11-12T16:05:45Z", list(`2024` = csv("X"), `2025` = csv("A"), `2026` = csv("B"))), snap)

  # 2025 edited, 2024 unchanged
  week2 <- make_raw_dir("2026-11-19T16:05:47Z", list(`2024` = csv("X"), `2025` = csv("A", "late"), `2026` = csv("B", "C")))
  saved <- save_snapshots(week2, snap)
  expect_setequal(saved$snapshot_file,
                  c("2026-11-19.csv", file.path("prior_years", "2026-11-19_ob_report_2025.csv")))
  expect_equal(readLines(file.path(snap, "2026-11-19.csv")), csv("B", "C"))  # copied unchanged
})

test_that("in January the latest year's file is treated as current", {
  snap <- tempfile("snap")
  saved <- save_snapshots(make_raw_dir("2027-01-07T16:00:00Z", list(`2026` = csv("B"))), snap)
  expect_equal(saved$snapshot_file, "2027-01-07.csv")
  expect_equal(saved$data_as_of, "2027-01-06")
})
