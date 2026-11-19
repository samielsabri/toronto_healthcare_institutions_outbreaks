good_raw <- function(...) {
  base <- list(`_id` = "1", `Institution Name` = "Home A", `Institution Address` = "1 Main St",
               `Outbreak Setting` = "LTCH", `Type of Outbreak` = "Respiratory",
               `Causative Agent-1` = "COVID-19", `Causative Agent-2` = "",
               `Date Outbreak Began` = "2026-11-01", `Date Declared Over` = "", Active = "Y")
  tibble::as_tibble(utils::modifyList(base, list(...)))
}
lk <- tibble::tibble(raw_label = "COVID-19", agent_groups = "covid19", note = "")
refreshed <- as.POSIXct("2026-11-12 16:05:45", tz = "UTC")

test_that("clean input passes", {
  res <- check_inputs(list(`2026` = good_raw()), lk, refreshed, today = as.Date("2026-11-13"))
  expect_length(res$failures, 0)
  expect_length(res$warnings, 0)
  expect_match(format_check_report(res, refreshed), "All checks passed")
})

test_that("a renamed or added column is reported as a schema change", {
  x <- good_raw(); names(x)[3] <- "Address"
  res <- check_inputs(list(`2026` = x), lk, refreshed, today = as.Date("2026-11-13"))
  expect_length(res$failures, 1)
  expect_match(res$failures, "Schema change in ob_report_2026")
  expect_match(res$failures, "`Address`")
  expect_match(res$failures, "Missing columns: `Institution Address`")
})

test_that("new labels and settings are all reported together", {
  x <- good_raw(`Causative Agent-1` = "Measles", `Outbreak Setting` = "Hospice")
  res <- check_inputs(list(`2026` = x), lk, refreshed, today = as.Date("2026-11-13"))
  expect_length(res$failures, 2)
  expect_true(any(grepl("Measles", res$failures)))
  expect_true(any(grepl("Hospice", res$failures)))
})

test_that("a stale refresh is a warning, not a failure", {
  res <- check_inputs(list(`2026` = good_raw()), lk, refreshed, today = as.Date("2026-11-26"))
  expect_length(res$failures, 0)
  expect_match(res$warnings, "14 days ago")
})

test_that("the committed raw data pass", {
  res <- check_inputs(read_raw_files(root_path("data", "raw")),
                      load_agent_lookup(root_path("data", "reference", "agent_lookup.csv")),
                      refreshed, today = as.Date("2026-11-13"))
  expect_length(res$failures, 0)
})
