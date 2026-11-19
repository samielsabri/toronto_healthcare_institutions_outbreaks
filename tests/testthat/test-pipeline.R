raw_row <- function(...) {
  base <- list(`_id` = "1", `Institution Name` = "Home A - 2nd Fl", `Institution Address` = "1 Main St",
               `Outbreak Setting` = "LTCH", `Type of Outbreak` = "Respiratory",
               `Causative Agent-1` = "COVID-19", `Causative Agent-2` = "",
               `Date Outbreak Began` = "2025-12-28", `Date Declared Over` = "2026-01-10", Active = "N")
  tibble::as_tibble(utils::modifyList(base, list(...)))
}

test_that("the 2020 column-name variant is accepted; other changes fail loudly", {
  ok <- raw_row()
  names(ok)[6:7] <- c("Causative Agent - 1", "Causative Agent - 2")
  expect_equal(nrow(stack_raw(list(`2020` = ok))), 1)

  renamed <- raw_row()
  names(renamed)[2] <- "Facility Name"
  expect_error(stack_raw(list(`2026` = renamed)), "Unexpected columns")

  extra <- dplyr::mutate(raw_row(), Region = "Toronto")
  expect_error(stack_raw(list(`2026` = extra)), "Unexpected columns")
})

test_that("an outbreak repeated in two annual files is kept once", {
  raw <- list(`2025` = raw_row(), `2026` = raw_row(`_id` = "57"))
  out <- dedup_outbreaks(stack_raw(raw))
  expect_equal(nrow(out), 1)
  expect_equal(attr(out, "n_removed"), 1)
  expect_equal(out$source_file_year, 2025L)  # the onset-year file is kept
})

test_that("rows that differ in any field are not deduplicated", {
  raw <- list(`2026` = rbind(raw_row(), raw_row(`_id` = "2", `Causative Agent-1` = "Influenza A (H3)")))
  expect_equal(nrow(dedup_outbreaks(stack_raw(raw))), 2)
})

test_that("outbreak IDs are unique even when the natural key repeats", {
  df <- stack_raw(list(`2022` = rbind(
    raw_row(`Institution Name` = "Hospital X - Tower", `Date Declared Over` = "2022-01-16"),
    raw_row(`_id` = "2", `Institution Name` = "Hospital X - Tower", `Date Declared Over` = "2022-01-18")
  )))
  ids <- make_outbreak_id(df)
  expect_equal(anyDuplicated(ids), 0L)
  expect_match(ids[2], "-2$")
})

# ---- Integration: the real data -------------------------------------------
clean <- clean_outbreaks(
  raw_list = read_raw_files(root_path("data", "raw")),
  as_of = as.Date("2026-11-11"),
  agent_lookup = load_agent_lookup(root_path("data", "reference", "agent_lookup.csv")),
  address_crosswalk = load_address_crosswalk(root_path("data", "reference", "address_crosswalk.csv"))
)

test_that("cleaning keeps every raw row and adds no duplicates", {
  raw_n <- sum(vapply(read_raw_files(root_path("data", "raw")), nrow, integer(1)))
  expect_equal(nrow(clean) + attr(clean, "n_duplicates_removed"), raw_n)
  expect_equal(anyDuplicated(clean$outbreak_id), 0L)
})

test_that("every outbreak has a facility, a season and an agent group", {
  for (col in c("facility_id", "facility_name", "unit_id", "season", "agent_groups",
                "setting_group", "outbreak_type", "period")) {
    expect_false(anyNA(clean[[col]]), info = col)
  }
  expect_true(all(clean$season_week_52 %in% 1:52))
})

test_that("durations and censoring are consistent", {
  expect_true(all(clean$duration_days >= 0))
  expect_equal(clean$event == 0, clean$is_open)
  expect_equal(clean$is_open, clean$active_raw == "Y")
})

test_that("agent counts match the raw labels", {
  expect_equal(sum(clean$agent_covid19), sum(clean$agent1_raw == "COVID-19"))
  expect_equal(sum(clean$agent_mix_group == "multiple"), sum(clean$co_detection))
  # Shelter is kept but out of scope (decision D1)
  expect_equal(sum(!clean$in_scope), sum(clean$setting_raw == "Shelter"))
})
