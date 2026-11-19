# Phase 2: build the cleaned outbreak dataset from data/raw/.
# Run from the project root:  Rscript scripts/03_clean.R
#
# Outputs:
#   data/clean/outbreaks.parquet   one row per outbreak (unit level)
#   data/clean/facilities.csv      one row per facility (physical site x setting)
#   reports/tables/clean_*.csv     QA tables

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
for (f in c("R/read_raw.R", "R/clean_agents.R", "R/clean_setting.R",
            "R/clean_facility.R", "R/clean_dates.R", "R/clean_pipeline.R")) source(f)

manifest <- read_csv("data/raw/manifest.csv", show_col_types = FALSE)
refreshed <- unique(manifest$package_last_refreshed)
if (length(refreshed) != 1 || is.na(refreshed)) {
  stop("manifest.csv must have one package_last_refreshed value.", call. = FALSE)
}
as_of <- data_as_of(as.POSIXct(refreshed, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"))

clean <- clean_outbreaks(
  raw_list = read_raw_files("data/raw"),
  as_of = as_of,
  agent_lookup = load_agent_lookup(),
  address_crosswalk = load_address_crosswalk()
)
facilities <- summarise_facilities(clean)

dir.create("data/clean", showWarnings = FALSE)
nanoparquet::write_parquet(
  clean, "data/clean/outbreaks.parquet", compression = "zstd",
  options = nanoparquet::parquet_options(compression_level = 19)
)
write_csv(facilities, "data/clean/facilities.csv", na = "")

# ---- QA tables ---------------------------------------------------------------
qa <- "reports/tables"
dir.create(qa, showWarnings = FALSE, recursive = TRUE)

# Agent groups by season (decision D4 counting: every group detected)
clean |>
  filter(in_scope) |>
  group_by(season) |>
  summarise(outbreaks = n(), co_detections = sum(co_detection),
            across(starts_with("agent_") & where(is.logical), sum)) |>
  write_csv(file.path(qa, "clean_agent_groups_by_season.csv"))

# Setting groups by season
clean |>
  count(season, setting_group, hospital_subtype) |>
  write_csv(file.path(qa, "clean_setting_by_season.csv"), na = "")

# Facilities whose setting is a small minority at their address: possible
# setting miscoding that creates a spurious facility (for review).
clean |>
  count(facility_address, setting_group, name = "n") |>
  group_by(facility_address) |>
  mutate(share = n / sum(n), n_settings = n()) |>
  ungroup() |>
  filter(n_settings > 1, n <= 2, share < 0.1) |>
  arrange(facility_address) |>
  write_csv(file.path(qa, "clean_minority_setting_facilities.csv"))

# Quality flags
clean |>
  summarise(across(starts_with("flag_"), sum)) |>
  write_csv(file.path(qa, "clean_quality_flags.csv"))

message(sprintf(
  paste0("Data as of %s (refresh %s). %d outbreaks (%d duplicates removed), ",
         "%d open; %d facilities; %d units."),
  as_of, refreshed, nrow(clean), attr(clean, "n_duplicates_removed"),
  sum(clean$is_open), nrow(facilities), n_distinct(clean$unit_id)
))
