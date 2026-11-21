# Phase 1, steps 2-3: profile the raw annual files and tabulate label drift.
# Writes the profiling tables to reports/tables/.
# Run from the project root:  Rscript scripts/02_profile_raw.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(lubridate)
})
source("R/read_raw.R")

out_dir <- "reports/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_table <- function(x, name) {
  write_csv(x, file.path(out_dir, paste0(name, ".csv")), na = "")
  invisible(x)
}

manifest <- read_csv("data/raw/manifest.csv", show_col_types = FALSE)
extract_date <- as.Date(substr(min(manifest$downloaded_at), 1, 10))
raw_list <- read_raw_files("data/raw")

# ---- 1. Schema: column names and readr's guessed types, per file ----------
schema <- imap_dfr(raw_list, function(df, year) {
  guessed <- suppressMessages(readr::type_convert(df, guess_integer = TRUE))
  tibble(
    year     = as.integer(year),
    position = seq_along(df),
    column   = names(df),
    guessed_type = map_chr(guessed, ~ class(.x)[1])
  )
})
save_table(schema, "schema_by_year")

schema_wide <- schema |>
  mutate(col = paste0(column, " <", guessed_type, ">")) |>
  select(year, position, col) |>
  pivot_wider(names_from = year, values_from = col)
save_table(schema_wide, "schema_wide")

# ---- 2. Stack all years with harmonized column names ----------------------
raw <- imap_dfr(raw_list, ~ standardize_raw_names(.x) |> mutate(file_year = as.integer(.y)))

d <- raw |>
  transmute(
    file_year,
    row_id  = `_id`,
    name    = `Institution Name`,
    address = `Institution Address`,
    setting = `Outbreak Setting`,
    type    = `Type of Outbreak`,
    agent1  = `Causative Agent-1`,
    agent2  = `Causative Agent-2`,
    began_raw = `Date Outbreak Began`,
    over_raw  = `Date Declared Over`,
    active  = Active,
    began = as.Date(began_raw, format = "%Y-%m-%d"),
    over  = as.Date(na_if(over_raw, ""), format = "%Y-%m-%d"),
    duration_days = as.integer(over - began)
  )

# ---- 3. Per-file profile -------------------------------------------------
file_profile <- d |>
  group_by(file_year) |>
  summarise(
    rows = n(),
    began_min = min(began), began_max = max(began),
    over_min = min(over, na.rm = TRUE), over_max = max(over, na.rm = TRUE),
    open_no_over_date = sum(is.na(over)),
    active_Y = sum(active == "Y"),
    began_unparseable = sum(is.na(began)),
    over_unparseable = sum(over_raw != "" & is.na(over)),
    began_outside_file_year = sum(year(began) != file_year),
    over_before_began = sum(over < began, na.rm = TRUE),
    over_same_day = sum(over == began, na.rm = TRUE),
    began_after_extract = sum(began > extract_date),
    duration_median = median(duration_days, na.rm = TRUE),
    duration_p95 = round(unname(quantile(duration_days, 0.95, na.rm = TRUE))),
    duration_max = max(duration_days, na.rm = TRUE),
    duration_gt_90 = sum(duration_days > 90, na.rm = TRUE),
    pct_name_has_unit = round(100 * mean(str_detect(name, " - "))),
    max_name_chars = max(nchar(name)),
    .groups = "drop"
  ) |>
  left_join(select(manifest, file_year = year, bytes), by = "file_year")
save_table(file_profile, "file_profile")

# ---- 4. Missingness (blank strings and known placeholders) ----------------
missingness <- d |>
  group_by(file_year) |>
  summarise(
    rows = n(),
    across(c(name, address, setting, type, agent1, agent2, began_raw, over_raw, active),
           ~ sum(.x == ""), .names = "blank_{.col}"),
    agent1_unable_to_identify = sum(agent1 == "Unable to identify"),
    agent1_pending = sum(agent1 == "Pending"),
    agent2_literal_None = sum(agent2 == "None"),
    .groups = "drop"
  )
save_table(missingness, "missingness")

# ---- 5. Unique values by year (label drift) -------------------------------
tab_by_year <- function(var) {
  d |>
    mutate(value = if_else(.data[[var]] == "", "<blank>", .data[[var]])) |>
    count(file_year, value) |>
    pivot_wider(names_from = file_year, values_from = n, values_fill = 0) |>
    mutate(total = rowSums(across(where(is.numeric)))) |>
    arrange(value)
}
save_table(tab_by_year("setting"), "values_setting_by_year")
save_table(tab_by_year("type"),    "values_type_by_year")
save_table(tab_by_year("agent1"),  "values_agent1_by_year")
save_table(tab_by_year("agent2"),  "values_agent2_by_year")
save_table(tab_by_year("active"),  "values_active_by_year")
save_table(count(d, type, agent1, sort = TRUE), "type_by_agent1")

# COVID-19 / seasonal coronavirus labels by onset month (where the switch happens)
covid_labels <- d |>
  filter(str_detect(paste(agent1, agent2), regex("corona|covid", ignore_case = TRUE))) |>
  mutate(onset_month = format(began, "%Y-%m"),
         label = case_when(
           agent1 == "COVID-19" ~ "agent1: COVID-19",
           str_detect(agent1, "^Coronavirus\\*") ~ "agent1: Coronavirus*",
           str_detect(agent1, "^Coronavirus") ~ "agent1: Coronavirus",
           TRUE ~ "agent2 only"
         )) |>
  count(onset_month, label) |>
  pivot_wider(names_from = label, values_from = n, values_fill = 0) |>
  arrange(onset_month)
save_table(covid_labels, "covid_labels_by_month")

# ---- 6. Data-quality listings --------------------------------------------
save_table(d |> filter(is.na(over)) |> arrange(began) |>
             select(file_year, name, setting, type, agent1, agent2, began, active),
           "open_outbreaks")

save_table(d |> filter(duration_days > 90 | duration_days <= 0) |>
             arrange(desc(duration_days)) |>
             select(file_year, name, setting, type, agent1, began, over, duration_days),
           "duration_outliers")

key_cols <- c("name", "address", "setting", "type", "agent1", "agent2", "began_raw", "over_raw")
exact_dups <- d |> group_by(across(all_of(key_cols))) |> filter(n() > 1) |> ungroup()
same_unit_onset <- d |>
  group_by(name, began) |>
  filter(n() > 1) |>
  arrange(name, began, type) |>
  ungroup() |>
  select(file_year, name, setting, type, agent1, agent2, began, over)
save_table(same_unit_onset, "same_unit_same_onset")

# ---- 7. Rough unit-vs-facility comparison (exploratory only) --------------
# A deliberately crude address normalization, used only to size the
# unit-splitting issue for the report. The real facility derivation is Phase 2.
rough_addr <- function(x) {
  x <- toupper(x)
  x <- str_replace(x, "\\s*-.*$", "")          # drop "- Unit 2-2" style suffixes
  x <- str_replace_all(x, "[.,]", " ")
  abbrev <- c("\\bSTREET\\b" = "ST", "\\bAVENUE\\b" = "AVE", "\\bROAD\\b" = "RD",
              "\\bDRIVE\\b" = "DR", "\\bBOULEVARD\\b" = "BLVD", "\\bPARKWAY\\b" = "PKY",
              "\\bEAST\\b" = "E", "\\bWEST\\b" = "W", "\\bNORTH\\b" = "N",
              "\\bSOUTH\\b" = "S", "\\bCRESCENT\\b" = "CRES", "\\bCOURT\\b" = "CRT",
              "\\bPLACE\\b" = "PL", "\\bLANE\\b" = "LN")
  str_squish(str_replace_all(x, abbrev))
}
unit_vs_facility <- d |>
  mutate(addr_key = rough_addr(address)) |>
  arrange(addr_key, type, began) |>
  group_by(addr_key, type) |>
  mutate(new_episode = is.na(lag(began)) | as.integer(began - lag(began)) > 14,
         episode = cumsum(new_episode)) |>
  ungroup() |>
  group_by(file_year) |>
  summarise(unit_level_rows = n(),
            facility_episodes_14d = n_distinct(paste(addr_key, type, episode)),
            units_per_episode = round(unit_level_rows / facility_episodes_14d, 2),
            distinct_rough_addresses = n_distinct(addr_key),
            .groups = "drop")
save_table(unit_vs_facility, "unit_vs_facility_rough")

# ---- 8. Counts by respiratory season (ISO week 35 to week 34) -------------
by_season <- d |>
  mutate(iso_y = isoyear(began), iso_w = isoweek(began),
         season_start = if_else(iso_w >= 35, iso_y, iso_y - 1L),
         season = paste0(season_start, "/", str_sub(season_start + 1, 3, 4)),
         group = case_when(
           type == "Enteric" ~ "enteric",
           type == "Other" ~ "other",
           agent1 == "COVID-19" ~ "resp_covid19",
           agent1 %in% c("Unable to identify", "Pending") ~ "resp_unknown",
           TRUE ~ "resp_other_known"
         )) |>
  count(season, group) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
  relocate(season, resp_covid19, resp_other_known, resp_unknown, enteric, other)
save_table(by_season, "counts_by_season")

message("Extract date: ", extract_date, "; ", nrow(d), " rows profiled.")
message("Tables written to ", out_dir, "/")
if (nrow(exact_dups) > 0) message("NOTE: ", nrow(exact_dups), " exact duplicate rows found.")
