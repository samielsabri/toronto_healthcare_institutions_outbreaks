# Cleaning pipeline: raw annual files -> one row per outbreak (unit level).
#
# Depends on R/read_raw.R, R/clean_agents.R, R/clean_setting.R,
# R/clean_facility.R and R/clean_dates.R.

EXPECTED_RAW_COLUMNS <- c(
  "_id", "Institution Name", "Institution Address", "Outbreak Setting",
  "Type of Outbreak", "Causative Agent-1", "Causative Agent-2",
  "Date Outbreak Began", "Date Declared Over", "Active"
)

#' Stack raw annual files into one table with snake_case raw columns.
#'
#' Fails if a file's columns (after mapping known historical variants) differ
#' from EXPECTED_RAW_COLUMNS.
stack_raw <- function(raw_list) {
  purrr::imap_dfr(raw_list, function(df, year) {
    df <- standardize_raw_names(df)
    if (!identical(names(df), EXPECTED_RAW_COLUMNS)) {
      stop(sprintf("Unexpected columns in %s file.\n  Expected: %s\n  Found:    %s",
                   year, toString(EXPECTED_RAW_COLUMNS), toString(names(df))),
           call. = FALSE)
    }
    tibble::tibble(
      source_file_year    = as.integer(year),
      source_row_id       = as.integer(df$`_id`),
      institution_name    = stringr::str_squish(df$`Institution Name`),
      institution_address = stringr::str_squish(df$`Institution Address`),
      setting_raw         = df$`Outbreak Setting`,
      type_raw            = df$`Type of Outbreak`,
      agent1_raw          = df$`Causative Agent-1`,
      agent2_raw          = df$`Causative Agent-2`,
      began               = as.Date(df$`Date Outbreak Began`, format = "%Y-%m-%d"),
      declared_over       = as.Date(dplyr::na_if(df$`Date Declared Over`, ""), format = "%Y-%m-%d"),
      active_raw          = df$Active
    )
  })
}

#' Remove exact duplicates across or within files.
#'
#' Rows identical on every raw field except _id and file year are duplicates;
#' the copy from the earliest file is kept. Returns the deduplicated table with
#' attribute "n_removed".
dedup_outbreaks <- function(df) {
  key_cols <- c("institution_name", "institution_address", "setting_raw", "type_raw",
                "agent1_raw", "agent2_raw", "began", "declared_over")
  df <- dplyr::arrange(df, .data$source_file_year, .data$source_row_id)
  dup <- duplicated(df[key_cols])
  out <- df[!dup, , drop = FALSE]
  attr(out, "n_removed") <- sum(dup)
  out
}

#' Stable-as-possible outbreak ID.
#'
#' A hash of normalized unit name, normalized address, onset date and raw
#' agent-1. `_id` cannot be used: it restarts at 1 in every file. Where the key
#' is not unique (e.g. two wards whose truncated names are identical), a
#' sequence suffix is added in declared-over order.
make_outbreak_id <- function(df) {
  key <- paste(normalize_name(df$institution_name), normalize_address(df$institution_address),
               df$began, df$agent1_raw, sep = "|")
  base <- substr(vapply(key, rlang::hash, character(1), USE.NAMES = FALSE), 1, 12)
  ord <- order(base, df$declared_over, df$source_row_id, na.last = TRUE)
  seq_in_key <- integer(length(base))
  seq_in_key[ord] <- stats::ave(seq_along(ord), base[ord], FUN = seq_along)
  ifelse(seq_in_key == 1, base, paste0(base, "-", seq_in_key))
}

#' Data-quality flags.
#'
#' - flag_same_unit_onset: another outbreak at the same unit with the same
#'   onset date (decision D3: kept as published, flagged);
#' - flag_name_truncated: name is exactly 60 characters in 2016-2021 files,
#'   where the source field was truncated;
#' - flag_zero_duration: declared over on the day it began;
#' - flag_pending_closed: agent still "Pending" although the outbreak is over.
add_quality_flags <- function(df) {
  unit_onset <- paste(normalize_name(df$institution_name), df$began)
  df$flag_same_unit_onset <- unit_onset %in% unit_onset[duplicated(unit_onset)]
  df$flag_name_truncated <- nchar(df$institution_name) == 60 & df$source_file_year <= 2021
  df$flag_zero_duration <- !df$is_open & df$duration_days == 0
  df$flag_pending_closed <- !df$is_open & (df$agent1_raw == "Pending" | df$agent2_raw == "Pending")
  df
}

#' Full cleaning pipeline.
#'
#' @param raw_list Named list of raw annual tibbles (from read_raw_files()).
#' @param as_of Data-as-of date used to censor open outbreaks.
#' @param agent_lookup From load_agent_lookup().
#' @param address_crosswalk From load_address_crosswalk().
#' @return One row per outbreak, with attribute "n_duplicates_removed".
clean_outbreaks <- function(raw_list, as_of, agent_lookup, address_crosswalk) {
  df <- stack_raw(raw_list)
  if (anyNA(df$began)) stop("Unparseable 'Date Outbreak Began' values.", call. = FALSE)
  df <- dedup_outbreaks(df)
  n_removed <- attr(df, "n_removed")

  df <- df |>
    harmonize_outbreak_type() |>
    harmonize_setting() |>
    harmonize_agents(agent_lookup) |>
    add_facility(address_crosswalk) |>
    add_duration(as_of) |>
    add_season() |>
    add_quality_flags()
  df$outbreak_id <- make_outbreak_id(df)

  df <- df |>
    dplyr::relocate("outbreak_id", "unit_id", "facility_id", "facility_name",
                    "institution_name", "outbreak_type", "setting_group",
                    "hospital_subtype", "in_scope", "began", "declared_over",
                    "is_open", "duration_days", "event", "agent_groups",
                    "agent_mix_group", "co_detection", "season", "season_week") |>
    dplyr::arrange(.data$began, .data$institution_name)
  attr(df, "n_duplicates_removed") <- n_removed
  df
}

#' One row per facility: canonical address, setting group, display name,
#' outbreak counts and first/last onset.
summarise_facilities <- function(clean) {
  clean |>
    dplyr::group_by(.data$facility_id, .data$facility_name, .data$facility_address,
                    .data$setting_group) |>
    dplyr::summarise(
      n_outbreaks = dplyr::n(),
      n_units = dplyr::n_distinct(.data$unit_id),
      n_address_spellings = dplyr::n_distinct(.data$institution_address),
      first_onset = min(.data$began),
      last_onset = max(.data$began),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$setting_group, .data$facility_name)
}
