# Pre-flight checks on the downloaded files, run before cleaning.
#
# Collects every problem (instead of stopping at the first) so that the
# GitHub issue opened on failure explains everything that changed at once.
# Failures stop the weekly build; warnings are reported but do not.

#' Check raw files against the expected schema and the reference tables.
#'
#' @return list(failures = character(), warnings = character())
check_inputs <- function(raw_list, agent_lookup, refreshed, today = Sys.Date()) {
  failures <- character()
  warnings <- character()

  # 1. Columns (the 2020 "Causative Agent - 1" variant is known and allowed)
  for (year in names(raw_list)) {
    found <- names(standardize_raw_names(raw_list[[year]]))
    if (!identical(found, EXPECTED_RAW_COLUMNS)) {
      added <- setdiff(found, EXPECTED_RAW_COLUMNS)
      missing <- setdiff(EXPECTED_RAW_COLUMNS, found)
      failures <- c(failures, sprintf(
        "**Schema change in ob_report_%s.** New/renamed columns: %s. Missing columns: %s.%s",
        year,
        if (length(added)) paste0("`", added, "`", collapse = ", ") else "none",
        if (length(missing)) paste0("`", missing, "`", collapse = ", ") else "none",
        if (!length(added) && !length(missing)) " (Column order changed.)" else ""))
    }
  }
  if (length(failures)) return(list(failures = failures, warnings = warnings))

  raw <- stack_raw(raw_list)

  # 2. Values that the reference tables must cover
  new_agents <- setdiff(unique(c(raw$agent1_raw, raw$agent2_raw)), c("", agent_lookup$raw_label))
  if (length(new_agents)) {
    failures <- c(failures, paste0(
      "**New causative agent label(s)** not in `data/reference/agent_lookup.csv`: ",
      paste0("`", new_agents, "`", collapse = ", "),
      ". Add each with its analysis group(s)."))
  }
  new_settings <- setdiff(unique(raw$setting_raw), SETTING_MAP$setting_raw)
  if (length(new_settings)) {
    failures <- c(failures, paste0("**New Outbreak Setting value(s):** ",
                                   paste0("`", new_settings, "`", collapse = ", "),
                                   ". Add to `SETTING_MAP` in `R/clean_setting.R`."))
  }
  new_types <- setdiff(unique(raw$type_raw), names(OUTBREAK_TYPE_MAP))
  if (length(new_types)) {
    failures <- c(failures, paste0("**New Type of Outbreak value(s):** ",
                                   paste0("`", new_types, "`", collapse = ", "),
                                   ". Add to `OUTBREAK_TYPE_MAP` in `R/clean_setting.R`."))
  }

  # 3. Dates
  bad_began <- sum(is.na(raw$began))
  bad_over <- sum(is.na(raw$declared_over) & raw$active_raw != "Y")
  if (bad_began) failures <- c(failures, sprintf("**%d onset dates** could not be parsed.", bad_began))
  if (bad_over) failures <- c(failures, sprintf(
    "**%d closed outbreaks** (Active = N) have no parseable declared-over date.", bad_over))
  as_of <- data_as_of(refreshed)
  future <- raw[!is.na(raw$began) & raw$began > as_of, ]
  if (nrow(future)) failures <- c(failures, sprintf(
    "**%d outbreak(s) begin after the data-as-of date (%s):** %s.", nrow(future), as_of,
    paste0("`", future$institution_name, "` (", future$began, ")", collapse = ", ")))
  n_backwards <- sum(raw$declared_over < raw$began, na.rm = TRUE)
  if (n_backwards) warnings <- c(warnings, sprintf(
    "%d outbreak(s) declared over before they began.", n_backwards))

  # 4. Freshness: the City refreshes weekly on Thursdays
  age <- as.integer(as.Date(today) - as.Date(refreshed))
  if (age > 8) warnings <- c(warnings, sprintf(
    "The City's data were last refreshed %s (%d days ago); the weekly refresh may have been missed.",
    format(as.Date(refreshed)), age))

  list(failures = failures, warnings = warnings)
}

#' Format check results as Markdown (used as the GitHub issue body).
format_check_report <- function(result, refreshed) {
  lines <- c(sprintf("Input check for the City refresh of %s.", format(refreshed, "%Y-%m-%d %H:%M UTC")), "")
  if (length(result$failures)) {
    lines <- c(lines, "### Failures (the weekly build stopped)", "",
               paste0("- ", result$failures), "")
  }
  if (length(result$warnings)) {
    lines <- c(lines, "### Warnings", "", paste0("- ", result$warnings), "")
  }
  if (!length(result$failures) && !length(result$warnings)) lines <- c(lines, "All checks passed.")
  paste(lines, collapse = "\n")
}
