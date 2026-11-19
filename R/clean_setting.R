# Outbreak setting and outbreak type harmonization (decision D1).
#
# Unknown raw values stop the pipeline so that new categories are reviewed.

SETTING_MAP <- tibble::tribble(
  ~setting_raw,            ~setting_group,     ~hospital_subtype,   ~in_scope,
  "Hospital-Acute Care",   "Hospital",         "Acute care",        TRUE,
  "Hospital-Chronic Care", "Hospital",         "Chronic care",      TRUE,
  "Hospital-Psychiatric",  "Hospital",         "Psychiatric",       TRUE,
  "Transitional Care",     "Hospital",         "Transitional care", TRUE,
  "LTCH",                  "LTCH",             NA,                  TRUE,
  "Retirement Home",       "Retirement home",  NA,                  TRUE,
  "Shelter",               "Shelter",          NA,                  FALSE
)

OUTBREAK_TYPE_MAP <- c(Respiratory = "Respiratory", Enteric = "Enteric", Other = "Other")

#' Add setting_group, hospital_subtype and in_scope from setting_raw.
harmonize_setting <- function(df) {
  unknown <- setdiff(unique(df$setting_raw), SETTING_MAP$setting_raw)
  if (length(unknown)) {
    stop("Unmapped Outbreak Setting value(s): ", toString(unknown),
         ". Add them to SETTING_MAP in R/clean_setting.R.", call. = FALSE)
  }
  dplyr::left_join(df, SETTING_MAP, by = "setting_raw")
}

#' Add outbreak_type from type_raw.
harmonize_outbreak_type <- function(df) {
  unknown <- setdiff(unique(df$type_raw), names(OUTBREAK_TYPE_MAP))
  if (length(unknown)) {
    stop("Unmapped Type of Outbreak value(s): ", toString(unknown),
         ". Add them to OUTBREAK_TYPE_MAP in R/clean_setting.R.", call. = FALSE)
  }
  df$outbreak_type <- unname(OUTBREAK_TYPE_MAP[df$type_raw])
  df
}
