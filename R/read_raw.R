# Read the raw annual files exactly as published, for profiling.
#
# Every column is read as character and empty strings are kept as "" (not
# converted to NA), so that profiling sees the data as the City wrote it,
# including placeholders such as "None" or "Unable to identify".

#' Read one raw annual file as all-character, keeping blanks as "".
read_raw_file <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    progress = FALSE
  )
}

#' Read all raw annual files into a named list (one tibble per year).
read_raw_files <- function(dir = "data/raw") {
  paths <- list.files(dir, pattern = "^ob_report_\\d{4}\\.csv$", full.names = TRUE)
  if (length(paths) == 0) stop("No ob_report_YYYY.csv files in ", dir, call. = FALSE)
  years <- as.integer(stringr::str_extract(basename(paths), "\\d{4}"))
  stats::setNames(lapply(paths, read_raw_file), years)
}

#' Map known historical column-name variants to the canonical names.
#'
#' The 2020 file uses "Causative Agent - 1" / "Causative Agent - 2" (spaces
#' around the hyphen); every other year uses "Causative Agent-1" / "-2".
standardize_raw_names <- function(df) {
  names(df) <- sub("^Causative Agent - ([12])$", "Causative Agent-\\1", names(df))
  df
}
