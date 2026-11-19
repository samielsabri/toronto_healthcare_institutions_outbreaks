# Weekly snapshots of the source files.
#
# The City overwrites its files every week, so without our own copies there is
# no way to see when an outbreak first appeared (reporting delay) or what was
# changed afterwards (retrospective edits). Each week we keep:
#
#   data/snapshots/YYYY-MM-DD.csv
#       the current-year file, exactly as published, named by the City's
#       refresh date (not our download date);
#   data/snapshots/prior_years/YYYY-MM-DD_ob_report_YYYY.csv
#       an earlier year's file, only when its contents changed since the last
#       copy we kept (past years are edited rarely, e.g. when late outbreaks
#       are declared over in January);
#   data/snapshots/manifest.csv
#       one row per saved file: refresh time, data-as-of date, rows, md5.
#
# Saving is idempotent: re-running for the same refresh saves nothing new.

SNAPSHOT_MANIFEST_COLS <- c("snapshot_file", "resource_name", "year", "package_last_refreshed",
                            "data_as_of", "saved_at", "rows", "md5")

read_snapshot_manifest <- function(snap_dir) {
  path <- file.path(snap_dir, "manifest.csv")
  if (!file.exists(path)) {
    return(tibble::as_tibble(stats::setNames(
      replicate(length(SNAPSHOT_MANIFEST_COLS), character(), simplify = FALSE),
      SNAPSHOT_MANIFEST_COLS)))
  }
  readr::read_csv(path, col_types = readr::cols(.default = "c"))
}

#' Save this week's snapshots from the raw files.
#'
#' @param raw_dir Directory with ob_report_YYYY.csv files and manifest.csv
#'   (as written by download_all_raw()).
#' @param snap_dir Snapshot directory.
#' @param saved_at Timestamp recorded in the manifest.
#' @return Tibble of files saved this time (zero rows if nothing new).
save_snapshots <- function(raw_dir = "data/raw", snap_dir = "data/snapshots",
                           saved_at = Sys.time()) {
  raw_manifest <- readr::read_csv(file.path(raw_dir, "manifest.csv"),
                                  col_types = readr::cols(.default = "c"))
  refreshed <- unique(raw_manifest$package_last_refreshed)
  if (length(refreshed) != 1 || is.na(refreshed)) {
    stop("raw manifest must have exactly one package_last_refreshed value.", call. = FALSE)
  }
  refresh_time <- as.POSIXct(refreshed, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  refresh_date <- format(as.Date(refresh_time))
  as_of <- format(data_as_of(refresh_time))

  dir.create(file.path(snap_dir, "prior_years"), recursive = TRUE, showWarnings = FALSE)
  snap_manifest <- read_snapshot_manifest(snap_dir)
  current_year <- max(as.integer(raw_manifest$year))

  saved <- list()
  for (i in seq_len(nrow(raw_manifest))) {
    r <- raw_manifest[i, ]
    src <- file.path(raw_dir, r$file)
    md5 <- unname(tools::md5sum(src))
    is_current <- as.integer(r$year) == current_year

    if (is_current) {
      dest_rel <- paste0(refresh_date, ".csv")
      if (file.exists(file.path(snap_dir, dest_rel))) next  # this refresh is already saved
    } else {
      previous <- snap_manifest[snap_manifest$resource_name == r$resource_name, ]
      if (nrow(previous) && utils::tail(previous$md5, 1) == md5) next  # unchanged
      dest_rel <- file.path("prior_years", paste0(refresh_date, "_", r$resource_name, ".csv"))
      if (file.exists(file.path(snap_dir, dest_rel))) next
    }

    file.copy(src, file.path(snap_dir, dest_rel))
    saved[[length(saved) + 1]] <- tibble::tibble(
      snapshot_file = dest_rel,
      resource_name = r$resource_name,
      year = r$year,
      package_last_refreshed = refreshed,
      data_as_of = as_of,
      saved_at = format(saved_at, "%Y-%m-%dT%H:%M:%S%z"),
      rows = as.character(nrow(read_raw_file(src))),
      md5 = md5
    )
  }

  saved <- dplyr::bind_rows(saved)
  if (nrow(saved)) {
    readr::write_csv(dplyr::bind_rows(snap_manifest, saved), file.path(snap_dir, "manifest.csv"))
  }
  saved
}
