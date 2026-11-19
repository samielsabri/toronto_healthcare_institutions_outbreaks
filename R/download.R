# Download helpers for the "Outbreaks in Toronto Healthcare Institutions"
# package on City of Toronto Open Data.
#
# Resources are resolved by *name* (ob_report_YYYY) via opendatatoronto, never
# by hard-coded resource ID, because the City re-creates resources and IDs
# change. Files are saved byte-for-byte as served by the CKAN datastore CSV
# export, so data/raw/ is an untouched copy of the source.

OUTBREAK_PACKAGE <- "outbreaks-in-toronto-healthcare-institutions"
CKAN_BASE_URL    <- "https://ckan0.cf.opendata.inter.prod-toronto.ca"

#' List the annual datastore resources (ob_report_YYYY) in the package.
#'
#' The package also contains static file copies (ob_report_YYYY.csv/.xml/.json);
#' those are excluded. Returns a tibble with name, id and year.
list_outbreak_resources <- function(package = OUTBREAK_PACKAGE) {
  res <- opendatatoronto::list_package_resources(package)
  res <- dplyr::filter(res, grepl("^ob_report_\\d{4}$", .data$name))
  if (nrow(res) == 0) {
    stop("No ob_report_YYYY resources found in package '", package, "'.",
         call. = FALSE)
  }
  res |>
    dplyr::mutate(year = as.integer(sub("^ob_report_", "", .data$name))) |>
    dplyr::select("name", "id", "year") |>
    dplyr::arrange(.data$year)
}

#' When the City last refreshed the package (CKAN `last_refreshed`, UTC).
#'
#' Read from the CKAN API directly because opendatatoronto::show_package()
#' fails on this package's metadata (v0.1.6).
get_package_last_refreshed <- function(package = OUTBREAK_PACKAGE) {
  url <- sprintf("%s/api/3/action/package_show?id=%s", CKAN_BASE_URL, package)
  meta <- jsonlite::fromJSON(url)
  if (!isTRUE(meta$success)) stop("CKAN package_show failed for ", package, call. = FALSE)
  as.POSIXct(meta$result$last_refreshed, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
}

#' Download one datastore resource as CSV, unchanged, to `dest`.
download_resource_csv <- function(resource_id, dest) {
  url <- sprintf("%s/datastore/dump/%s", CKAN_BASE_URL, resource_id)
  curl::curl_download(url, dest, quiet = TRUE)
  invisible(dest)
}

#' Download all annual files to `dir` and write a provenance manifest.
#'
#' @param years Optional integer vector to restrict which years are fetched.
#' @return The manifest tibble (one row per file), invisibly.
download_all_raw <- function(dir = "data/raw", years = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  resources <- list_outbreak_resources()
  if (!is.null(years)) resources <- dplyr::filter(resources, .data$year %in% years)

  last_refreshed <- format(get_package_last_refreshed(), "%Y-%m-%dT%H:%M:%SZ")
  downloaded_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  manifest <- purrr::pmap_dfr(resources, function(name, id, year) {
    dest <- file.path(dir, paste0(name, ".csv"))
    download_resource_csv(id, dest)
    message(sprintf("  %s -> %s (%s bytes)", name, dest, file.size(dest)))
    tibble::tibble(
      resource_name = name,
      resource_id   = id,
      year          = year,
      file          = basename(dest),
      bytes         = file.size(dest),
      md5           = unname(tools::md5sum(dest)),
      downloaded_at = downloaded_at,
      package_last_refreshed = last_refreshed
    )
  })
  readr::write_csv(manifest, file.path(dir, "manifest.csv"))
  invisible(manifest)
}
