# Geocode facility addresses with the City of Toronto address register and
# add the City neighbourhood (158-neighbourhood model) for each facility.
# Writes data/reference/facility_geocodes.csv (committed).
#
# Run from the project root after scripts/03_clean.R, whenever new facility
# addresses appear (the weekly input check warns when one is missing):
#   Rscript scripts/geocode_facilities.R
# The two City source files (~185 MB) are downloaded to data/cache/ (not committed).

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

facilities <- readr::read_csv("data/clean/facilities.csv", show_col_types = FALSE)
geo_path <- "data/reference/facility_geocodes.csv"
if (file.exists(geo_path) && !("--all" %in% commandArgs(trailingOnly = TRUE))) {
  known <- readr::read_csv(geo_path, show_col_types = FALSE)
  if (all(facilities$facility_address %in% known$facility_address[known$match != "unmatched"])) {
    message("All facility addresses are already geocoded; nothing to do (use --all to redo).")
    quit(status = 0)
  }
}

cache <- "data/cache"
dir.create(cache, showWarnings = FALSE)
fetch_city_file <- function(package, resource_name, dest) {
  if (file.exists(dest)) return(invisible(dest))
  meta <- jsonlite::fromJSON(sprintf("%s/api/3/action/package_show?id=%s", CKAN_BASE_URL, package))
  res <- meta$result$resources
  url <- res$url[res$name == resource_name]
  if (length(url) != 1) stop("Resource '", resource_name, "' not found in ", package, call. = FALSE)
  message("Downloading ", resource_name, " ...")
  curl::curl_download(url, dest, quiet = TRUE)
  invisible(dest)
}
ap_file <- fetch_city_file("address-points-municipal-toronto-one-address-repository",
                           "Address Points - 4326.csv", file.path(cache, "address_points_4326.csv"))
nb_file <- fetch_city_file("neighbourhoods", "Neighbourhoods - 4326.geojson",
                           file.path(cache, "neighbourhoods_4326.geojson"))

geo <- geocode_facilities(facilities$facility_address, read_address_points(ap_file),
                          manual = load_geocode_manual())

nb <- sf::st_read(nb_file, quiet = TRUE) |>
  transmute(neighbourhood_id = as.integer(AREA_SHORT_CODE), neighbourhood = AREA_NAME)
geo <- add_neighbourhood(geo, nb)

readr::write_csv(geo, geo_path, na = "")
message(sprintf("Geocoded %d of %d facility addresses (%s).",
                sum(geo$match != "unmatched"), nrow(geo),
                paste(names(table(geo$match)), table(geo$match), sep = ": ", collapse = ", ")))
if (any(geo$match == "unmatched")) {
  um <- geo$facility_address[geo$match == "unmatched"]
  message("Unmatched (add to data/reference/geocode_manual.csv or the address crosswalk):\n  ",
          paste(um, collapse = "\n  "))
  # Reported as a warning in the weekly run's GitHub issue (not a failure)
  cat(paste0("- New facility address(es) not found in the City address register, so not on the map: ",
             paste0("`", um, "`", collapse = ", "),
             ". Add a lookup to `data/reference/geocode_manual.csv` or fix the address in the crosswalk.\n"),
      file = "pipeline_warnings.md", append = TRUE)
}
