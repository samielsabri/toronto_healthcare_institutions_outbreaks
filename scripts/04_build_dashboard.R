# Build the dashboard: _site/index.html, one self-contained file with the
# data, Chart.js and Leaflet inlined (dashboard/template.html).
# Run from the project root after scripts/03_clean.R:
#   Rscript scripts/04_build_dashboard.R
# The basemap uses CARTO if CARTO_API_KEY is set (a GitHub Actions secret in
# the weekly build; locally, a git-ignored .Renviron), otherwise OpenStreetMap.

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

clean <- nanoparquet::read_parquet("data/clean/outbreaks.parquet")
facilities <- readr::read_csv("data/clean/facilities.csv", show_col_types = FALSE)
info <- freshness_info()

data <- assemble_dashboard_data(clean, facilities, info)
out <- render_dashboard(data, "_site/index.html")
message(sprintf("Dashboard written to %s (%.1f MB; data as of %s; basemap: %s).",
                out, file.size(out) / 1e6, info$as_of,
                if (nzchar(Sys.getenv("CARTO_API_KEY"))) "CARTO" else "OpenStreetMap"))
