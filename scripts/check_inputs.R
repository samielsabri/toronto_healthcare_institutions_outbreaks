# Weekly step 2: check the downloaded files before cleaning.
# Writes pipeline_check.md (used as the GitHub issue body if the run fails)
# and exits with status 1 if there is any failure.
# Run from the project root:  Rscript scripts/check_inputs.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

manifest <- readr::read_csv("data/raw/manifest.csv", show_col_types = FALSE,
                            col_types = readr::cols(.default = "c"))
refreshed <- as.POSIXct(unique(manifest$package_last_refreshed), tz = "UTC",
                        format = "%Y-%m-%dT%H:%M:%SZ")

result <- check_inputs(read_raw_files("data/raw"), load_agent_lookup(), refreshed)
report <- format_check_report(result, refreshed)
writeLines(report, "pipeline_check.md")
message(report)

if (length(result$warnings)) {
  cat(paste0("- ", result$warnings, "\n"), file = "pipeline_warnings.md", append = TRUE, sep = "")
}
if (length(result$failures)) quit(status = 1)
