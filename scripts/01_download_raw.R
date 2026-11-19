# Phase 1, step 1: download every annual outbreak file to data/raw/, unchanged.
# Run from the project root:  Rscript scripts/01_download_raw.R

source("R/download.R")

message("Downloading annual outbreak files from Toronto Open Data...")
manifest <- download_all_raw("data/raw")
message(sprintf("Done: %d files, %s bytes total. Manifest: data/raw/manifest.csv",
                nrow(manifest), format(sum(manifest$bytes), big.mark = ",")))
