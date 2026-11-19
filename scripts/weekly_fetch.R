# Weekly step 1: download every annual file to data/raw/ (unchanged) and save
# this week's snapshots to data/snapshots/.
# Run from the project root:  Rscript scripts/weekly_fetch.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

message("Downloading annual outbreak files from Toronto Open Data...")
manifest <- download_all_raw("data/raw")
message("City refresh: ", unique(manifest$package_last_refreshed))

saved <- save_snapshots("data/raw", "data/snapshots")
if (nrow(saved) == 0) {
  message("No new snapshot: this refresh is already saved and past years are unchanged.")
} else {
  message("Saved snapshot(s):\n  ", paste(saved$snapshot_file, collapse = "\n  "))
}
