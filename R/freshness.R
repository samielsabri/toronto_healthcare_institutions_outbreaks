# Data freshness: what the published page says about how current the data are.

#' Freshness facts from the raw manifest.
#'
#' @return list(refreshed = POSIXct, as_of = Date, downloaded = POSIXct)
freshness_info <- function(manifest_path = "data/raw/manifest.csv") {
  m <- readr::read_csv(manifest_path, col_types = readr::cols(.default = "c"))
  refreshed <- as.POSIXct(unique(m$package_last_refreshed), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  list(
    refreshed = refreshed,
    as_of = data_as_of(refreshed),
    downloaded = as.POSIXct(max(m$downloaded_at), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' HTML banner. "Weeks behind" is computed in the viewer's browser, so the
#' banner stays truthful even if the weekly build stops and the page is not
#' re-rendered. Expected data-as-of date = the most recent Wednesday before
#' today (the City refreshes on Thursdays).
freshness_banner_html <- function(info) {
  as_of <- format(info$as_of, "%Y-%m-%d")
  sprintf(
    '<div class="freshness-banner" data-as-of="%s">
  <strong>Data as of %s</strong> (City of Toronto refresh %s).
  <span class="freshness-lag"></span>
</div>
<script>
(function () {
  var el = document.querySelector(".freshness-banner");
  var asOf = new Date(el.dataset.asOf + "T00:00:00");
  var today = new Date(); today.setHours(0, 0, 0, 0);
  var back = ((today.getDay() - 3 + 7) %% 7) || 7;   // days since the last Wednesday before today
  var expected = new Date(today); expected.setDate(today.getDate() - back);
  var weeks = Math.round((expected - asOf) / (7 * 864e5));
  var lag = el.querySelector(".freshness-lag");
  if (weeks <= 0) { lag.textContent = "Up to date with the weekly refresh."; }
  else {
    lag.textContent = weeks + (weeks === 1 ? " week" : " weeks") + " behind the expected weekly refresh.";
    el.classList.add("is-stale");
  }
})();
</script>',
    as_of, format(info$as_of, "%A %e %B %Y"), format(info$refreshed, "%e %B %Y %H:%M UTC")
  )
}
