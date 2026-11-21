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
    # The div stays on one line: inside a dashboard card, line breaks in raw
    # HTML are turned into paragraph breaks.
    '<div class="freshness-banner" data-as-of="%s"><strong>Data as of %s</strong> &middot; <span class="freshness-lag"></span><div class="freshness-source">City of Toronto Open Data, refreshed %s &middot; Open Government Licence &ndash; Toronto &middot; unofficial</div></div>
<script>
(function () {
  document.querySelectorAll(".freshness-banner").forEach(function (el) {
    var asOf = new Date(el.dataset.asOf + "T00:00:00");
    var today = new Date(); today.setHours(0, 0, 0, 0);
    var back = ((today.getDay() - 3 + 7) %% 7) || 7;  /* days since the last Wednesday before today */
    var expected = new Date(today); expected.setDate(today.getDate() - back);
    var weeks = Math.round((expected - asOf) / (7 * 864e5));
    var lag = el.querySelector(".freshness-lag");
    if (weeks <= 0) { lag.textContent = "up to date"; }
    else {
      lag.textContent = weeks + (weeks === 1 ? " week" : " weeks") + " behind the weekly refresh";
      el.classList.add("is-stale");
    }
  });
})();
</script>',
    as_of, format(info$as_of, "%a %e %b %Y"), format(info$refreshed, "%e %b %Y")
  )
}
