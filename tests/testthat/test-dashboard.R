clean <- nanoparquet::read_parquet(root_path("data", "clean", "outbreaks.parquet"))
facilities <- readr::read_csv(root_path("data", "clean", "facilities.csv"), show_col_types = FALSE)
info <- freshness_info(root_path("data", "raw", "manifest.csv"))
x <- assemble_dashboard_data(clean, facilities, info)

test_that("the payload has every in-scope outbreak, with valid facility and season indices", {
  n <- sum(clean$in_scope)
  o <- x$outbreaks
  expect_equal(length(o$fac), n)
  expect_true(all(lengths(o) == n))
  expect_true(all(o$fac >= 0 & o$fac < length(x$facilities$id)))
  expect_true(all(o$season >= 0 & o$season < length(x$seasons)))
  expect_true(all(o$week %in% 1:52))
  expect_false(anyNA(x$facilities$lat))
  expect_equal(x$seasons[x$current$season_idx + 1], x$current$season)
  expect_equal(sum(is.na(o$over)), sum(clean$is_open & clean$in_scope))
})

test_that("agent flags encode the D4 counting (every group detected)", {
  covid_bit <- which(x$agent_groups == "COVID-19") - 1
  expect_equal(sum(bitwAnd(x$outbreaks$flags, 2^covid_bit) > 0),
               sum(clean$agent_covid19 & clean$in_scope))
})

test_that("the calendar has 52 weeks per season, week 53 merged into week 18 (D7)", {
  expect_true(all(vapply(x$calendar, function(c) length(c$start), integer(1)) == 52))
  s2020 <- x$calendar[[which(x$seasons == "2020/21")]]
  expect_equal(c(s2020$start[18], s2020$end[18]), c("2020-12-21", "2021-01-03"))  # ISO weeks 52-53
  expect_equal(s2020$start[19], "2021-01-04")                                     # ISO week 1
  s19 <- x$calendar[[which(x$seasons == "2019/20")]]
  expect_equal(s19$period[c(28, 29)], c("pre", "pan"))                            # D6 cut at ISO week 10
})

test_that("Kaplan-Meier curves start near 1 and never increase", {
  # day 0 can sit just below 1: one outbreak was declared over the day it began
  for (v in names(x$km)) for (g in x$km[[v]]) {
    expect_true(g$s[1] <= 1 && g$s[1] > 0.99, info = paste(v, g$name))
    expect_true(all(diff(g$s) <= 0), info = paste(v, g$name))
  }
})

test_that("inlined JSON cannot close the script element", {
  expect_false(grepl("</", to_inline_json(list(a = "</script><script>alert(1)</script>")), fixed = TRUE))
})

test_that("placeholders are replaced literally and the page is complete", {
  expect_equal(replace_once("a /*__X__*/ b", "/*__X__*/", "c\\d $1"), "a c\\d $1 b")
  expect_error(replace_once("abc", "/*__X__*/", "y"), "placeholder missing")
  out <- tempfile(fileext = ".html")
  render_dashboard(x, out, template = root_path("dashboard", "template.html"),
                   vendor = root_path("dashboard", "vendor"),
                   tiles = list(url = "https://example.org/{z}/{x}/{y}.png", attribution = "test"))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("/*__", html, fixed = TRUE))
  expect_true(grepl("Chart.js v4", html, fixed = TRUE))
  expect_true(grepl("Leaflet 1.9.4", html, fixed = TRUE))
})

test_that("the basemap uses CARTO only when a key is set, and never hard-codes one", {
  expect_match(basemap_tiles("abc 123")$url, "rastertiles/light_all/.*\\?key=abc%20123$")
  expect_match(basemap_tiles("")$url, "tile.openstreetmap.org")
  src <- paste(readLines(root_path("R", "dashboard.R")), collapse = "\n")
  expect_false(grepl("cb1_", src, fixed = TRUE))
})
