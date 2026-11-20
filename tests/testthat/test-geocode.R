test_that("point geometry strings are parsed to lon/lat", {
  g <- parse_point_geometry(c('{"type": "Point", "coordinates": [-79.4335454175225, 43.7368]}', "bad"))
  expect_equal(g$lon, c(-79.4335454175225, NA))
  expect_equal(g$lat, c(43.7368, NA))
})

test_that("lookup keys use the first number of a range and the register's 'Ter'", {
  expect_equal(geocode_key(c("10-14 WILLIAM MORGAN DR", "15 SUMMERLAND TERR", "399 BATHURST ST")),
               c("10 WILLIAM MORGAN DR", "15 SUMMERLAND TER", "399 BATHURST ST"))
})

test_that("exact matches first, then hand-checked lookups, otherwise unmatched", {
  ap <- tibble::tibble(addr_key = c("399 BATHURST ST", "265 QUEEN S DR"), n_points = 1L,
                       address_point_id = c("1", "2"), lon = c(-79.40, -79.50), lat = c(43.65, 43.70),
                       place_name = NA_character_)
  manual <- tibble::tibble(facility_address = "265 QUEENS DR", register_address = "265 QUEEN S DR", note = "")
  g <- geocode_facilities(c("399 BATHURST ST", "265 QUEENS DR", "1 NOWHERE RD"), ap, manual)
  expect_equal(g$match[match(c("399 BATHURST ST", "265 QUEENS DR", "1 NOWHERE RD"), g$facility_address)],
               c("exact", "manual", "unmatched"))
  expect_equal(g$lon[g$facility_address == "265 QUEENS DR"], -79.50)
})

test_that("every facility in the cleaned data has a geocode inside Toronto", {
  geo <- readr::read_csv(root_path("data", "reference", "facility_geocodes.csv"), show_col_types = FALSE)
  fac <- readr::read_csv(root_path("data", "clean", "facilities.csv"), show_col_types = FALSE)
  expect_true(all(fac$facility_address %in% geo$facility_address))
  expect_false(any(geo$match == "unmatched"))
  expect_true(all(geo$lon > -79.65 & geo$lon < -79.10 & geo$lat > 43.57 & geo$lat < 43.86))
  expect_false(anyNA(geo$neighbourhood))
})
