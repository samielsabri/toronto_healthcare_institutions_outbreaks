test_that("unit suffixes are removed but street-number ranges are kept", {
  expect_equal(normalize_address("1001 Queen Street West - Unit 3-5"), "1001 QUEEN ST W")
  expect_equal(normalize_address("3030 Lawrence Ave. E.- T 9-Medicine"), "3030 LAWRENCE AVE E")
  expect_equal(normalize_address("2867 Ellesmere Road -General Medicine Unit"), "2867 ELLESMERE RD")
  expect_equal(normalize_address("705 - 707 Ellesmere Road"), "705-707 ELLESMERE RD")
  expect_equal(normalize_address("10 -14 William Morgan Drive"), "10-14 WILLIAM MORGAN DR")
  expect_equal(normalize_address("10/14 William Morgan Dr"), "10-14 WILLIAM MORGAN DR")
  expect_equal(normalize_address("700 & 720 Mount Pleasant Rd - 6th Fl"), "700-720 MOUNT PLEASANT RD")
})

test_that("street types, directions and punctuation are standardized", {
  expect_equal(normalize_address("2701 St. Clair Avenue East"), "2701 ST CLAIR AVE E")
  expect_equal(normalize_address("60 Guildwood Parkway"), "60 GUILDWOOD PKWY")
  expect_equal(normalize_address("1800 O'Connor Dr"), "1800 O CONNOR DR")
  expect_equal(normalize_address("  2 Buchan   Court "), "2 BUCHAN CRT")
})

test_that("the crosswalk maps listed variants and leaves others unchanged", {
  xw <- tibble::tibble(addr_norm = "5395 BATHURST ST", addr_canonical = "5935 BATHURST ST", reason = "typo")
  expect_equal(canonical_address(c("5395 BATHURST ST", "399 BATHURST ST"), xw),
               c("5935 BATHURST ST", "399 BATHURST ST"))
  expect_error(canonical_address("X", rbind(xw, xw)), "duplicate")
})

test_that("the project crosswalk has no chains and every entry is used", {
  xw <- load_address_crosswalk(root_path("data", "reference", "address_crosswalk.csv"))
  expect_false(any(xw$addr_canonical %in% xw$addr_norm))
  raw <- stack_raw(read_raw_files(root_path("data", "raw")))
  expect_true(all(xw$addr_norm %in% normalize_address(raw$institution_address)))
})

test_that("name stems drop only the last unit segment", {
  expect_equal(name_stem("Baycrest Apotex - 2RF"), "BAYCREST APOTEX")
  expect_equal(name_stem("William Osler Health System - Etobicoke General Hospital - 7E"),
               "WILLIAM OSLER HEALTH SYSTEM - ETOBICOKE GENERAL HOSPITAL")
  expect_equal(name_stem("Seaton House"), "SEATON HOUSE")
})

test_that("facility = physical site x setting group (decision D5)", {
  df <- tibble::tibble(
    institution_name = c("Home A - 2nd Fl", "Home A - 3rd Fl", "Home A Retirement", "Home B"),
    institution_address = c("1 Main St", "1 Main Street - 3rd Fl", "1 Main St", "2 Main St"),
    setting_group = c("LTCH", "LTCH", "Retirement home", "LTCH"),
    began = as.Date("2024-01-01") + 0:3
  )
  xw <- tibble::tibble(addr_norm = character(), addr_canonical = character(), reason = character())
  out <- add_facility(df, xw)
  expect_equal(out$facility_id[1], out$facility_id[2])          # two units, one site
  expect_false(out$facility_id[1] == out$facility_id[3])        # same address, different setting
  expect_false(out$facility_id[1] == out$facility_id[4])        # different address
  expect_false(out$unit_id[1] == out$unit_id[2])                 # units kept distinct
  expect_equal(out$facility_id[1], "1-main-st--ltch")
})
