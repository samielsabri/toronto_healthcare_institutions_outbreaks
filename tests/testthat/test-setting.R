test_that("settings map to groups and subtypes (decision D1)", {
  df <- tibble::tibble(setting_raw = c("Hospital-Acute Care", "Transitional Care", "LTCH",
                                       "Retirement Home", "Shelter"))
  out <- harmonize_setting(df)
  expect_equal(out$setting_group, c("Hospital", "Hospital", "LTCH", "Retirement home", "Shelter"))
  expect_equal(out$hospital_subtype, c("Acute care", "Transitional care", NA, NA, NA))
  expect_equal(out$in_scope, c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("an unknown setting stops the pipeline", {
  expect_error(harmonize_setting(tibble::tibble(setting_raw = "Hospice")), "Hospice")
})

test_that("outbreak types map, and unknown types stop the pipeline", {
  out <- harmonize_outbreak_type(tibble::tibble(type_raw = c("Respiratory", "Enteric", "Other")))
  expect_equal(out$outbreak_type, c("Respiratory", "Enteric", "Other"))
  expect_error(harmonize_outbreak_type(tibble::tibble(type_raw = "Skin")), "Skin")
})
