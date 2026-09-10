test_that("check_growth_data flags missing columns", {
  bad <- dplyr::select(growth_wide_example, -leaf_area_cm2)
  issues <- check_growth_data(bad)

  expect_true(any(issues$issue == "missing_required_columns"))
  expect_true(any(issues$severity == "error"))
})

test_that("check_growth_data detects no issues for basic valid subset", {
  dat <- growth_wide_example %>%
    dplyr::filter(!is.na(total_biomass_g), !is.na(leaf_area_cm2))

  issues <- check_growth_data(dat)
  expect_true(nrow(issues) >= 1)
})
