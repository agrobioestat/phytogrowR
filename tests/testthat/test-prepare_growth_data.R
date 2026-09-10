test_that("prepare_growth_data parses comma decimal numeric strings", {
  raw <- tibble::tibble(
    plant_id = c("P1", "P2"),
    plot_id = c("A1", "A1"),
    treatment = c("T1", "T1"),
    genotype = c("G1", "G1"),
    time = c("7,0", "14,0"),
    total_biomass_g = c("1,25", "2,50"),
    leaf_biomass_g = c("0,50", "0,90"),
    stem_biomass_g = c("0,45", "1,00"),
    root_biomass_g = c("0,30", "0,60"),
    leaf_area_cm2 = c("12,30", "25,80"),
    ground_area_m2 = c("0,04", "0,04")
  )

  dat <- prepare_growth_data(raw, quiet = TRUE)

  expect_true(is.numeric(dat$time))
  expect_true(is.numeric(dat$total_biomass_g))
  expect_true(is.numeric(dat$leaf_area_cm2))

  expect_equal(dat$total_biomass_g, c(1.25, 2.50), tolerance = 1e-8)
  expect_equal(dat$leaf_area_cm2, c(12.30, 25.80), tolerance = 1e-8)
})
