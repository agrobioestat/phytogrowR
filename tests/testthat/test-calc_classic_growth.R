test_that("calc_classic_growth computes expected formulas", {
  dat <- tibble::tibble(
    treatment = "A",
    genotype = "G1",
    block = "B1",
    plot_id = "P1",
    plant_id = "Pl1",
    time = c(10, 20),
    total_biomass_g = c(2, 4),
    leaf_area_cm2 = c(20, 30),
    leaf_biomass_g = c(1, 1.8),
    stem_biomass_g = c(0.6, 1.4),
    root_biomass_g = c(0.4, 0.8),
    reproductive_biomass_g = c(0, 0),
    ground_area_m2 = c(0.5, 0.5)
  )

  out <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"))

  expect_equal(nrow(out), 1)
  expect_equal(out$rgr, (log(4) - log(2)) / 10, tolerance = 1e-8)
  expect_equal(out$agr, (4 - 2) / 10, tolerance = 1e-8)
  expect_equal(out$nar, ((4 - 2) / 10) * ((log(30) - log(20)) / (30 - 20)), tolerance = 1e-8)
  expect_equal(out$algr, (30 - 20) / 10, tolerance = 1e-8)
  expect_equal(out$rlgr, (log(30) - log(20)) / 10, tolerance = 1e-8)
  expect_equal(out$lar, ((20 / 2) + (30 / 4)) / 2, tolerance = 1e-8)
  expect_equal(out$lai, (((20 / 10000) / 0.5) + ((30 / 10000) / 0.5)) / 2, tolerance = 1e-8)
  expect_equal(out$lad, ((20 + 30) / 2) * 10, tolerance = 1e-8)
  expect_equal(out$sla, 20 / 1, tolerance = 1e-8)
  expect_equal(out$lmr, 1 / 2, tolerance = 1e-8)
  expect_equal(out$lwr, out$lmr, tolerance = 1e-8)
  expect_equal(out$smr, 0.6 / 2, tolerance = 1e-8)
  expect_equal(out$swr, out$smr, tolerance = 1e-8)
  expect_equal(out$rmr, 0.4 / 2, tolerance = 1e-8)
  expect_equal(out$cgr, (4 - 2) / (10 * 0.5), tolerance = 1e-8)
})
