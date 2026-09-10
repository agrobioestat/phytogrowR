test_that("fit_growth_curve returns phytogrow_fit object", {
  dat <- prepare_growth_data(growth_wide_example)

  fit <- fit_growth_curve(
    dat,
    response = "total_biomass_g",
    group_cols = c("treatment"),
    method = "gam",
    n_grid = 50
  )

  expect_s3_class(fit, "phytogrow_fit")
  expect_true(nrow(fit$predictions) > 0)
})

test_that("instantaneous rates are computed", {
  dat <- prepare_growth_data(growth_wide_example)

  fit_w <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = c("treatment"), method = "spline")
  fit_la <- fit_growth_curve(dat, response = "leaf_area_cm2", group_cols = c("treatment"), method = "spline")

  inst <- calc_instant_rates(fit_w, leaf_area_fit = fit_la, epsilon = 1e-6)

  expect_true(nrow(inst) > 0)
  expect_true(all(c("agr_inst", "rgr_inst", "algr_inst", "rlgr_inst", "nar_inst", "lar_inst", "lai_inst") %in% names(inst)))
})

test_that("S3 methods return expected structures", {
  dat <- prepare_growth_data(growth_wide_example)
  fit <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = c("treatment"), method = "gam")

  sm <- summary(fit)
  td <- tidy.phytogrow_fit(fit)
  ag <- augment.phytogrow_fit(fit)
  gl <- glance.phytogrow_fit(fit)

  expect_s3_class(sm, "summary.phytogrow_fit")
  expect_true(is.data.frame(td))
  expect_true(is.data.frame(ag))
  expect_true(is.data.frame(gl))
})
