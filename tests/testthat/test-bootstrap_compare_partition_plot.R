test_that("bootstrap, compare, partition and plots run", {
  dat <- prepare_growth_data(growth_wide_example)

  boot_out <- bootstrap_growth(
    dat,
    metric = "rgr",
    group_var = "treatment",
    resample_unit = "plot",
    times = 50,
    seed = 42,
    epsilon = 1e-6
  )

  cmp <- compare_growth(dat, metric = "rgr", group_var = "treatment", epsilon = 1e-6)
  cmp_lai <- compare_growth(dat, metric = "lai", group_var = "treatment", epsilon = 1e-6)
  part <- biomass_partition(dat, group_cols = c("treatment", "time"))

  fit <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = c("treatment"), method = "gam")
  p1 <- plot_growth_curve(fit)

  classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"), epsilon = 1e-6)
  p2 <- plot_growth_rates(classic, type = "classic", metrics = c("rgr", "agr"))
  p3 <- plot_partition(part, facet_var = "treatment")

  expect_true(nrow(boot_out) > 0)
  expect_true(nrow(cmp) > 0)
  expect_true(nrow(cmp_lai) > 0)
  expect_true(nrow(part) > 0)
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("main workflow does not error", {
  expect_error({
    dat <- prepare_growth_data(growth_wide_example)
    check_growth_data(dat)
    classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"), epsilon = 1e-6)
    fit_w <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = c("treatment"), method = "gam")
    fit_la <- fit_growth_curve(dat, response = "leaf_area_cm2", group_cols = c("treatment"), method = "gam")
    calc_instant_rates(fit_w, leaf_area_fit = fit_la, epsilon = 1e-6)
    biomass_partition(dat)
    compare_growth(dat, metric = "rgr", group_var = "treatment", epsilon = 1e-6)
    bootstrap_growth(dat, metric = "rgr", times = 20, seed = 1, epsilon = 1e-6)
  }, NA)
})
