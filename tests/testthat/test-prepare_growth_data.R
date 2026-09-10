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

test_that("column mapping renames user columns to the standard names", {
  # Regression: the rename was inverted, so `mapping` only ever worked for data
  # that already used the standard names, i.e. exactly when it was not needed.
  raw <- data.frame(
    dias = rep(c(10, 20, 30, 40), each = 3),
    massa_total = c(1, 1.1, 1.2, 3, 3.2, 3.1, 7, 7.2, 6.8, 11, 11.3, 10.9),
    area_foliar = c(10, 11, 12, 30, 32, 31, 70, 72, 68, 110, 113, 109),
    trat = rep(c("A", "B", "C"), 4),
    stringsAsFactors = FALSE
  )

  out <- prepare_growth_data(
    raw,
    mapping = list(
      time = "dias",
      total_biomass_g = "massa_total",
      leaf_area_cm2 = "area_foliar",
      treatment = "trat"
    )
  )

  expect_true(all(c("time", "total_biomass_g", "leaf_area_cm2", "treatment") %in% names(out)))
  expect_equal(sort(unique(out$time)), c(10, 20, 30, 40))
  expect_equal(sort(unique(out$treatment)), c("A", "B", "C"))
  expect_equal(sum(out$total_biomass_g), sum(raw$massa_total))

  # The original names must be gone, not duplicated alongside the new ones.
  expect_false(any(c("dias", "massa_total", "area_foliar", "trat") %in% names(out)))
})

test_that("an unmapped dataset with standard names is unchanged by the mapping step", {
  dat <- prepare_growth_data(growth_wide_example)
  same <- prepare_growth_data(growth_wide_example, mapping = list(time = "time"))
  expect_equal(names(dat), names(same))
  expect_equal(dat$time, same$time)
})

test_that("a mapping that shadows an existing column warns instead of failing", {
  raw <- data.frame(
    time = 1:4,                      # a decoy already called `time`
    dias = c(10, 20, 30, 40),
    total_biomass_g = c(1, 3, 7, 11),
    leaf_area_cm2 = c(10, 30, 70, 110)
  )

  expect_warning(
    out <- prepare_growth_data(raw, mapping = list(time = "dias")),
    "dropped"
  )

  # The user said `dias` is the time column, so that is what `time` holds.
  expect_equal(out$time, c(10, 20, 30, 40))
  expect_false("dias" %in% names(out))
})

test_that("mapping two standard names onto one column is refused", {
  raw <- data.frame(x = 1:4, y = 1:4, z = 1:4)
  expect_error(
    prepare_growth_data(
      raw,
      mapping = list(time = "x", total_biomass_g = "x", leaf_area_cm2 = "y")
    ),
    "only once"
  )
})
