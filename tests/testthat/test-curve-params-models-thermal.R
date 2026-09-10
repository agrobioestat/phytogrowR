# growth_curve_params() -------------------------------------------------------

test_that("landmarks of a logistic fit match the closed-form values", {
  # For W = Asym / (1 + exp((xmid - t)/scal)) the algebra is known exactly:
  # the inflection is at t = xmid, where W = Asym/2 and dW/dt = Asym/(4*scal).
  set.seed(11)
  tt <- rep(seq(5, 90, by = 5), each = 6)
  asym <- 20; xmid <- 45; scal <- 10

  sim <- data.frame(
    time = tt,
    treatment = "A",
    total_biomass_g = asym / (1 + exp((xmid - tt) / scal)) + stats::rnorm(length(tt), 0, 0.05)
  )

  fit <- fit_growth_curve(
    sim, response = "total_biomass_g", group_cols = "treatment",
    method = "logistic", n_grid = 4000
  )
  p <- growth_curve_params(fit)

  expect_equal(p$asymptote, asym, tolerance = 0.02)
  expect_equal(p$asymptote_source, "model parameter")
  expect_equal(p$t_inflection, xmid, tolerance = 0.5)
  expect_equal(p$y_inflection, asym / 2, tolerance = 0.3)
  expect_equal(p$max_agr, asym / (4 * scal), tolerance = 0.02)

  # The logistic is symmetric about xmid, so t10 and t90 are equidistant.
  expect_equal(p$t50, xmid, tolerance = 0.5)
  expect_equal(p$t50 - p$t10, p$t90 - p$t50, tolerance = 0.5)
})

test_that("a smoother reports the fitted maximum, not an invented asymptote", {
  dat <- prepare_growth_data(growth_wide_example)
  fit <- fit_growth_curve(dat, response = "total_biomass_g",
                          group_cols = "treatment", method = "gam")
  p <- growth_curve_params(fit)

  expect_true(all(p$asymptote_source == "maximum fitted value"))
  expect_equal(nrow(p), length(unique(dat$treatment)))
  expect_true(all(p$t10 <= p$t50, na.rm = TRUE))
  expect_true(all(p$t50 <= p$t90, na.rm = TRUE))
  expect_true(all(p$active_phase > 0, na.rm = TRUE))
})

test_that("plateau_reached is FALSE while the curve is still rising", {
  # An exponential never plateaus, so the flag must not claim otherwise.
  tt <- rep(seq(5, 40, by = 5), each = 5)
  sim <- data.frame(
    time = tt, treatment = "A",
    total_biomass_g = 2 * exp(0.07 * tt)
  )

  fit <- fit_growth_curve(sim, response = "total_biomass_g",
                          group_cols = "treatment", method = "spline")
  p <- growth_curve_params(fit)

  expect_false(isTRUE(p$plateau_reached))
  expect_equal(p$final_time, max(tt))
})

test_that("growth_curve_params validates its input", {
  expect_error(growth_curve_params(mtcars), "fit_growth_curve")

  dat <- prepare_growth_data(growth_wide_example)
  fit <- fit_growth_curve(dat, group_cols = "treatment", method = "gam")
  expect_error(growth_curve_params(fit, plateau_tol = 0), "between 0 and 1")
  expect_error(growth_curve_params(fit, plateau_tol = 2), "between 0 and 1")
})

test_that("first crossing interpolates between grid points", {
  x <- c(0, 10)
  y <- c(0, 20)
  expect_equal(phytogrowR:::.first_crossing(x, y, 10), 5)
  expect_true(is.na(phytogrowR:::.first_crossing(x, y, 50)))
})

# compare_growth_models() -----------------------------------------------------

test_that("the generating model is the one selected", {
  set.seed(202)
  tt <- rep(seq(5, 90, by = 5), each = 8)
  sim <- data.frame(
    time = tt,
    total_biomass_g = 20 / (1 + exp((45 - tt) / 10)) + stats::rnorm(length(tt), 0, 0.4)
  )

  res <- compare_growth_models(
    sim, response = "total_biomass_g",
    models = c("exponential", "logistic", "gompertz"),
    poly_degrees = 1
  )

  expect_equal(res$model[res$best], "logistic")
  expect_equal(res$delta[res$best], 0)
  expect_true(all(res$delta >= 0, na.rm = TRUE))

  # Ranking must follow the criterion actually requested.
  expect_equal(attr(res, "criterion"), "aic")
  expect_equal(res$aic, sort(res$aic))
})

test_that("candidates that fail to converge are kept and flagged", {
  # Two distinct time points cannot support a three-parameter sigmoid.
  hunt <- prepare_growth_data(hunt_classical_example)

  res <- compare_growth_models(
    hunt, response = "total_biomass_g", group_var = "treatment",
    models = c("logistic", "gompertz"), poly_degrees = 1:2
  )

  expect_true(any(!res$converged))
  # poly2 needs 3 distinct times; it must be reported, not silently dropped.
  expect_true("poly2" %in% res$model)
  expect_true(all(is.na(res$aic[!res$converged])))
  expect_true(all(!res$best[!res$converged]))
})

test_that("grouped and pooled forms agree on a single-group dataset", {
  dat <- prepare_growth_data(growth_wide_example)
  one <- dat[dat$treatment == "Control", ]

  grouped <- compare_growth_models(one, group_var = "treatment",
                                   models = "logistic", poly_degrees = 2)
  pooled <- compare_growth_models(one, models = "logistic", poly_degrees = 2)

  expect_true("treatment" %in% names(grouped))
  expect_false("treatment" %in% names(pooled))
  expect_equal(pooled$aic, grouped$aic)
})

test_that("compare_growth_models validates candidates and criterion", {
  dat <- prepare_growth_data(growth_wide_example)

  expect_error(
    compare_growth_models(dat, models = character(0), poly_degrees = integer(0)),
    "At least one candidate"
  )
  expect_error(compare_growth_models(dat, poly_degrees = 9), "between 1 and 5")
  expect_error(compare_growth_models(dat, criterion = "nope"))

  res <- compare_growth_models(dat, models = "logistic", poly_degrees = 2,
                               criterion = "bic")
  expect_equal(attr(res, "criterion"), "bic")
})

# thermal_time() --------------------------------------------------------------

weather_fixture <- function() {
  data.frame(
    date = as.Date("2024-01-01") + 0:9,
    tmin = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
    tmax = c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38)
  )
}

test_that("simple degree-days match the textbook formula", {
  w <- weather_fixture()
  out <- thermal_time(w, date = date, tmin = tmin, tmax = tmax, t_base = 10)

  expected <- pmax(0, (w$tmin + w$tmax) / 2 - 10)
  expect_equal(out$gdd, expected)
  expect_equal(out$thermal_time, cumsum(expected))
  expect_equal(out$t_mean_effective, (w$tmin + w$tmax) / 2)
})

test_that("cutoff caps the maximum and modified also lifts the minimum", {
  w <- weather_fixture()

  cut <- thermal_time(w, date = date, tmin = tmin, tmax = tmax,
                      t_base = 10, t_upper = 30, method = "cutoff")
  mod <- thermal_time(w, date = date, tmin = tmin, tmax = tmax,
                      t_base = 10, t_upper = 30, method = "modified")

  expect_equal(cut$t_mean_effective, (w$tmin + pmin(w$tmax, 30)) / 2)
  expect_equal(mod$t_mean_effective, (pmax(w$tmin, 10) + pmin(w$tmax, 30)) / 2)

  # Capping can only remove thermal time; lifting tmin can only add it back.
  expect_lt(max(cut$thermal_time), max(thermal_time(
    w, date = date, tmin = tmin, tmax = tmax, t_base = 10)$thermal_time))
  expect_gte(max(mod$thermal_time), max(cut$thermal_time))

  # The first day has tmin = 8, below t_base: that is where they differ.
  expect_gt(mod$gdd[1], cut$gdd[1])
})

test_that("cold days contribute zero rather than negative thermal time", {
  cold <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    tmin = c(-5, -3, 0), tmax = c(2, 4, 6)
  )
  out <- thermal_time(cold, date = date, tmin = tmin, tmax = tmax, t_base = 10)

  expect_true(all(out$gdd == 0))
  expect_true(all(out$thermal_time == 0))
})

test_that("accumulation restarts per group and respects an origin", {
  w <- weather_fixture()
  two <- rbind(cbind(w, site = "A"), cbind(w, site = "B"))

  out <- thermal_time(two, date = date, tmin = tmin, tmax = tmax,
                      t_base = 10, group_var = site)

  a <- out$thermal_time[out$site == "A"]
  b <- out$thermal_time[out$site == "B"]
  expect_equal(a, b)
  expect_equal(max(a), max(out$thermal_time))

  # Days before the origin must not accumulate.
  from_day5 <- thermal_time(w, date = date, tmin = tmin, tmax = tmax,
                            t_base = 10, origin = as.Date("2024-01-05"))
  expect_equal(from_day5$thermal_time[1:4], rep(0, 4))
  expect_gt(from_day5$thermal_time[5], 0)
  expect_lt(max(from_day5$thermal_time), max(out$thermal_time))
})

test_that("string and bare column names are both accepted", {
  w <- weather_fixture()
  bare <- thermal_time(w, date = date, tmin = tmin, tmax = tmax, t_base = 10)
  strs <- thermal_time(w, date = "date", tmin = "tmin", tmax = "tmax", t_base = 10)
  expect_equal(bare$thermal_time, strs$thermal_time)

  # Defaults already name the conventional columns.
  expect_equal(thermal_time(w, t_base = 10)$thermal_time, bare$thermal_time)
})

test_that("thermal_time refuses impossible requests", {
  w <- weather_fixture()

  expect_error(thermal_time(w, date = nope, t_base = 10), "was not found")
  expect_error(
    thermal_time(w[, c("date", "tmin")], t_base = 10),
    "Missing: tmax"
  )
  expect_error(
    thermal_time(w, t_base = 10, method = "cutoff"),
    "requires `t_upper`"
  )
  expect_error(
    thermal_time(w, t_base = 10, t_upper = 5, method = "cutoff"),
    "greater than `t_base`"
  )
  expect_error(
    thermal_time(w, tmean = "not_there", t_base = 10),
    "was not found"
  )

  # A pre-computed mean cannot be capped at the daily maximum.
  wm <- w
  wm$tmean <- (w$tmin + w$tmax) / 2
  expect_error(
    thermal_time(wm, tmean = tmean, t_base = 10, t_upper = 30, method = "cutoff"),
    "needs `tmin` and `tmax`"
  )
  expect_silent(thermal_time(wm, tmean = tmean, t_base = 10))
})

test_that("thermal time can drive a full growth analysis", {
  # The point of the function: swap calendar days for degree-days as `time`.
  w <- data.frame(
    date = as.Date("2024-01-01") + 0:99,
    tmin = 14, tmax = 26
  )
  tt <- thermal_time(w, date = date, tmin = tmin, tmax = tmax, t_base = 10)

  growth <- data.frame(
    date = as.Date("2024-01-01") + rep(c(14, 29, 44, 59, 74), each = 4),
    treatment = rep(c("A", "B"), 10),
    total_biomass_g = rep(c(2, 6, 12, 18, 22), each = 4) + stats::runif(20, 0, 0.5),
    leaf_area_cm2 = rep(c(20, 60, 120, 180, 220), each = 4)
  )
  growth <- merge(growth, tt[, c("date", "thermal_time")], by = "date")

  dat <- prepare_growth_data(growth, mapping = list(time = "thermal_time"))
  expect_true(all(dat$time > 0))
  expect_equal(length(unique(dat$time)), 5L)

  # 10 degree-days per calendar day here, so the span should scale accordingly.
  expect_equal(diff(range(dat$time)), 10 * (74 - 14))
})
