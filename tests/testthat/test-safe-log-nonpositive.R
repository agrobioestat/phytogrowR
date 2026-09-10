test_that("epsilon rescues zeros but never negatives, and says so", {
  # A positive offset makes log() defined at zero...
  expect_equal(.safe_log(c(0, 1), epsilon = 1), log(c(1, 2)))
  expect_silent(.safe_log(c(0, 1, 2), epsilon = 1e-6))

  # ...but it cannot rescue a value that is genuinely below -epsilon. That must
  # be reported once, with context, instead of a bare "NaNs produced" from R.
  expect_warning(
    out <- .safe_log(c(-0.5, 1, 2), epsilon = 1e-6, name = "fitted biomass"),
    class = "phytogrowR_nonpositive_log"
  )

  expect_true(is.na(out[1]))
  expect_false(is.nan(out[1]))
  expect_equal(out[2:3], log(c(1, 2) + 1e-6))

  # The message must name the variable and quantify the damage.
  msg <- tryCatch(
    .safe_log(c(-0.5, -2, 1), epsilon = 1e-6, name = "fitted leaf area"),
    warning = conditionMessage
  )
  expect_match(msg, "fitted leaf area")
  expect_match(msg, "^2 value")
})

test_that("safe_log without epsilon still aborts on non-positive input", {
  expect_error(.safe_log(c(0, 1), name = "W"), "cannot be log-transformed")
  expect_error(.safe_log(c(-1, 1), name = "W"), "W")
  expect_equal(.safe_log(c(1, exp(1))), c(0, 1))
})

test_that("NA input stays NA and is not reported as non-positive", {
  expect_silent(out <- .safe_log(c(NA_real_, 1, 2), epsilon = 1e-6))
  expect_true(is.na(out[1]))
})

test_that("fit_growth_curve reports fitted values that dip below zero", {
  # A penalised spline is not constrained to be non-negative and can overshoot
  # at the boundary of the time range, which makes log-based rates undefined.
  dat <- prepare_growth_data(growth_wide_example)
  fit <- fit_growth_curve(
    dat,
    response = "total_biomass_g",
    method = "gam",
    group_cols = c("treatment", "genotype")
  )

  expect_true("n_negative_fitted" %in% names(fit$messages))
  expect_type(fit$messages$n_negative_fitted, "integer")

  # The reported count must match the predictions it describes.
  counted <- fit$predictions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(fit$group_cols))) %>%
    dplyr::summarise(n_neg = sum(.data$fitted <= 0, na.rm = TRUE), .groups = "drop")

  merged <- dplyr::inner_join(
    fit$messages, counted,
    by = fit$group_cols
  )

  expect_equal(nrow(merged), nrow(fit$messages))
  expect_equal(merged$n_negative_fitted, as.integer(merged$n_neg))

  flagged <- fit$messages[!is.na(fit$messages$n_negative_fitted) &
                            fit$messages$n_negative_fitted > 0, ]
  if (nrow(flagged) > 0) {
    expect_match(flagged$message, "smoother overshoot", all = TRUE)
  }
})

test_that("instantaneous rates are NA, not NaN, where the curve is non-positive", {
  dat <- prepare_growth_data(growth_wide_example)
  gc <- c("treatment", "genotype")

  fw <- fit_growth_curve(dat, response = "total_biomass_g", method = "gam", group_cols = gc)
  fl <- fit_growth_curve(dat, response = "leaf_area_cm2", method = "gam", group_cols = gc)

  inst <- suppressWarnings(calc_instant_rates(fw, leaf_area_fit = fl, epsilon = 1e-6))

  num_cols <- names(inst)[vapply(inst, is.numeric, logical(1))]
  n_nan <- vapply(inst[num_cols], function(z) sum(is.nan(z)), integer(1))

  # NaN is a computation that went wrong; NA is an honest missing value.
  expect_equal(sum(n_nan), 0L)
  expect_gt(sum(is.finite(inst$rgr_inst)), 0.9 * nrow(inst))
})
