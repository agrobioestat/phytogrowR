test_that("the F statistic reproduces stats::anova() on the same nested models", {
  dat <- prepare_growth_data(growth_wide_example)

  keep <- stats::complete.cases(dat[, c("time", "total_biomass_g", "treatment")])
  d <- dat[keep, ]
  x <- d$time - mean(d$time)
  y <- log(d$total_biomass_g + 1e-6)
  g <- factor(d$treatment)

  m0 <- stats::lm(y ~ I(x^1) + I(x^2))
  m1 <- stats::lm(y ~ (I(x^1) + I(x^2)) * g)
  ref <- stats::anova(m0, m1)

  ct <- compare_growth_curves(
    dat,
    group_var = "treatment", degree = 2,
    log_response = TRUE, epsilon = 1e-6, test = "F"
  )
  got <- ct$tests[ct$tests$hypothesis == "coincidence", ]

  expect_equal(got$statistic, ref$F[2])
  expect_equal(got$p_value, ref[["Pr(>F)"]][2])
  expect_equal(got$df_num, 6)
  expect_equal(got$df_den, nrow(d) - 9)
})

test_that("coincident curves are not rejected and distinct curves are", {
  set.seed(20240501)
  tt <- rep(seq(5, 40, by = 5), each = 10)

  same <- data.frame(
    time = rep(tt, 2),
    treatment = rep(c("A", "B"), each = length(tt)),
    total_biomass_g = 10 * exp(0.06 * rep(tt, 2)) + stats::rnorm(2 * length(tt), 0, 1.5)
  )

  null_fit <- compare_growth_curves(same, group_var = "treatment", degree = 2)
  null_p <- null_fit$tests$p_value[null_fit$tests$hypothesis == "coincidence"]
  expect_true(all(null_p > 0.05))

  shifted <- same
  shifted$total_biomass_g[shifted$treatment == "B"] <-
    shifted$total_biomass_g[shifted$treatment == "B"] * 2

  alt_fit <- compare_growth_curves(shifted, group_var = "treatment", degree = 2)
  alt_p <- alt_fit$tests$p_value[alt_fit$tests$hypothesis == "coincidence"]
  expect_true(all(alt_p < 0.001))
})

test_that("the likelihood ratio statistic equals n * log(RSS0 / RSS1)", {
  dat <- prepare_growth_data(growth_wide_example)

  ct <- compare_growth_curves(dat, group_var = "treatment", degree = 2, test = "both")
  co <- ct$tests[ct$tests$hypothesis == "coincidence", ]
  lrt <- co[co$test == "LRT", ]

  expect_equal(
    lrt$statistic,
    ct$n_obs * log(lrt$rss_reduced / lrt$rss_full)
  )
  expect_equal(
    lrt$p_value,
    stats::pchisq(lrt$statistic, df = lrt$df_num, lower.tail = FALSE)
  )
})

test_that("centring time leaves the test statistics unchanged", {
  dat <- prepare_growth_data(growth_wide_example)

  a <- compare_growth_curves(dat, group_var = "treatment", degree = 2, center_time = TRUE)
  b <- compare_growth_curves(dat, group_var = "treatment", degree = 2, center_time = FALSE)

  expect_equal(a$tests$statistic, b$tests$statistic)
  expect_equal(a$tests$p_value, b$tests$p_value)
})

test_that("the polynomial decomposition is internally consistent", {
  dat <- prepare_growth_data(growth_wide_example)
  ct <- compare_growth_curves(dat, group_var = "treatment", degree = 2, test = "F")

  expect_setequal(unique(ct$tests$hypothesis), c("coincidence", "level", "shape"))

  co <- ct$tests[ct$tests$hypothesis == "coincidence", ]
  lv <- ct$tests[ct$tests$hypothesis == "level", ]
  sh <- ct$tests[ct$tests$hypothesis == "shape", ]

  # Nesting: reduced -> parallel -> full, so RSS must decrease monotonically
  # and the two partial hypotheses must share the intermediate model's RSS.
  expect_equal(co$rss_reduced, lv$rss_reduced)
  expect_equal(lv$rss_full, sh$rss_reduced)
  expect_equal(sh$rss_full, co$rss_full)
  expect_true(co$rss_reduced >= sh$rss_reduced)
  expect_true(sh$rss_reduced >= co$rss_full)

  # Degrees of freedom of the partial hypotheses add up to the global one.
  expect_equal(lv$df_num + sh$df_num, co$df_num)
})

test_that("pairwise tests are returned with adjusted p-values per test family", {
  dat <- prepare_growth_data(growth_wide_example)
  ct <- compare_growth_curves(
    dat,
    group_var = "treatment", degree = 2,
    pairwise = TRUE, p_adjust_method = "bonferroni", test = "F"
  )

  expect_equal(nrow(ct$pairwise), 3L)
  expect_true(all(c("group1", "group2", "p_value", "p_adj") %in% names(ct$pairwise)))
  expect_true(all(ct$pairwise$p_adj >= ct$pairwise$p_value))
  expect_equal(
    ct$pairwise$p_adj,
    stats::p.adjust(ct$pairwise$p_value, method = "bonferroni")
  )
})

test_that("non-linear families run and isolate the differing parameters", {
  dat <- prepare_growth_data(growth_wide_example)

  ct <- compare_growth_curves(
    dat,
    group_var = "treatment", model = "logistic", test = "F"
  )

  expect_s3_class(ct, "phytogrow_curve_test")
  expect_true("coincidence" %in% ct$tests$hypothesis)
  expect_setequal(
    ct$tests$term[ct$tests$hypothesis == "parameter"],
    c("Asym", "xmid", "scal")
  )
  expect_equal(sort(unique(ct$parameters$group)), sort(ct$groups))
})

test_that("invalid input is rejected with informative errors", {
  dat <- prepare_growth_data(growth_wide_example)

  expect_error(compare_growth_curves(dat, group_var = "nope"), "was not found")
  expect_error(compare_growth_curves(as.list(dat)), "data frame")
  expect_error(
    compare_growth_curves(dat[dat$treatment == "Control", ], group_var = "treatment"),
    "at least two levels"
  )
  expect_error(
    compare_growth_curves(dat, group_var = "treatment", degree = 9),
    "not supported"
  )

  tiny <- data.frame(
    time = c(1, 2, 3, 1, 2, 3),
    treatment = rep(c("A", "B"), each = 3),
    total_biomass_g = c(1, 2, 3, 2, 4, 6)
  )
  expect_error(
    compare_growth_curves(tiny, group_var = "treatment", degree = 2),
    "too few observations"
  )
})

test_that("a non-identifiable polynomial is refused instead of silently aliased", {
  # Two harvest dates support a straight line and nothing more, no matter how
  # many plants were measured. Fitting a quadratic makes lm() alias two
  # coefficients to NA; counting them would inflate df_num and deflate df_den.
  hunt <- prepare_growth_data(hunt_classical_example)
  expect_equal(length(unique(hunt$time)), 2L)

  expect_error(
    compare_growth_curves(hunt, group_var = "treatment", degree = 2),
    "not identifiable"
  )
  expect_error(
    compare_growth_curves(hunt, group_var = "treatment", model = "logistic"),
    "distinct time point"
  )

  ct <- compare_growth_curves(hunt, group_var = "treatment", degree = 1, test = "F")
  co <- ct$tests[ct$tests$hypothesis == "coincidence", ]

  # Straight line, two groups: 4 estimated parameters against 2 under H0.
  expect_equal(co$df_num, 2)
  expect_equal(co$df_den, nrow(hunt) - 4)
  expect_equal(ct$n_par[["full"]], 4L)

  ref <- stats::anova(
    stats::lm(log(total_biomass_g) ~ time, data = hunt),
    stats::lm(log(total_biomass_g) ~ time * treatment, data = hunt)
  )
  ct_log <- compare_growth_curves(
    hunt, group_var = "treatment", degree = 1, log_response = TRUE, test = "F"
  )
  expect_equal(
    ct_log$tests$statistic[ct_log$tests$hypothesis == "coincidence"],
    ref$F[2]
  )
})

test_that("model rank, not coefficient length, drives the degrees of freedom", {
  # A rank-deficient lm must report the parameters it actually estimated.
  d <- data.frame(x = rep(c(1, 2), each = 6), y = rnorm(12), g = rep(c("a", "b"), 6))
  m <- stats::lm(y ~ I(x^1) + I(x^2), data = d)

  expect_true(any(is.na(stats::coef(m))))
  expect_equal(phytogrowR:::.model_rank(m), 2L)
  expect_lt(phytogrowR:::.model_rank(m), length(stats::coef(m)))
})

test_that("S3 methods behave", {
  dat <- prepare_growth_data(growth_wide_example)
  ct <- compare_growth_curves(dat, group_var = "treatment", degree = 2, pairwise = TRUE)

  expect_output(print(ct), "Coincidence of growth curves")
  expect_output(print(summary(ct)), "Interpretation guide")

  td <- tidy(ct)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("hypothesis", "test", "statistic", "p_value", "model") %in% names(td)))

  gl <- glance(ct)
  expect_equal(nrow(gl), 1L)
  expect_true(all(c("statistic_F", "p_value_F", "statistic_LRT", "p_value_LRT") %in% names(gl)))

  p <- plot(ct)
  expect_s3_class(p, "ggplot")

  # The prediction grid must carry both the per-treatment and the common curve.
  pred <- phytogrowR:::.curve_test_predict(ct, n_grid = 10)
  expect_setequal(unique(pred$hypothesis), c("full", "reduced"))
  expect_true(all(is.finite(pred$fitted)))
})

test_that("optional-dependency guards fail loudly and only when needed", {
  expect_error(
    phytogrowR:::.check_suggested("definitelyNotAnInstalledPackage"),
    class = "phytogrowR_missing_suggests"
  )
  expect_true(phytogrowR:::.check_suggested("stats"))
  expect_true(phytogrowR:::.has_pkg("stats"))
  expect_false(phytogrowR:::.has_pkg("definitelyNotAnInstalledPackage"))

  # The coefficient-only tidy fallback keeps the broom column contract.
  m <- stats::lm(mpg ~ wt, data = datasets::mtcars)
  fb <- phytogrowR:::.coef_tidy(m)
  expect_equal(
    names(fb),
    c("term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(fb$estimate, unname(stats::coef(m)))
})
