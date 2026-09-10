make_classical_test_data <- function() {
  tibble::tibble(
    plant_id = paste0("P", seq_len(8)),
    harvest = rep(c(1L, 2L), each = 4),
    time = rep(c(10, 20), each = 4),
    treatment = "A",
    genotype = "G1",
    block = "B1",
    plot_id = "PL1",
    root_biomass_g = c(0.7, 0.8, 0.65, 0.75, 1.1, 1.15, 1.05, 1.12),
    leaf_biomass_g = c(1.0, 1.1, 0.9, 1.0, 1.5, 1.6, 1.45, 1.55),
    stem_biomass_g = c(0.6, 0.7, 0.55, 0.62, 0.95, 1.0, 0.92, 0.98),
    reproductive_biomass_g = c(0.1, 0.12, 0.09, 0.1, 0.25, 0.27, 0.23, 0.24),
    shoot_biomass_g = c(1.7, 1.92, 1.54, 1.72, 2.7, 2.87, 2.6, 2.77),
    total_biomass_g = c(2.4, 2.72, 2.19, 2.47, 3.8, 4.02, 3.65, 3.89),
    leaf_area_cm2 = c(34, 36, 33, 35, 51, 54, 50, 53)
  )
}

get_metric_est <- function(fit, idx, ci_source = "classical") {
  fit$results %>%
    dplyr::filter(.data$index == idx, .data$ci_source == ci_source) %>%
    dplyr::pull(.data$estimate) %>%
    .[1]
}

test_that("RGR uses mean(log(W)) and not log(mean(W))", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    data = dat,
    time = time,
    total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g,
    stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g,
    leaf_area = leaf_area_cm2,
    group_by = treatment,
    ci_method = "classical",
    warn = FALSE
  )

  w1 <- dat$total_biomass_g[dat$time == 10]
  w2 <- dat$total_biomass_g[dat$time == 20]
  expected <- (mean(log(w2)) - mean(log(w1))) / 10
  wrong <- (log(mean(w2)) - log(mean(w1))) / 10

  obs <- get_metric_est(fit, "rgr")
  expect_equal(obs, expected, tolerance = 1e-10)
  expect_true(abs(obs - wrong) > 1e-6)
})

test_that("AGR is calculated correctly", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )

  w1 <- dat$total_biomass_g[dat$time == 10]
  w2 <- dat$total_biomass_g[dat$time == 20]
  expected <- (mean(w2) - mean(w1)) / 10
  expect_equal(get_metric_est(fit, "agr"), expected, tolerance = 1e-10)
})

test_that("ULR and NAR are calculated correctly and equivalent", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", nar_label = "both", warn = FALSE
  )

  w1 <- dat$total_biomass_g[dat$time == 10]
  w2 <- dat$total_biomass_g[dat$time == 20]
  la1 <- dat$leaf_area_cm2[dat$time == 10]
  la2 <- dat$leaf_area_cm2[dat$time == 20]

  expected <- ((mean(w2) - mean(w1)) / 10) * ((log(mean(la2)) - log(mean(la1))) / (mean(la2) - mean(la1)))
  ulr <- get_metric_est(fit, "ulr")
  nar <- get_metric_est(fit, "nar")

  expect_equal(ulr, expected, tolerance = 1e-10)
  expect_equal(nar, expected, tolerance = 1e-10)
})

test_that("SLA is calculated correctly", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )

  sla1 <- dat$leaf_area_cm2[dat$time == 10] / dat$leaf_biomass_g[dat$time == 10]
  sla2 <- dat$leaf_area_cm2[dat$time == 20] / dat$leaf_biomass_g[dat$time == 20]
  expected <- (mean(sla1) + mean(sla2)) / 2
  expect_equal(get_metric_est(fit, "sla"), expected, tolerance = 1e-10)
})

test_that("LWF and LWR return identical results", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )
  expect_equal(get_metric_est(fit, "lwf"), get_metric_est(fit, "lwr"), tolerance = 1e-12)
})

test_that("LAR is calculated correctly", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )

  lar1 <- dat$leaf_area_cm2[dat$time == 10] / dat$total_biomass_g[dat$time == 10]
  lar2 <- dat$leaf_area_cm2[dat$time == 20] / dat$total_biomass_g[dat$time == 20]
  expected <- (mean(lar1) + mean(lar2)) / 2
  expect_equal(get_metric_est(fit, "lar"), expected, tolerance = 1e-10)
})

test_that("LAR is approximately SLA * LWF on same aggregation scale", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )

  lar <- get_metric_est(fit, "lar")
  sla <- get_metric_est(fit, "sla")
  lwf <- get_metric_est(fit, "lwf")
  expect_equal(lar, sla * lwf, tolerance = 0.3)
})

test_that("root_shoot_ratio is calculated correctly", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, reproductive_biomass = reproductive_biomass_g,
    leaf_area = leaf_area_cm2, group_by = treatment,
    ci_method = "classical", warn = FALSE
  )

  s1 <- dat$leaf_biomass_g[dat$time == 10] + dat$stem_biomass_g[dat$time == 10] + dat$reproductive_biomass_g[dat$time == 10]
  s2 <- dat$leaf_biomass_g[dat$time == 20] + dat$stem_biomass_g[dat$time == 20] + dat$reproductive_biomass_g[dat$time == 20]
  rs1 <- dat$root_biomass_g[dat$time == 10] / s1
  rs2 <- dat$root_biomass_g[dat$time == 20] / s2
  expected <- (mean(rs1) + mean(rs2)) / 2
  expect_equal(get_metric_est(fit, "root_shoot_ratio"), expected, tolerance = 1e-10)
})

test_that("shoot_root_ratio is calculated correctly", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, reproductive_biomass = reproductive_biomass_g,
    leaf_area = leaf_area_cm2, group_by = treatment,
    ci_method = "classical", warn = FALSE
  )

  s1 <- dat$leaf_biomass_g[dat$time == 10] + dat$stem_biomass_g[dat$time == 10] + dat$reproductive_biomass_g[dat$time == 10]
  s2 <- dat$leaf_biomass_g[dat$time == 20] + dat$stem_biomass_g[dat$time == 20] + dat$reproductive_biomass_g[dat$time == 20]
  sr1 <- s1 / dat$root_biomass_g[dat$time == 10]
  sr2 <- s2 / dat$root_biomass_g[dat$time == 20]
  expected <- (mean(sr1) + mean(sr2)) / 2
  expect_equal(get_metric_est(fit, "shoot_root_ratio"), expected, tolerance = 1e-10)
})

test_that("root-shoot allometric coefficient works", {
  dat <- make_classical_test_data()
  allom <- calc_root_shoot_allometry(
    root_biomass = dat$root_biomass_g,
    shoot_biomass = dat$shoot_biomass_g,
    method = "lm"
  )
  expect_true(is.finite(allom$beta))
  expect_true(allom$n >= 5)
})

test_that("bivariate_ml allometry approximation works", {
  dat <- make_classical_test_data()
  allom <- calc_root_shoot_allometry(
    root_biomass = dat$root_biomass_g,
    shoot_biomass = dat$shoot_biomass_g,
    method = "bivariate_ml"
  )
  expect_true(is.finite(allom$beta))
  expect_match(allom$method, "bivariate_ml")
})

test_that("error when biomass used in log is zero", {
  dat <- make_classical_test_data()
  dat$total_biomass_g[1] <- 0
  expect_error(
    classical_growth_interval(
      dat, time = time, total_biomass = total_biomass_g,
      leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
      root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
      group_by = treatment
    ),
    "Zero or negative total biomass"
  )
})

test_that("error when negative values are present", {
  dat <- make_classical_test_data()
  dat$leaf_area_cm2[1] <- -1
  expect_error(
    classical_growth_interval(
      dat, time = time, total_biomass = total_biomass_g,
      leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
      root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
      group_by = treatment
    ),
    "Negative values"
  )
})

test_that("warning when missing values are present", {
  dat <- make_classical_test_data()
  dat$leaf_area_cm2[1] <- NA_real_
  expect_warning(
    classical_growth_interval(
      dat, time = time, total_biomass = total_biomass_g,
      leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
      root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
      group_by = treatment, warn = TRUE
    ),
    "Missing values detected"
  )
})

test_that("error when fewer than two harvests are provided", {
  dat <- make_classical_test_data() %>% dplyr::filter(time == 10)
  expect_error(
    classical_growth_interval(
      dat, time = time, total_biomass = total_biomass_g,
      leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
      root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
      group_by = treatment
    ),
    "At least two harvest times"
  )
})

test_that("error when more than two harvests are provided without interval", {
  dat <- make_classical_test_data() %>%
    dplyr::bind_rows(
      make_classical_test_data() %>%
        dplyr::mutate(time = 30, harvest = 3L, total_biomass_g = total_biomass_g * 1.3, leaf_area_cm2 = leaf_area_cm2 * 1.2)
    )
  expect_error(
    classical_growth_interval(
      dat, time = time, total_biomass = total_biomass_g,
      leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
      root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
      group_by = treatment,
      warn = FALSE
    ),
    "More than two harvests"
  )
})

test_that("classical_growth_interval returns phytogrow_classical object", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )
  expect_s3_class(fit, "phytogrow_classical")
  expect_true(nrow(fit$results) > 0)
})

test_that("print, summary, tidy and plot methods work for phytogrow_classical", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )

  expect_error(print(fit), NA)
  sm <- summary(fit)
  td <- tidy(fit)
  plt <- plot(fit, metric = "rgr")

  expect_s3_class(sm, "summary.phytogrow_classical")
  expect_true(is.data.frame(td))
  expect_s3_class(plt, "ggplot")
})

test_that("ci_method classical works", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "classical", warn = FALSE
  )
  expect_true(all(fit$results$ci_source == "classical"))
})

test_that("ci_method bootstrap works", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "bootstrap", n_boot = 49, seed = 1, warn = FALSE
  )
  expect_true(all(fit$results$ci_source == "bootstrap"))
})

test_that("ci_method both works", {
  dat <- make_classical_test_data()
  fit <- classical_growth_interval(
    dat, time = time, total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g, stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g, leaf_area = leaf_area_cm2,
    group_by = treatment, ci_method = "both", n_boot = 49, seed = 1, warn = FALSE
  )
  expect_true(all(c("classical", "bootstrap") %in% fit$results$ci_source))
})
