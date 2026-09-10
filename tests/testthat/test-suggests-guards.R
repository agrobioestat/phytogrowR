# The point of moving shiny, bslib, DT, readr, rmarkdown, knitr and broom to
# Suggests is that the package must still install and work without them. Simply
# running the checks on a machine where they happen to be installed proves
# nothing, so the guard is exercised directly by forcing `.has_pkg()` to report
# the packages as absent.

test_that("run_phytogrow_app() aborts with an installable message when the app deps are missing", {
  testthat::local_mocked_bindings(.has_pkg = function(pkg) FALSE)

  expect_error(run_phytogrow_app(), class = "phytogrowR_missing_suggests")

  msg <- tryCatch(run_phytogrow_app(), error = conditionMessage)
  expect_match(msg, "shiny")
  expect_match(msg, "bslib")
  expect_match(msg, "DT")
  expect_match(msg, "readr")
  expect_match(msg, "install.packages")
  # It must fail before touching the app directory, not half-way through.
  expect_match(msg, "interactive phytogrowR application")
})

test_that("growth_report() aborts before writing anything when rmarkdown is missing", {
  testthat::local_mocked_bindings(.has_pkg = function(pkg) FALSE)

  out <- file.path(tempdir(), "should_never_be_created.html")
  expect_error(
    growth_report(growth_wide_example, output_file = out),
    class = "phytogrowR_missing_suggests"
  )
  expect_false(file.exists(out))

  msg <- tryCatch(
    growth_report(growth_wide_example, output_file = out),
    error = conditionMessage
  )
  expect_match(msg, "rmarkdown")
  expect_match(msg, "knitr")
})

test_that("only the genuinely missing packages are named", {
  # 'stats' is always there; the report is not allowed to cry wolf about it.
  testthat::local_mocked_bindings(.has_pkg = function(pkg) pkg == "knitr")

  msg <- tryCatch(
    .check_suggested("knitr", "rmarkdown", use = "the report"),
    error = conditionMessage
  )
  expect_match(msg, "rmarkdown")
  expect_false(grepl("'knitr'", msg, fixed = TRUE))
})

test_that("tidy() degrades to the coefficient-only tidier when broom is absent", {
  dat <- prepare_growth_data(growth_wide_example)
  fit <- fit_growth_curve(
    dat,
    response = "total_biomass_g",
    method = "logistic",
    group_cols = "treatment"
  )

  with_broom <- tidy(fit)

  testthat::local_mocked_bindings(.has_pkg = function(pkg) FALSE)
  without_broom <- tidy(fit)

  # The contract that downstream code relies on must not change.
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in%
                    names(without_broom)))
  expect_equal(nrow(without_broom), nrow(with_broom))
  expect_equal(without_broom$term, with_broom$term)
  expect_equal(without_broom$estimate, with_broom$estimate)

  # Only the extra columns broom would have filled in are lost.
  expect_true(all(is.na(without_broom$std.error)))
})
