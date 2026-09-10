# phytogrowR 0.1.1

- Adds `growth_curve_params()`, which extracts the landmarks an experiment is
  usually designed to estimate from a fitted curve: asymptote, inflection point,
  maximum absolute and relative growth rates and when they occur, the times to
  10/50/90% of the asymptote, and the duration of the active phase. It reports
  whether the asymptote comes from a model parameter or is merely the highest
  fitted value, and flags `plateau_reached = FALSE` when the curve was still
  rising at the last harvest.
- Adds `compare_growth_models()`, which fits `exponential`, `logistic`,
  `gompertz`, `richards` and polynomial candidates to the same data and ranks
  them by AIC, BIC or RMSE, so the functional form tested by
  `compare_growth_curves()` is chosen from the data. Candidates that fail to
  converge are reported rather than dropped.
- Adds `thermal_time()`, converting a daily temperature series into growing
  degree days (`simple`, `cutoff` and `modified` methods, optional upper
  threshold, per-group accumulation and a sowing/emergence `origin`), so growth
  analysis can be run against thermal units instead of calendar days.
- Fixes `prepare_growth_data(mapping = )`, which renamed columns in the wrong
  direction. The mapping only ever worked on data that already used the
  standard names, that is, exactly when it was not needed; any real mapping
  such as `list(time = "dias")` failed with "Column `time` doesn't exist".
- Adds "Curve landmarks" and model-selection panels to the Shiny app, plus CSV
  export for both.
- Adds `compare_growth_curves()`, a formal test of coincidence of growth curves
  between treatments based on nested models. It reports the F test for nested
  models (Chow, 1960; Graybill, 1976) and the likelihood ratio test, decomposes
  the difference into `level` and `shape` for polynomial models and into
  parameter-wise hypotheses for the `exponential`, `logistic`, `gompertz` and
  `richards` families, and offers pairwise contrasts with multiplicity
  adjustment.
- Adds S3 methods for `phytogrow_curve_test`: `print()`, `summary()`, `tidy()`,
  `glance()`, and `plot()`; the plot overlays the treatment-specific curves and
  the single common curve implied by the null hypothesis.
- Adds a "Curve comparison" tab to the Shiny app, with model/response/grouping
  controls, a plain-language verdict, the nested-model and pairwise test tables,
  per-treatment parameter estimates, and CSV export.
- Moves `shiny`, `DT`, `readr`, `rmarkdown`, `knitr` and `broom` from `Imports`
  to `Suggests`. `run_phytogrow_app()` and `growth_report()` now verify these
  optional dependencies with `requireNamespace()` and fail early with an
  installation hint, and `tidy()` falls back to a coefficient-only tidier when
  `broom` is absent. Drops the unused `boot` and `purrr` dependencies.
- Adds `URL` and `BugReports` to `DESCRIPTION`, removes the invalid `Contact`
  field, and cites Radford (1967) and Poorter & Garnier (1996) alongside
  Hunt et al. (2002).
- Fixes the opaque "NaNs produced" warning raised when a fitted curve dipped
  below zero. A positive `epsilon` offsets zeros but cannot rescue negative
  values, so `.safe_log()` now reports how many values are affected, for which
  variable, and returns `NA` instead of `NaN`.
- `fit_growth_curve()` gains an `n_negative_fitted` column in `$messages` and
  says so in the message text, since a smoother fitted on the identity scale is
  not constrained to stay non-negative near the boundary of the time range.
  Log-based rates (RGR, ULR/NAR) are `NA` at those times.
- Adds a new classical two-harvest module based on Hunt et al. (2002):
  `classical_growth_interval()` with interval-average indices, checks, and CI.
- Adds classical helper functions:
  `calc_rgr_hunt()`, `calc_ulr()`, `calc_nar()`, `calc_lwf()`, `calc_lwr()`,
  `calc_lar_components()`, `calc_root_shoot_ratio()`,
  `calc_shoot_root_ratio()`, `calc_root_shoot_allometry()`,
  `classical_ci()`, `bootstrap_classical_growth()`, and `plot_allometry()`.
- Improves `calc_root_shoot_allometry(method = "bivariate_ml")` with a
  bivariate Model II approximation (RMA on log scale) and jackknife-based
  uncertainty, replacing simple fallback behavior.
- Adds S3 methods for `phytogrow_classical`: `print()`, `summary()`, `tidy()`,
  and `plot()`.
- Adds the new in-package dataset `hunt_classical_example` and corresponding
  data-raw script.
- Updates `calc_classic_growth()` to use `classical_growth_interval()`
  internally when the data represent a two-harvest interval with replication.
- Updates `growth_report()` with a dedicated "Classical Plant Growth Analysis"
  section.
- Updates the Shiny app with a new "Classical analysis" tab, interval controls,
  and CSV export for classical interval results.
- Adds Hunt et al. (2002) citation metadata to `DESCRIPTION`, `inst/CITATION`,
  docs, README, and a new vignette.

# phytogrowR 0.1.0

- Initial release candidate with core plant growth analysis workflow.
- Includes data quality checks, data preparation, classic and functional growth metrics.
- Adds bootstrap confidence intervals, treatment comparisons, biomass partitioning,
  publication-ready plots, and report generation.
- Includes a Shiny app for interactive analysis.
- Adds extended indices: `ALGR`, `RLGR`, `LAI`, `LAD`, `LWR`, and `SWR`.
- Adds classical references (Hunt, 1990; Benincasa, 2003) in package docs.
- Adds multiple in-app example datasets for each analysis module workflow.
