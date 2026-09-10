# phytogrowR 0.1.1

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
