## Test environments

- local Windows 11 x64, R 4.5.3

## R CMD check results

- `R CMD build .`
- `R CMD check --as-cran phytogrowR_0.1.1.tar.gz`

Result: **0 ERROR | 0 WARNING | 2 NOTE**

## Notes

- New submission.
- `Skipping checking math rendering: package 'V8' unavailable` (check
  environment only).

## Dependencies

The analytical core depends only on `stats`, `utils`, `graphics`, `dplyr`,
`generics`, `ggplot2`, `mgcv`, `rlang`, `tibble` and `tidyr`.

Packages needed only by the optional interactive app, the report writer or the
`tidy()` fallback (`shiny`, `DT`, `readr`, `rmarkdown`, `knitr`, `broom`) are
listed in `Suggests` and guarded with `requireNamespace(..., quietly = TRUE)`
before use, so the package installs and runs without them.
