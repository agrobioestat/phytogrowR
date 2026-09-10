# phytogrowR

> **Phyto** + **Grow** + **R**  
> *Modern plant growth analysis for longitudinal and harvest data.*

`phytogrowR` is an R package focused on modern growth analysis in plant physiology and agronomic experiments. It supports repeated measurements, destructive sequential harvests, smooth growth modeling, derivative-based rates, bootstrap uncertainty, treatment comparison, biomass partitioning, automatic reports, and an interactive Shiny app.

Classical foundations follow Hunt (1990) and Benincasa (2003), extended here to tidy longitudinal workflows.

## Development Status

Release candidate (`0.1.1`) prepared with CRAN-oriented structure.

## Installation

```r
# install.packages("remotes")
# remotes::install_github("example/phytogrowR")
# or locally:
# remotes::install_local(".")
```

## Minimal Example

```r
library(phytogrowR)

dat <- prepare_growth_data(growth_wide_example)
classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"), epsilon = 1e-6)
head(classic)
```

## Full Example

```r
library(phytogrowR)
library(dplyr)

dat <- prepare_growth_data(growth_wide_example)
qc <- check_growth_data(dat)

fit_w <- fit_growth_curve(dat, response = "total_biomass_g", method = "gam", group_cols = c("treatment"))
fit_la <- fit_growth_curve(dat, response = "leaf_area_cm2", method = "gam", group_cols = c("treatment"))

instant <- calc_instant_rates(fit_w, leaf_area_fit = fit_la, epsilon = 1e-6)
classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"), epsilon = 1e-6)
part <- biomass_partition(dat, group_cols = c("treatment", "time"))
cmp <- compare_growth(dat, metric = "rgr", group_var = "treatment", epsilon = 1e-6)
boot_ci <- bootstrap_growth(dat, metric = "rgr", group_var = "treatment", times = 200, seed = 2026, epsilon = 1e-6)

# Additional indices available in the same workflow:
# ALGR, RLGR, LAI, LAD, LWR and SWR

plot_growth_curve(fit_w)
plot_growth_rates(instant, type = "instantaneous", metrics = c("agr_inst", "rgr_inst", "nar_inst"))
plot_partition(part, facet_var = "treatment")
```

## Classical growth analysis

```r
library(phytogrowR)

fit_classic <- classical_growth_interval(
  data = hunt_classical_example,
  time = time,
  total_biomass = total_biomass_g,
  leaf_biomass = leaf_biomass_g,
  root_biomass = root_biomass_g,
  stem_biomass = stem_biomass_g,
  leaf_area = leaf_area_cm2,
  group_by = treatment,
  ci_method = "both",
  n_boot = 999,
  seed = 123
)

summary(fit_classic)
tidy(fit_classic)
plot(fit_classic)
```

## Main Functions

- `check_growth_data()`
- `prepare_growth_data()`
- `calc_classic_growth()`
- `fit_growth_curve()`
- `calc_instant_rates()`
- `bootstrap_growth()`
- `compare_growth()`
- `compare_growth_curves()` (formal test of coincidence of curves between
  treatments: F test for nested models and likelihood ratio test)
- `biomass_partition()`
- `plot_growth_curve()`
- `plot_growth_rates()`
- `plot_partition()`
- `growth_report()`
- `run_phytogrow_app()`

## Classical References

- Hunt, R. (1990). *Basic Growth Analysis*. Unwin Hyman, London.
- Benincasa, M. M. P. (2003). *Plant Growth Analysis: Basic Concepts*. FUNEP, Jaboticabal.

## Key Differentials

- Built for longitudinal and destructive-harvest growth analysis.
- Supports multiple treatments, genotypes, blocks, plots, and plants.
- Flexible curve fitting (`spline`, `gam`, `loess`, `nls` variants).
- Instantaneous rates from fitted curve derivatives.
- Bootstrap CI by row, plant, or plot (with stratification).
- Tidy outputs and publication-ready graphics.
- Interactive Shiny app and automated report export.

## Suggested Citation

```text
Ribeiro, J. E. S. (2026). phytogrowR: Modern Growth Analysis for Plant Biomass and Leaf Area Data. R package version 0.1.1.
```

## License

MIT License.
