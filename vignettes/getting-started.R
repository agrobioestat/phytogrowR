## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(phytogrowR)
library(dplyr)


## -----------------------------------------------------------------------------
head(growth_wide_example)


## -----------------------------------------------------------------------------
dat <- prepare_growth_data(growth_wide_example)
qc <- check_growth_data(dat)
qc


## -----------------------------------------------------------------------------
classic <- calc_classic_growth(
  dat,
  group_cols = c("treatment", "plot_id", "plant_id"),
  epsilon = 1e-6
)
classic %>%
  group_by(treatment) %>%
  summarise(across(c(rgr, agr, nar, algr, rlgr, lar, lai, sla, cgr), mean, na.rm = TRUE), .groups = "drop")


## -----------------------------------------------------------------------------
fit <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = "treatment", method = "gam")
plot_growth_curve(fit)
plot_growth_rates(classic, type = "classic", metrics = c("rgr", "agr", "rlgr", "algr"))

