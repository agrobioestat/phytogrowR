## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(phytogrowR)
library(dplyr)


## -----------------------------------------------------------------------------
dat <- prepare_growth_data(growth_wide_example)
fit_w <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = c("treatment"), method = "gam")
fit_la <- fit_growth_curve(dat, response = "leaf_area_cm2", group_cols = c("treatment"), method = "gam")

summary(fit_w)
plot_growth_curve(fit_w)


## -----------------------------------------------------------------------------
inst <- calc_instant_rates(fit_w, leaf_area_fit = fit_la, epsilon = 1e-6)
head(inst)
plot_growth_rates(inst, type = "instantaneous", metrics = c("agr_inst", "rgr_inst", "algr_inst", "rlgr_inst", "nar_inst"))


## -----------------------------------------------------------------------------
cmp_rgr <- compare_growth(dat, metric = "rgr", group_var = "treatment", epsilon = 1e-6)
cmp_rgr

boot_rgr <- bootstrap_growth(dat, metric = "rgr", group_var = "treatment", times = 100, seed = 2026, epsilon = 1e-6)
boot_rgr

