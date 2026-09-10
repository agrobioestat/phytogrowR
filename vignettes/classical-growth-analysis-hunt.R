## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(phytogrowR)
library(dplyr)


## -----------------------------------------------------------------------------
data(hunt_classical_example)
dplyr::glimpse(hunt_classical_example)


## -----------------------------------------------------------------------------
fit_classic <- classical_growth_interval(
  data = hunt_classical_example,
  time = time,
  total_biomass = total_biomass_g,
  leaf_biomass = leaf_biomass_g,
  root_biomass = root_biomass_g,
  stem_biomass = stem_biomass_g,
  reproductive_biomass = reproductive_biomass_g,
  leaf_area = leaf_area_cm2,
  group_by = treatment,
  ci_method = "both",
  n_boot = 199,
  seed = 123
)

print(fit_classic)


## -----------------------------------------------------------------------------
summary(fit_classic)


## -----------------------------------------------------------------------------
head(tidy(fit_classic))


## -----------------------------------------------------------------------------
fit_treatment <- tidy(fit_classic) %>%
  filter(index %in% c("rgr", "ulr", "sla", "lar")) %>%
  arrange(index, treatment)

fit_treatment


## -----------------------------------------------------------------------------
plot(fit_classic, metric = "rgr")


## -----------------------------------------------------------------------------
plot(fit_classic, metric = "ulr")


## -----------------------------------------------------------------------------
plot_allometry(
  data = hunt_classical_example,
  shoot_biomass = "shoot_biomass_g",
  root_biomass = "root_biomass_g",
  treatment = "treatment"
)

