#' phytogrowR: Modern Plant Growth Analysis
#'
#' `phytogrowR` supports classical and functional growth analysis for plant
#' physiology and agronomic experiments based on repeated measurements,
#' destructive sequential harvests, biomass trajectories, and leaf area series.
#' Core concepts are based on classical growth analysis references including
#' Hunt (1990) and Benincasa (2003).
#'
#' @references
#' Hunt, R. (1990). *Basic Growth Analysis*. Unwin Hyman, London.
#'
#' Benincasa, M. M. P. (2003). *Plant Growth Analysis: Basic Concepts*.
#' FUNEP, Jaboticabal.
#'
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @keywords internal
#' @importFrom stats approx as.formula coef complete.cases lm loess median nls
#'   p.adjust pchisq pf predict qnorm quantile resid sd setNames smooth.spline
#'   t.test var
#' @importFrom utils combn head tail
#' @importFrom graphics plot
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom generics augment glance tidy
"_PACKAGE"

utils::globalVariables(
  c(
    ".", ".response", ".time", "..group_key", "LA", "W", "agr", "agr_inst", "algr", "algr_inst", "alpha", "block",
    "cgr", "cgr_inst", "ci_source", "component", "conf.high", "conf.low", "conf_high", "conf_low", "date", "dLAdt",
    "days_after_sowing", "degrees_freedom", "delta_t", "details", "difference", "estimate", "estimate1", "estimate2",
    "fitted", "genotype", "group", "group1", "group2", "harvest", "issue",
    "index", "lai", "lai_inst", "lad", "leaf_area_cm2", "leaf_biomass_g", "leaf_mass",
    "leaf_stem_ratio", "lmr", "lmr_inst", "logLA", "lwr", "metric", "nar",
    "nar_inst", "n_obs", "p.value", "plant_seq",
    "p_adj", "plant_id", "plot_id", "prop_leaf", "prop_reproductive", "prop_root",
    "prop_stem", "ratio", "rgr", "rgr_inst", "rlgr", "rlgr_inst", "rmr",
    "root", "root_biomass_g", "root_mass", "root_shoot_ratio", "row_id", "severity", "shoot", "shoot_biomass_g",
    "sla", "sla_cm2_g", "sla_inst", "smr", "standard_error", "status", "stem_biomass_g", "stem_mass", "swr",
    "suggestion", "time",
    "time_end", "time_start", "total_biomass_g", "treatment", "value", "variable", "reproductive_biomass_g",
    "w", "x", "xmid", "y",
    ".group_", ".resp_", ".time_", "comparison", "df_full", "df_reduced",
    "df_num", "df_den", "hypothesis", "logLik_full", "logLik_reduced",
    "n_par_full", "n_par_reduced", "p_value", "rss_full", "rss_reduced",
    "statistic", "test", "term"
  )
)

