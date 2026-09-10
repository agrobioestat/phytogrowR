#' Compare Growth Metrics Between Treatments or Genotypes
#'
#' Performs pairwise comparisons for selected growth metrics with optional
#' multiplicity adjustment.
#'
#' @param data Standardized growth data.
#' @param metric Metric to compare.
#' @param group_var Grouping variable (for example `"treatment"` or `"genotype"`).
#' @param reference Optional reference group. If `NULL`, all pairwise contrasts
#' are computed.
#' @param p_adjust_method Method passed to [stats::p.adjust()].
#' @param conf_level Confidence level for interval estimates.
#' @param epsilon Optional log offset for classical metrics.
#'
#' @return A tibble with pairwise differences, ratios, confidence intervals,
#' raw p-values, and adjusted p-values.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' cmp <- compare_growth(dat, metric = "rgr", group_var = "treatment")
#' cmp
#' @export
compare_growth <- function(
    data,
    metric = c("rgr", "nar", "lar", "sla", "cgr", "lai", "rlgr", "algr", "final_biomass"),
    group_var = "treatment",
    reference = NULL,
    p_adjust_method = "BH",
    conf_level = 0.95,
    epsilon = NULL
) {
  metric <- match.arg(metric)
  .assert_conf_level(conf_level)

  dat <- prepare_growth_data(data, quiet = TRUE)

  if (!group_var %in% names(dat)) {
    rlang::abort(paste0("Grouping column `", group_var, "` was not found in data."))
  }

  unit_col <- if ("plot_id" %in% names(dat)) "plot_id" else if ("plant_id" %in% names(dat)) "plant_id" else NULL

  values <- if (metric == "final_biomass") {
    by_cols <- intersect(c(group_var, unit_col), names(dat))
    dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) %>%
      dplyr::slice_max(order_by = .data$time, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        group = .data[[group_var]],
        unit = if (!is.null(unit_col)) .data[[unit_col]] else dplyr::row_number(),
        value = .data$total_biomass_g
      )
  } else {
    classic <- calc_classic_growth(
      dat,
      group_cols = intersect(c(group_var, unit_col, "plant_id"), names(dat)),
      epsilon = epsilon
    )

    if (nrow(classic) == 0 || !metric %in% names(classic)) {
      return(tibble::tibble())
    }

    if (is.null(unit_col) || !unit_col %in% names(classic)) {
      classic <- classic %>% dplyr::mutate(.unit_tmp = dplyr::row_number())
      unit_name <- ".unit_tmp"
    } else {
      unit_name <- unit_col
    }

    classic %>%
      dplyr::group_by(.data[[group_var]], .data[[unit_name]]) %>%
      dplyr::summarise(value = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(group = dplyr::all_of(group_var), unit = dplyr::all_of(unit_name))
  }

  groups <- sort(unique(values$group))
  if (length(groups) < 2) {
    return(tibble::tibble())
  }
  if (!is.null(reference)) {
    if (!reference %in% groups) {
      rlang::abort("`reference` was not found among group levels.")
    }
    pairs <- lapply(setdiff(groups, reference), function(g) c(reference, g))
  } else {
    pairs <- utils::combn(groups, 2, simplify = FALSE)
  }

  if (length(pairs) == 0) {
    return(tibble::tibble())
  }

  alpha <- 1 - conf_level

  comp <- lapply(pairs, function(p) {
    g1 <- p[1]
    g2 <- p[2]

    x <- values$value[values$group == g1]
    y <- values$value[values$group == g2]

    est1 <- mean(x, na.rm = TRUE)
    est2 <- mean(y, na.rm = TRUE)

    t_res <- tryCatch(
      stats::t.test(x, y, conf.level = conf_level),
      error = function(e) NULL
    )

    tibble::tibble(
      group1 = g1,
      group2 = g2,
      estimate1 = est1,
      estimate2 = est2,
      difference = est1 - est2,
      ratio = .safe_div(est1, est2),
      conf.low = if (is.null(t_res)) NA_real_ else unname(t_res$conf.int[1]),
      conf.high = if (is.null(t_res)) NA_real_ else unname(t_res$conf.int[2]),
      p.value = if (is.null(t_res)) NA_real_ else unname(t_res$p.value)
    )
  })

  out <- dplyr::bind_rows(comp) %>%
    dplyr::mutate(
      p_adj = stats::p.adjust(.data$p.value, method = p_adjust_method),
      metric = metric,
      group_var = group_var,
      conf_level = conf_level,
      alpha = alpha
    )

  out <- out %>%
    dplyr::select(dplyr::all_of(c(
      "group_var", "metric", "group1", "group2",
      "estimate1", "estimate2", "difference", "ratio",
      "conf.low", "conf.high", "p.value", "p_adj"
    )))

  out
}
