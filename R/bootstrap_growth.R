.compute_metric <- function(data, metric, group_var = "treatment", epsilon = NULL) {
  if (!group_var %in% names(data)) {
    data[[group_var]] <- "all"
  }

  if (metric == "final_biomass") {
    by_cols <- intersect(c(group_var, "plot_id", "plant_id"), names(data))

    final_dat <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) %>%
      dplyr::slice_max(order_by = .data$time, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    return(
      final_dat %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(estimate = mean(.data$total_biomass_g, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(group = dplyr::all_of(group_var))
    )
  }

  classic <- calc_classic_growth(
    data,
    group_cols = intersect(c(group_var, "plot_id", "plant_id"), names(data)),
    epsilon = epsilon
  )

  if (nrow(classic) == 0 || !metric %in% names(classic)) {
    return(
      tibble::tibble(
        group = unique(data[[group_var]]),
        estimate = NA_real_
      )
    )
  }

  classic %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(estimate = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(group = dplyr::all_of(group_var))
}

.bootstrap_resample <- function(data, resample_unit = "row", unit_col = NULL, stratify_by = NULL) {
  dat <- tibble::as_tibble(data)

  strata_cols <- intersect(stratify_by %||% character(), names(dat))
  dat$.stratum <- if (length(strata_cols) > 0) .make_group_key(dat, strata_cols) else "all"

  split_dat <- split(dat, dat$.stratum)

  out <- lapply(split_dat, function(df) {
    if (resample_unit == "row") {
      return(df[sample.int(nrow(df), size = nrow(df), replace = TRUE), , drop = FALSE])
    }

    ucol <- unit_col
    if (is.null(ucol) || !ucol %in% names(df)) {
      rlang::abort("`unit_col` must exist in data for unit-based bootstrap.")
    }

    units <- unique(df[[ucol]])
    sampled <- sample(units, size = length(units), replace = TRUE)

    dplyr::bind_rows(lapply(sampled, function(u) {
      df[df[[ucol]] == u, , drop = FALSE]
    }))
  })

  dplyr::bind_rows(out) %>% dplyr::select(-dplyr::all_of(".stratum"))
}

#' Bootstrap Confidence Intervals for Growth Metrics
#'
#' Estimate uncertainty for growth metrics using bootstrap resampling by row,
#' plant, or plot, optionally stratified by treatment.
#'
#' @param data A standardized growth dataset.
#' @param metric Metric to summarize. One of `"rgr"`, `"agr"`, `"nar"`,
#' `"lar"`, `"sla"`, `"cgr"`, `"lai"`, `"algr"`, `"rlgr"`, or `"final_biomass"`.
#' @param group_var Grouping variable for reported estimates.
#' @param resample_unit Bootstrap unit: `"row"`, `"plant"`, or `"plot"`.
#' @param stratify_by Optional character vector of stratification columns.
#' @param times Number of bootstrap resamples.
#' @param conf_level Confidence level.
#' @param seed Reproducible random seed.
#' @param epsilon Optional offset for log-based rates.
#'
#' @return A tibble with `group`, `estimate`, `std.error`, `conf.low`,
#' `conf.high`, `metric`, and `method`.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' boot_ci <- bootstrap_growth(dat, metric = "rgr", times = 3, seed = 100)
#' boot_ci
#' @export
bootstrap_growth <- function(
    data,
    metric = c("rgr", "agr", "nar", "lar", "sla", "cgr", "lai", "algr", "rlgr", "final_biomass"),
    group_var = "treatment",
    resample_unit = c("row", "plant", "plot"),
    stratify_by = "treatment",
    times = 500,
    conf_level = 0.95,
    seed = 123,
    epsilon = NULL
) {
  metric <- match.arg(metric)
  resample_unit <- match.arg(resample_unit)
  .assert_positive_integer(times, arg = "times")
  .assert_conf_level(conf_level)

  dat <- prepare_growth_data(data, quiet = TRUE)

  if (!group_var %in% names(dat)) {
    dat[[group_var]] <- "all"
  }

  unit_col <- switch(
    resample_unit,
    row = NULL,
    plant = if ("plant_id" %in% names(dat)) "plant_id" else NULL,
    plot = if ("plot_id" %in% names(dat)) "plot_id" else NULL
  )

  if (resample_unit != "row" && is.null(unit_col)) {
    rlang::abort("Unit-based bootstrap requested but the corresponding id column is missing.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  point <- .compute_metric(dat, metric = metric, group_var = group_var, epsilon = epsilon)

  boot_vals <- vector("list", times)

  for (i in seq_len(times)) {
    bdat <- .bootstrap_resample(
      data = dat,
      resample_unit = resample_unit,
      unit_col = unit_col,
      stratify_by = stratify_by
    )

    bstat <- .compute_metric(bdat, metric = metric, group_var = group_var, epsilon = epsilon)
    bstat$replicate <- i
    boot_vals[[i]] <- bstat
  }

  boot_tbl <- dplyr::bind_rows(boot_vals)

  out <- point %>%
    dplyr::select(dplyr::all_of(c("group", "estimate"))) %>%
    dplyr::left_join(
      boot_tbl %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(
          std.error = stats::sd(.data$estimate, na.rm = TRUE),
          conf.low = stats::quantile(
            .data$estimate,
            probs = (1 - conf_level) / 2,
            na.rm = TRUE,
            names = FALSE
          ),
          conf.high = stats::quantile(
            .data$estimate,
            probs = 1 - (1 - conf_level) / 2,
            na.rm = TRUE,
            names = FALSE
          ),
          .groups = "drop"
        ),
      by = "group"
    ) %>%
    dplyr::mutate(
      metric = metric,
      method = paste0("bootstrap_", resample_unit)
    )

  out <- out %>%
    dplyr::select(dplyr::all_of(c(
      "group", "metric", "estimate", "std.error", "conf.low", "conf.high", "method"
    )))

  out
}
