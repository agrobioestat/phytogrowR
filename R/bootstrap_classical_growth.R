.classical_boot_resample <- function(data, resample_unit = "row", unit_col = NULL, stratify_by = NULL) {
  dat <- tibble::as_tibble(data)

  strata <- intersect(stratify_by %||% character(), names(dat))
  dat$.stratum <- if (length(strata) > 0L) .make_group_key(dat, strata) else "all"

  by_stratum <- split(dat, dat$.stratum)

  out <- lapply(by_stratum, function(df) {
    if (resample_unit == "row") {
      idx <- sample.int(nrow(df), size = nrow(df), replace = TRUE)
      return(df[idx, , drop = FALSE])
    }

    if (is.null(unit_col) || !unit_col %in% names(df)) {
      rlang::abort("Unit-based bootstrap requested but the id column is missing.")
    }

    units <- unique(df[[unit_col]])
    units <- units[!is.na(units)]
    if (length(units) == 0L) {
      return(df[0, , drop = FALSE])
    }

    sampled <- sample(units, size = length(units), replace = TRUE)
    dplyr::bind_rows(lapply(sampled, function(u) df[df[[unit_col]] == u, , drop = FALSE]))
  })

  dplyr::bind_rows(out) %>%
    dplyr::select(-dplyr::all_of(".stratum"))
}

#' Bootstrap Confidence Intervals for Classical Interval Growth Indices
#'
#' Perform bootstrap resampling for the classical two-harvest module
#' (`classical_growth_interval()`), with support for row, plant, or plot
#' resampling and optional stratification.
#'
#' @inheritParams classical_growth_interval
#' @param resample_unit Resampling unit: `"row"`, `"plant"`, or `"plot"`.
#' @param stratify_by Optional stratification columns for bootstrap sampling.
#'
#' @return A tibble with bootstrap uncertainty summaries for each index and group:
#' `estimate`, `standard_error`, `conf_low`, `conf_high`, and metadata.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' boot_classic <- bootstrap_classical_growth(
#'   data = hunt_classical_example,
#'   time = time,
#'   total_biomass = total_biomass_g,
#'   leaf_biomass = leaf_biomass_g,
#'   root_biomass = root_biomass_g,
#'   stem_biomass = stem_biomass_g,
#'   leaf_area = leaf_area_cm2,
#'   group_by = treatment,
#'   resample_unit = "row",
#'   n_boot = 3,
#'   seed = 101
#' )
#' boot_classic
#'
#' @section Interpretation:
#' Bootstrap intervals reflect empirical sampling uncertainty in interval-average
#' indices under the chosen resampling scheme.
#'
#' @section Limitations:
#' Unit-based bootstrap requires the corresponding id column (`plant_id` or
#' `plot_id`) to be present.
#'
#' @export
bootstrap_classical_growth <- function(
    data,
    time = time,
    total_biomass = total_biomass_g,
    leaf_biomass = leaf_biomass_g,
    stem_biomass = stem_biomass_g,
    root_biomass = root_biomass_g,
    reproductive_biomass = reproductive_biomass_g,
    leaf_area = leaf_area_cm2,
    group_by = NULL,
    interval = NULL,
    resample_unit = c("row", "plant", "plot"),
    stratify_by = "treatment",
    n_boot = 499,
    conf_level = 0.95,
    seed = NULL,
    nar_label = c("ulr", "nar", "both"),
    allometry_method = c("lm", "bivariate_ml"),
    warn = FALSE
) {
  resample_unit <- match.arg(resample_unit)
  nar_label <- match.arg(nar_label)
  allometry_method <- match.arg(allometry_method)

  .assert_conf_level(conf_level)
  .assert_positive_integer(n_boot, arg = "n_boot")

  if (!is.null(seed)) {
    set.seed(seed)
  }

  point_fit <- classical_growth_interval(
    data = data,
    time = {{ time }},
    total_biomass = {{ total_biomass }},
    leaf_biomass = {{ leaf_biomass }},
    stem_biomass = {{ stem_biomass }},
    root_biomass = {{ root_biomass }},
    reproductive_biomass = {{ reproductive_biomass }},
    leaf_area = {{ leaf_area }},
    group_by = {{ group_by }},
    interval = interval,
    ci_method = "classical",
    conf_level = conf_level,
    nar_label = nar_label,
    allometry_method = allometry_method,
    warn = warn
  )

  point <- point_fit$results %>%
    dplyr::filter(.data$ci_source == "classical")

  if (nrow(point) == 0L) {
    return(point)
  }

  group_cols <- .classical_result_cols(point)
  join_cols <- c(group_cols, "index")

  dat <- tibble::as_tibble(data)
  unit_col <- switch(
    resample_unit,
    row = NULL,
    plant = if ("plant_id" %in% names(dat)) "plant_id" else NULL,
    plot = if ("plot_id" %in% names(dat)) "plot_id" else NULL
  )

  if (resample_unit != "row" && is.null(unit_col)) {
    rlang::abort(
      paste0(
        "Bootstrap by `", resample_unit,
        "` requires column `", ifelse(resample_unit == "plant", "plant_id", "plot_id"), "`."
      )
    )
  }

  boot_vals <- vector("list", n_boot)

  for (i in seq_len(n_boot)) {
    bdat <- .classical_boot_resample(
      data = dat,
      resample_unit = resample_unit,
      unit_col = unit_col,
      stratify_by = stratify_by
    )

    bfit <- tryCatch(
      classical_growth_interval(
        data = bdat,
        time = {{ time }},
        total_biomass = {{ total_biomass }},
        leaf_biomass = {{ leaf_biomass }},
        stem_biomass = {{ stem_biomass }},
        root_biomass = {{ root_biomass }},
        reproductive_biomass = {{ reproductive_biomass }},
        leaf_area = {{ leaf_area }},
        group_by = {{ group_by }},
        interval = interval,
        ci_method = "classical",
        conf_level = conf_level,
        nar_label = nar_label,
        allometry_method = allometry_method,
        warn = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(bfit) || nrow(bfit$results) == 0L) {
      next
    }

    bres <- bfit$results %>%
      dplyr::filter(.data$ci_source == "classical") %>%
      dplyr::select(dplyr::all_of(c(join_cols, "estimate"))) %>%
      dplyr::mutate(replicate = i)

    boot_vals[[i]] <- bres
  }

  boot_tbl <- dplyr::bind_rows(boot_vals)

  alpha <- (1 - conf_level) / 2
  boot_sum <- if (nrow(boot_tbl) == 0L) {
    point %>%
      dplyr::select(dplyr::all_of(join_cols)) %>%
      dplyr::mutate(
        standard_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      )
  } else {
    boot_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
      dplyr::summarise(
        standard_error = stats::sd(.data$estimate, na.rm = TRUE),
        conf_low = stats::quantile(.data$estimate, probs = alpha, na.rm = TRUE, names = FALSE),
        conf_high = stats::quantile(.data$estimate, probs = 1 - alpha, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }

  out <- point %>%
    dplyr::left_join(boot_sum, by = join_cols, suffix = c("", ".boot")) %>%
    dplyr::mutate(
      standard_error = dplyr::coalesce(.data$standard_error.boot, .data$standard_error),
      conf_low = dplyr::coalesce(.data$conf_low.boot, .data$conf_low),
      conf_high = dplyr::coalesce(.data$conf_high.boot, .data$conf_high),
      degrees_freedom = NA_real_,
      ci_source = "bootstrap"
    ) %>%
    dplyr::select(-dplyr::any_of(c("standard_error.boot", "conf_low.boot", "conf_high.boot")))

  out
}
