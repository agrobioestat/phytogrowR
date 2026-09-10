#' Check Plant Growth Input Data Quality
#'
#' Validate plant growth datasets before analysis, reporting common problems,
#' severity level, and practical suggestions to fix each issue.
#'
#' @param data A data frame or tibble containing growth observations.
#' @param mapping Optional named list mapping custom column names to phytogrowR
#' standard names (for example, `list(total_biomass_g = "W")`).
#' @param required_cols Character vector of required columns after mapping.
#' @param group_cols Grouping columns used to detect duplicated times.
#' @param total_tolerance Relative tolerance for consistency between
#' `total_biomass_g` and the sum of biomass parts.
#'
#' @details
#' The function checks:
#' - required columns;
#' - negative values;
#' - zero biomass values;
#' - duplicated times within groups;
#' - missing treatment labels;
#' - consistency between total biomass and biomass parts;
#' - probable unit mismatches;
#' - missing data patterns.
#'
#' @return A tibble with columns: `issue`, `severity`, `column`, `n`,
#' `suggestion`.
#'
#' @references
#' Hunt, R. (1990). *Basic Growth Analysis*.
#'
#' Benincasa, M. M. P. (2003). *Plant Growth Analysis: Basic Concepts*.
#' FUNEP, Jaboticabal.
#'
#' @examples
#' issues <- check_growth_data(growth_wide_example)
#' dplyr::count(issues, severity)
#' @export
check_growth_data <- function(
    data,
    mapping = list(),
    required_cols = c("time", "total_biomass_g", "leaf_area_cm2"),
    group_cols = c("treatment", "genotype", "block", "plot_id", "plant_id"),
    total_tolerance = 0.08
) {
  map <- .resolve_mapping(data, mapping)
  dat <- .rename_by_mapping(data, map)

  if (!is.numeric(total_tolerance) || length(total_tolerance) != 1 ||
      is.na(total_tolerance) || total_tolerance < 0) {
    rlang::abort("`total_tolerance` must be a single non-negative numeric value.")
  }

  out <- tibble::tibble(
    issue = character(),
    severity = character(),
    column = character(),
    n = integer(),
    suggestion = character()
  )

  add_issue <- function(issue, severity, column, n, suggestion) {
    out <<- dplyr::bind_rows(
      out,
      tibble::tibble(
        issue = issue,
        severity = severity,
        column = column,
        n = as.integer(n),
        suggestion = suggestion
      )
    )
  }

  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    add_issue(
      issue = "missing_required_columns",
      severity = "error",
      column = paste(missing_cols, collapse = ", "),
      n = length(missing_cols),
      suggestion = "Use `prepare_growth_data()` with a proper mapping or rename columns."
    )
  }

  numeric_targets <- intersect(
    c(
      "time", "total_biomass_g", "leaf_biomass_g", "stem_biomass_g",
      "root_biomass_g", "reproductive_biomass_g", "leaf_area_cm2",
      "ground_area_m2"
    ),
    names(dat)
  )

  for (col in numeric_targets) {
    if (!is.numeric(dat[[col]])) {
      add_issue(
        issue = "non_numeric_column",
        severity = "error",
        column = col,
        n = sum(!is.na(dat[[col]])),
        suggestion = "Convert this column to numeric before analysis."
      )
      next
    }

    neg_n <- sum(dat[[col]] < 0, na.rm = TRUE)
    if (neg_n > 0) {
      add_issue(
        issue = "negative_values",
        severity = "error",
        column = col,
        n = neg_n,
        suggestion = "Negative values are biologically implausible for these measurements."
      )
    }
  }

  if ("total_biomass_g" %in% names(dat)) {
    zero_biomass <- sum(dat$total_biomass_g == 0, na.rm = TRUE)
    if (zero_biomass > 0) {
      add_issue(
        issue = "zero_total_biomass",
        severity = "warning",
        column = "total_biomass_g",
        n = zero_biomass,
        suggestion = "Log-based indices (RGR/NAR) require positive biomass values."
      )
    }
  }

  if ("ground_area_m2" %in% names(dat)) {
    zero_ground <- sum(dat$ground_area_m2 <= 0, na.rm = TRUE)
    if (zero_ground > 0) {
      add_issue(
        issue = "non_positive_ground_area",
        severity = "error",
        column = "ground_area_m2",
        n = zero_ground,
        suggestion = "Ground area must be positive for CGR and LAI calculations."
      )
    }
  }

  if (all(c("time", "treatment") %in% names(dat))) {
    missing_treatment <- sum(is.na(dat$treatment) | trimws(as.character(dat$treatment)) == "")
    if (missing_treatment > 0) {
      add_issue(
        issue = "missing_treatment",
        severity = "warning",
        column = "treatment",
        n = missing_treatment,
        suggestion = "Define treatment labels for all observations."
      )
    }
  }

  if ("time" %in% names(dat)) {
    gcols <- intersect(group_cols, names(dat))

    dup_n <- if (length(gcols) > 0) {
      dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(gcols, "time")))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(.data$n > 1) %>%
        nrow()
    } else {
      sum(duplicated(dat$time))
    }

    if (dup_n > 0) {
      add_issue(
        issue = "duplicated_time_within_group",
        severity = "warning",
        column = "time",
        n = dup_n,
        suggestion = "Check repeated entries or aggregate replicates before interval metrics."
      )
    }
  }

  part_cols <- intersect(
    c("leaf_biomass_g", "stem_biomass_g", "root_biomass_g", "reproductive_biomass_g"),
    names(dat)
  )

  if ("total_biomass_g" %in% names(dat) && length(part_cols) >= 3) {
    part_sum <- rowSums(dat[, part_cols, drop = FALSE], na.rm = TRUE)
    ok <- !is.na(dat$total_biomass_g) & dat$total_biomass_g > 0
    rel_diff <- abs(dat$total_biomass_g[ok] - part_sum[ok]) / dat$total_biomass_g[ok]
    mismatch_n <- sum(rel_diff > total_tolerance, na.rm = TRUE)

    if (mismatch_n > 0) {
      add_issue(
        issue = "total_parts_inconsistency",
        severity = "warning",
        column = "total_biomass_g",
        n = mismatch_n,
        suggestion = "Review dry mass partition and unit consistency across plant organs."
      )
    }
  }

  if ("leaf_area_cm2" %in% names(dat)) {
    la_med <- stats::median(dat$leaf_area_cm2, na.rm = TRUE)
    if (is.finite(la_med) && la_med > 0 && la_med < 3) {
      add_issue(
        issue = "possible_leaf_area_unit_mismatch",
        severity = "info",
        column = "leaf_area_cm2",
        n = sum(!is.na(dat$leaf_area_cm2)),
        suggestion = "Values look small for cm2. Confirm if units are m2."
      )
    }
  }

  if ("total_biomass_g" %in% names(dat)) {
    w_med <- stats::median(dat$total_biomass_g, na.rm = TRUE)
    if (is.finite(w_med) && w_med > 1000) {
      add_issue(
        issue = "possible_biomass_unit_mismatch",
        severity = "info",
        column = "total_biomass_g",
        n = sum(!is.na(dat$total_biomass_g)),
        suggestion = "Values may be in mg instead of g. Verify units before analysis."
      )
    }
  }

  key_cols <- intersect(
    c("time", "treatment", "total_biomass_g", "leaf_area_cm2", "ground_area_m2"),
    names(dat)
  )

  for (col in key_cols) {
    miss_n <- sum(is.na(dat[[col]]))
    if (miss_n > 0) {
      add_issue(
        issue = "missing_values",
        severity = "warning",
        column = col,
        n = miss_n,
        suggestion = "Handle missing values (impute, filter, or model-based approach)."
      )
    }
  }

  if (nrow(out) == 0) {
    out <- tibble::tibble(
      issue = "no_issues_detected",
      severity = "ok",
      column = "-",
      n = 0L,
      suggestion = "Data passed basic checks for growth analysis."
    )
  }

  dplyr::arrange(
    out,
    factor(.data$severity, levels = c("error", "warning", "info", "ok")),
    dplyr::desc(.data$n)
  )
}

