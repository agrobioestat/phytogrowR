.classical_reference_text <- function() {
  paste0(
    "Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002). ",
    "A modern tool for classical plant growth analysis. Annals of Botany, 90(4), 485-488. doi:10.1093/aob/mcf214"
  )
}

.classical_formulas <- function() {
  list(
    rgr = "RGR = (mean(log(W2)) - mean(log(W1))) / (t2 - t1)",
    agr = "AGR = (mean(W2) - mean(W1)) / (t2 - t1)",
    ulr = "ULR = ((W2 - W1)/(t2 - t1)) * ((log(LA2) - log(LA1))/(LA2 - LA1))",
    nar = "NAR = ULR (operational equivalence in this module)",
    sla = "SLA = leaf_area / leaf_biomass",
    lwf = "LWF = leaf_biomass / total_biomass",
    lwr = "LWR = LWF",
    lar = "LAR = leaf_area / total_biomass = SLA * LWF",
    root_shoot_ratio = "root_shoot_ratio = root_biomass / shoot_biomass",
    shoot_root_ratio = "shoot_root_ratio = shoot_biomass / root_biomass",
    root_shoot_allometric_coefficient = "log(root_biomass) = alpha + beta * log(shoot_biomass)"
  )
}

.classical_col_from_quo <- function(data, quo, arg_name, required = TRUE) {
  expr <- rlang::get_expr(quo)
  col <- NULL

  if (rlang::quo_is_null(quo) || is.null(expr)) {
    if (!required) {
      return(NULL)
    }
    rlang::abort(paste0("`", arg_name, "` must refer to a column in `data`."))
  }

  if (is.symbol(expr)) {
    col <- rlang::as_name(expr)
    if (!col %in% names(data)) {
      val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
      if (is.character(val) && length(val) == 1L) {
        col <- val
      }
    }
  } else if (is.character(expr)) {
    if (length(expr) == 1L) {
      col <- expr
    }
  }

  if (is.null(col)) {
    val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
    if (is.character(val) && length(val) == 1L) {
      col <- val
    }
  }

  if (is.null(col)) {
    if (!required) {
      return(NULL)
    }
    rlang::abort(
      paste0(
        "Could not parse `", arg_name, "`. Use a column name, e.g. `",
        arg_name,
        " = total_biomass_g`."
      )
    )
  }

  if (!col %in% names(data)) {
    if (!required) {
      return(NULL)
    }
    rlang::abort(paste0("Column `", col, "` (from `", arg_name, "`) was not found in `data`."))
  }

  col
}

.classical_group_cols <- function(data, group_by_quo) {
  defaults <- intersect(c("treatment", "genotype", "block", "plot_id"), names(data))

  if (rlang::quo_is_null(group_by_quo) || is.null(rlang::get_expr(group_by_quo))) {
    return(defaults)
  }

  expr <- rlang::get_expr(group_by_quo)
  cols <- character()

  if (is.symbol(expr)) {
    candidate <- rlang::as_name(expr)
    if (candidate %in% names(data)) {
      cols <- candidate
    } else {
      val <- tryCatch(rlang::eval_tidy(group_by_quo), error = function(e) NULL)
      if (is.character(val)) {
        cols <- val
      } else {
        cols <- candidate
      }
    }
  } else if (is.character(expr)) {
    cols <- as.character(expr)
  } else if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
    parts <- as.list(expr)[-1]
    cols <- vapply(
      parts,
      function(p) {
        if (is.symbol(p)) {
          rlang::as_name(p)
        } else if (is.character(p) && length(p) == 1L) {
          p
        } else {
          NA_character_
        }
      },
      FUN.VALUE = character(1)
    )
    cols <- cols[!is.na(cols)]
  } else {
    val <- tryCatch(rlang::eval_tidy(group_by_quo), error = function(e) NULL)
    if (is.character(val)) {
      cols <- val
    }
  }

  cols <- unique(cols)
  if (length(cols) == 0L) {
    return(defaults)
  }

  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    rlang::abort(
      paste0("Grouping columns not found in `data`: ", paste(missing, collapse = ", "), ".")
    )
  }

  cols
}

.classical_resolve_interval <- function(dat, interval = NULL) {
  dat <- tibble::as_tibble(dat)

  if (is.null(interval)) {
    uniq_num <- sort(unique(dat$.time_num[!is.na(dat$.time_num)]))

    if (length(uniq_num) < 2L) {
      rlang::abort("At least two harvest times are required.")
    }

    if (length(uniq_num) > 2L) {
      rlang::abort(
        paste0(
          "More than two harvests were found (",
          length(uniq_num),
          "). Please pass `interval = c(t1, t2)` explicitly."
        )
      )
    }

    t1_num <- uniq_num[1]
    t2_num <- uniq_num[2]

    t1_raw <- dat$.time_raw[which(dat$.time_num == t1_num)[1]]
    t2_raw <- dat$.time_raw[which(dat$.time_num == t2_num)[1]]

    keep <- dat$.time_num %in% c(t1_num, t2_num)
    dat <- dat[keep, , drop = FALSE]

    return(list(data = dat, t1_num = t1_num, t2_num = t2_num, t1_raw = t1_raw, t2_raw = t2_raw))
  }

  if (length(interval) != 2L) {
    rlang::abort("`interval` must contain exactly two harvest values.")
  }

  keep <- as.character(dat$.time_raw) %in% as.character(interval)
  dat <- dat[keep, , drop = FALSE]

  uniq_num <- sort(unique(dat$.time_num[!is.na(dat$.time_num)]))
  if (length(uniq_num) != 2L) {
    rlang::abort("`interval` must match exactly two harvests in `data`.")
  }

  t1_num <- uniq_num[1]
  t2_num <- uniq_num[2]

  t1_raw <- dat$.time_raw[which(dat$.time_num == t1_num)[1]]
  t2_raw <- dat$.time_raw[which(dat$.time_num == t2_num)[1]]

  list(data = dat, t1_num = t1_num, t2_num = t2_num, t1_raw = t1_raw, t2_raw = t2_raw)
}

.classical_check_tbl <- function() {
  tibble::tibble(
    issue = character(),
    severity = character(),
    index = character(),
    n = integer(),
    details = character()
  )
}

.classical_add_check <- function(checks, issue, severity, index = NA_character_, n = NA_integer_, details = "") {
  dplyr::bind_rows(
    checks,
    tibble::tibble(
      issue = issue,
      severity = severity,
      index = index,
      n = as.integer(n),
      details = details
    )
  )
}

.classical_var <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1L) {
    return(NA_real_)
  }
  stats::var(x)
}

.classical_interval_ratio <- function(v1, v2) {
  v1 <- v1[is.finite(v1)]
  v2 <- v2[is.finite(v2)]

  if (length(v1) == 0L || length(v2) == 0L) {
    return(list(estimate = NA_real_, se = NA_real_, df = NA_real_, n1 = length(v1), n2 = length(v2)))
  }

  m1 <- mean(v1)
  m2 <- mean(v2)
  est <- (m1 + m2) / 2

  se <- NA_real_
  df <- NA_real_
  if (length(v1) > 1L && length(v2) > 1L) {
    se <- sqrt(stats::var(v1) / length(v1) + stats::var(v2) / length(v2)) / 2
    df <- max(min(length(v1), length(v2)) - 2, 1)
  }

  list(estimate = est, se = se, df = df, n1 = length(v1), n2 = length(v2))
}

.classical_result_cols <- function(tbl) {
  known <- c(
    "index", "estimate", "standard_error", "conf_low", "conf_high", "degrees_freedom",
    "n_harvest_1", "n_harvest_2", "method", "ci_source", "note", "intercept",
    "p_value", "r_squared", "n"
  )
  setdiff(names(tbl), known)
}

.classical_pick_ci <- function(results, ci_source = c("auto", "classical", "bootstrap", "all")) {
  ci_source <- match.arg(ci_source)

  if (!"ci_source" %in% names(results)) {
    return(results)
  }

  if (ci_source == "all") {
    return(results)
  }

  if (ci_source == "classical") {
    return(dplyr::filter(results, .data$ci_source == "classical"))
  }

  if (ci_source == "bootstrap") {
    return(dplyr::filter(results, .data$ci_source == "bootstrap"))
  }

  if (any(results$ci_source == "classical")) {
    return(dplyr::filter(results, .data$ci_source == "classical"))
  }

  results
}

.classical_compute_group <- function(
    gdat,
    t1_num,
    t2_num,
    conf_level,
    nar_label,
    allometry_method
) {
  checks <- .classical_check_tbl()

  h1 <- gdat %>% dplyr::filter(.data$.time_num == t1_num)
  h2 <- gdat %>% dplyr::filter(.data$.time_num == t2_num)

  w1 <- h1$.total_biomass
  w2 <- h2$.total_biomass
  n1 <- sum(!is.na(w1))
  n2 <- sum(!is.na(w2))

  out <- tibble::tibble(
    index = character(),
    estimate = numeric(),
    standard_error = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    degrees_freedom = numeric(),
    n_harvest_1 = integer(),
    n_harvest_2 = integer(),
    method = character(),
    ci_source = character(),
    note = character(),
    intercept = numeric(),
    p_value = numeric(),
    r_squared = numeric(),
    n = integer()
  )

  info <- tibble::tibble(
    t1 = h1$.time_raw[1],
    t2 = h2$.time_raw[1],
    delta_t = t2_num - t1_num,
    n_harvest_1 = n1,
    n_harvest_2 = n2,
    n_total = n1 + n2
  )

  if ((t2_num - t1_num) <= 0) {
    checks <- .classical_add_check(
      checks,
      issue = "invalid_interval",
      severity = "error",
      n = 1,
      details = "t2 - t1 must be positive."
    )
    return(list(results = out, checks = checks, info = info))
  }

  if (n1 < 2 || n2 < 2) {
    checks <- .classical_add_check(
      checks,
      issue = "insufficient_plants_per_harvest",
      severity = "error",
      n = min(n1, n2),
      details = "At least two plants are required in each harvest per interval/group."
    )
    return(list(results = out, checks = checks, info = info))
  }

  dt <- t2_num - t1_num

  add_metric <- function(index, estimate, se, df, note = NA_character_, intercept = NA_real_, p_value = NA_real_, r_squared = NA_real_, n = NA_integer_) {
    ci <- classical_ci(
      estimate = estimate,
      standard_error = se,
      degrees_freedom = df,
      conf_level = conf_level
    )

    row <- tibble::tibble(
      index = index,
      estimate = ci$estimate,
      standard_error = ci$standard_error,
      conf_low = ci$conf_low,
      conf_high = ci$conf_high,
      degrees_freedom = ci$degrees_freedom,
      n_harvest_1 = n1,
      n_harvest_2 = n2,
      method = "classical interval",
      ci_source = "classical",
      note = note,
      intercept = intercept,
      p_value = p_value,
      r_squared = r_squared,
      n = as.integer(n)
    )

    if (!is.na(estimate) && !is.finite(estimate)) {
      row$estimate <- NA_real_
    }

    if (is.na(row$estimate)) {
      checks <<- .classical_add_check(
        checks,
        issue = "index_not_calculated",
        severity = "warning",
        index = index,
        n = 1,
        details = note %||% "Insufficient or invalid data."
      )
    }

    out <<- dplyr::bind_rows(out, row)
  }

  # RGR and AGR ------------------------------------------------------------
  w1_ok <- w1[!is.na(w1)]
  w2_ok <- w2[!is.na(w2)]

  rgr_est <- calc_rgr_hunt(w1 = w1_ok, w2 = w2_ok, delta_t = dt)
  rgr_se <- sqrt(.classical_var(log(w1_ok)) / length(w1_ok) + .classical_var(log(w2_ok)) / length(w2_ok)) / dt
  rgr_df <- max(min(length(w1_ok), length(w2_ok)) - 2, 1)
  add_metric("rgr", rgr_est, rgr_se, rgr_df)

  agr_est <- (mean(w2_ok) - mean(w1_ok)) / dt
  agr_se <- sqrt(.classical_var(w1_ok) / length(w1_ok) + .classical_var(w2_ok) / length(w2_ok)) / dt
  agr_df <- max(min(length(w1_ok), length(w2_ok)) - 2, 1)
  add_metric("agr", agr_est, agr_se, agr_df)

  # ULR/NAR ----------------------------------------------------------------
  la1 <- h1$.leaf_area
  la2 <- h2$.leaf_area
  la1_ok <- la1[!is.na(la1)]
  la2_ok <- la2[!is.na(la2)]

  ulr_note <- NA_character_
  ulr_est <- NA_real_
  ulr_se <- NA_real_
  ulr_df <- NA_real_

  if (length(la1_ok) == 0L || length(la2_ok) == 0L) {
    ulr_note <- "Leaf area values are missing for at least one harvest."
  } else if (mean(la1_ok) <= 0 || mean(la2_ok) <= 0) {
    ulr_note <- "Leaf area means must be positive for ULR/NAR."
  } else if (abs(mean(la2_ok) - mean(la1_ok)) < 1e-12) {
    ulr_note <- "Mean leaf area change is near zero; ULR/NAR undefined."
  } else {
    ulr_est <- calc_ulr(w1 = w1_ok, w2 = w2_ok, la1 = la1_ok, la2 = la2_ok, delta_t = dt)

    var_a <- .classical_var(w1_ok) / length(w1_ok) / (dt^2) + .classical_var(w2_ok) / length(w2_ok) / (dt^2)

    m1 <- mean(la1_ok)
    m2 <- mean(la2_ok)
    u <- log(m2) - log(m1)
    v <- m2 - m1
    dg_dm1 <- (-v / m1 + u) / (v^2)
    dg_dm2 <- (v / m2 - u) / (v^2)
    var_b <- (dg_dm1^2) * (.classical_var(la1_ok) / length(la1_ok)) +
      (dg_dm2^2) * (.classical_var(la2_ok) / length(la2_ok))

    b <- u / v
    a <- (mean(w2_ok) - mean(w1_ok)) / dt
    ulr_var <- (b^2) * var_a + (a^2) * var_b

    if (is.finite(ulr_var) && ulr_var >= 0) {
      ulr_se <- sqrt(ulr_var)
    }

    ulr_df <- max(min(length(w1_ok), length(w2_ok), length(la1_ok), length(la2_ok)) - 2, 1)
  }

  if (nar_label %in% c("ulr", "both")) {
    add_metric("ulr", ulr_est, ulr_se, ulr_df, note = ulr_note)
  }
  if (nar_label %in% c("nar", "both")) {
    add_metric("nar", ulr_est, ulr_se, ulr_df, note = ulr_note)
  }

  # SLA / LWF / LWR / LAR --------------------------------------------------
  sla1 <- .safe_div(h1$.leaf_area, h1$.leaf_biomass)
  sla2 <- .safe_div(h2$.leaf_area, h2$.leaf_biomass)
  sla_stat <- .classical_interval_ratio(sla1, sla2)
  add_metric("sla", sla_stat$estimate, sla_stat$se, sla_stat$df,
             note = if (is.na(sla_stat$estimate)) "SLA unavailable (leaf area and/or leaf biomass missing)." else NA_character_)

  lwf1 <- calc_lwf(leaf_biomass = h1$.leaf_biomass, total_biomass = h1$.total_biomass)
  lwf2 <- calc_lwf(leaf_biomass = h2$.leaf_biomass, total_biomass = h2$.total_biomass)
  lwf_stat <- .classical_interval_ratio(lwf1, lwf2)
  add_metric("lwf", lwf_stat$estimate, lwf_stat$se, lwf_stat$df,
             note = if (is.na(lwf_stat$estimate)) "LWF unavailable (leaf and/or total biomass missing)." else NA_character_)
  add_metric("lwr", lwf_stat$estimate, lwf_stat$se, lwf_stat$df,
             note = if (is.na(lwf_stat$estimate)) "LWR unavailable (leaf and/or total biomass missing)." else NA_character_)

  lar1 <- .safe_div(h1$.leaf_area, h1$.total_biomass)
  lar2 <- .safe_div(h2$.leaf_area, h2$.total_biomass)
  lar_stat <- .classical_interval_ratio(lar1, lar2)

  lar_comp <- calc_lar_components(
    leaf_area = c(h1$.leaf_area, h2$.leaf_area),
    leaf_biomass = c(h1$.leaf_biomass, h2$.leaf_biomass),
    total_biomass = c(h1$.total_biomass, h2$.total_biomass)
  )
  lar_note <- lar_comp$warning[1]

  add_metric("lar", lar_stat$estimate, lar_stat$se, lar_stat$df,
             note = lar_note %||% if (is.na(lar_stat$estimate)) "LAR unavailable (leaf area and/or total biomass missing)." else NA_character_)

  if (!is.na(lar_comp$warning[1])) {
    checks <- .classical_add_check(
      checks,
      issue = "lar_decomposition_warning",
      severity = "warning",
      index = "lar",
      n = 1,
      details = lar_comp$warning[1]
    )
  }

  # Root-shoot / shoot-root ------------------------------------------------
  shoot1 <- h1$.leaf_biomass + h1$.stem_biomass
  shoot2 <- h2$.leaf_biomass + h2$.stem_biomass
  if (!all(is.na(gdat$.reproductive_biomass))) {
    shoot1 <- shoot1 + h1$.reproductive_biomass
    shoot2 <- shoot2 + h2$.reproductive_biomass
  }

  rs1 <- .safe_div(h1$.root_biomass, shoot1)
  rs2 <- .safe_div(h2$.root_biomass, shoot2)
  rs_stat <- .classical_interval_ratio(rs1, rs2)
  add_metric("root_shoot_ratio", rs_stat$estimate, rs_stat$se, rs_stat$df,
             note = if (is.na(rs_stat$estimate)) "Root:shoot ratio unavailable (missing root/shoot biomass)." else NA_character_)

  sr1 <- .safe_div(shoot1, h1$.root_biomass)
  sr2 <- .safe_div(shoot2, h2$.root_biomass)
  sr_stat <- .classical_interval_ratio(sr1, sr2)
  add_metric("shoot_root_ratio", sr_stat$estimate, sr_stat$se, sr_stat$df,
             note = if (is.na(sr_stat$estimate)) "Shoot:root ratio unavailable (missing root/shoot biomass)." else NA_character_)

  # Allometry ---------------------------------------------------------------
  root_all <- c(h1$.root_biomass, h2$.root_biomass)
  shoot_all <- c(shoot1, shoot2)

  allom <- calc_root_shoot_allometry(
    root_biomass = root_all,
    shoot_biomass = shoot_all,
    method = allometry_method,
    conf_level = conf_level
  )

  allom_note <- if (is.na(allom$beta[1])) {
    "Root-shoot allometry requires at least five valid plants with positive root and shoot biomass."
  } else {
    NA_character_
  }

  add_metric(
    index = "root_shoot_allometric_coefficient",
    estimate = allom$beta[1],
    se = allom$standard_error[1],
    df = allom$degrees_freedom[1],
    note = allom_note,
    intercept = allom$intercept[1],
    p_value = allom$p_value[1],
    r_squared = allom$r_squared[1],
    n = allom$n[1]
  )

  if (!is.na(allom_note)) {
    checks <- .classical_add_check(
      checks,
      issue = "insufficient_allometry_samples",
      severity = "warning",
      index = "root_shoot_allometric_coefficient",
      n = allom$n[1],
      details = allom_note
    )
  }

  list(results = out, checks = checks, info = info)
}

#' Classical Plant Growth Analysis by Two-Harvest Interval
#'
#' Compute classical interval growth indices from two successive harvests,
#' without mandatory plant pairing and without functional curve fitting.
#'
#' @param data A data frame with harvest observations.
#' @param time Time or harvest-date column.
#' @param total_biomass Total biomass column.
#' @param leaf_biomass Leaf biomass column.
#' @param stem_biomass Stem biomass column.
#' @param root_biomass Root biomass column.
#' @param reproductive_biomass Optional reproductive biomass column.
#' @param leaf_area Leaf area column.
#' @param group_by Grouping columns (unquoted names or character vector).
#' @param interval Optional length-2 vector defining the two harvests.
#' @param ci_method One of `"classical"`, `"bootstrap"`, or `"both"`.
#' @param bootstrap_unit Bootstrap resampling unit (`"row"`, `"plant"`, `"plot"`).
#' @param bootstrap_stratify Optional stratification columns for bootstrap.
#' @param n_boot Number of bootstrap resamples.
#' @param conf_level Confidence level.
#' @param seed Optional random seed.
#' @param nar_label One of `"ulr"`, `"nar"`, or `"both"`.
#' @param allometry_method One of `"lm"` or `"bivariate_ml"`.
#' @param total_tolerance Relative tolerance for total biomass consistency checks.
#' @param warn Logical; emit warnings for non-fatal checks.
#'
#' @return An S3 object of class `phytogrow_classical` with components:
#' - `results`: tibble with interval indices and uncertainty;
#' - `input_data`: data used in analysis;
#' - `checks`: tibble with warnings/issues;
#' - `interval_info`: interval metadata;
#' - `formulas`: list of formulas;
#' - `ci_method`: selected CI method;
#' - `reference`: Hunt et al. (2002) reference.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' fit_classic <- classical_growth_interval(
#'   data = hunt_classical_example,
#'   time = time,
#'   total_biomass = total_biomass_g,
#'   leaf_biomass = leaf_biomass_g,
#'   root_biomass = root_biomass_g,
#'   stem_biomass = stem_biomass_g,
#'   leaf_area = leaf_area_cm2,
#'   group_by = treatment,
#'   ci_method = "classical"
#' )
#' print(fit_classic)
#' summary(fit_classic)
#' tidy(fit_classic)
#' plot(fit_classic, metric = "rgr")
#'
#' @section Interpretation:
#' Reported indices are interval means between two harvests, not instantaneous
#' derivatives. For long time series with many harvests, functional approaches
#' may be more appropriate.
#'
#' @section Limitations:
#' The method requires exactly two harvests per interval and positive values for
#' variables used in logarithmic transformations.
#'
#' @export
classical_growth_interval <- function(
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
    ci_method = c("classical", "bootstrap", "both"),
    bootstrap_unit = c("row", "plant", "plot"),
    bootstrap_stratify = "treatment",
    n_boot = 499,
    conf_level = 0.95,
    seed = NULL,
    nar_label = c("ulr", "nar", "both"),
    allometry_method = c("lm", "bivariate_ml"),
    total_tolerance = 0.12,
    warn = TRUE
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  ci_method <- match.arg(ci_method)
  bootstrap_unit <- match.arg(bootstrap_unit)
  nar_label <- match.arg(nar_label)
  allometry_method <- match.arg(allometry_method)

  .assert_conf_level(conf_level)
  .assert_positive_integer(n_boot, arg = "n_boot")

  dat <- tibble::as_tibble(data)

  time_col <- .classical_col_from_quo(dat, rlang::enquo(time), "time", required = TRUE)
  total_col <- .classical_col_from_quo(dat, rlang::enquo(total_biomass), "total_biomass", required = TRUE)
  leaf_col <- .classical_col_from_quo(dat, rlang::enquo(leaf_biomass), "leaf_biomass", required = FALSE)
  stem_col <- .classical_col_from_quo(dat, rlang::enquo(stem_biomass), "stem_biomass", required = FALSE)
  root_col <- .classical_col_from_quo(dat, rlang::enquo(root_biomass), "root_biomass", required = FALSE)
  repro_col <- .classical_col_from_quo(dat, rlang::enquo(reproductive_biomass), "reproductive_biomass", required = FALSE)
  leaf_area_col <- .classical_col_from_quo(dat, rlang::enquo(leaf_area), "leaf_area", required = FALSE)

  group_cols <- .classical_group_cols(dat, rlang::enquo(group_by))

  dat <- dat %>%
    dplyr::mutate(
      .time_raw = .data[[time_col]],
      .time_num = as.numeric(.data[[time_col]]),
      .total_biomass = as.numeric(.data[[total_col]]),
      .leaf_biomass = if (!is.null(leaf_col)) as.numeric(.data[[leaf_col]]) else NA_real_,
      .stem_biomass = if (!is.null(stem_col)) as.numeric(.data[[stem_col]]) else NA_real_,
      .root_biomass = if (!is.null(root_col)) as.numeric(.data[[root_col]]) else NA_real_,
      .reproductive_biomass = if (!is.null(repro_col)) as.numeric(.data[[repro_col]]) else NA_real_,
      .leaf_area = if (!is.null(leaf_area_col)) as.numeric(.data[[leaf_area_col]]) else NA_real_
    )

  if (any(is.na(dat$.time_num) & !is.na(dat$.time_raw))) {
    rlang::abort("`time` must be numeric or coercible to numeric for interval analysis.")
  }

  checks <- .classical_check_tbl()

  # Basic validations -------------------------------------------------------
  numeric_cols <- c(".total_biomass", ".leaf_biomass", ".stem_biomass", ".root_biomass", ".reproductive_biomass", ".leaf_area")
  for (cl in numeric_cols) {
    neg_n <- sum(dat[[cl]] < 0, na.rm = TRUE)
    if (neg_n > 0) {
      rlang::abort(paste0("Negative values are not allowed in classical analysis (column `", cl, "`)."))
    }
  }

  if (any(dat$.total_biomass <= 0, na.rm = TRUE)) {
    rlang::abort("Zero or negative total biomass is not allowed for log-based indices (RGR).")
  }

  if (!all(is.na(dat$.leaf_area)) && any(dat$.leaf_area <= 0, na.rm = TRUE)) {
    rlang::abort("Zero or negative leaf area is not allowed for log-based ULR/NAR calculations.")
  }

  miss_cols <- c(".time_num", ".total_biomass")
  if (!is.null(leaf_col)) miss_cols <- c(miss_cols, ".leaf_biomass")
  if (!is.null(stem_col)) miss_cols <- c(miss_cols, ".stem_biomass")
  if (!is.null(root_col)) miss_cols <- c(miss_cols, ".root_biomass")
  if (!is.null(repro_col)) miss_cols <- c(miss_cols, ".reproductive_biomass")
  if (!is.null(leaf_area_col)) miss_cols <- c(miss_cols, ".leaf_area")

  miss_n <- sum(!stats::complete.cases(dat[, miss_cols, drop = FALSE]))
  if (miss_n > 0) {
    checks <- .classical_add_check(
      checks,
      issue = "missing_values",
      severity = "warning",
      n = miss_n,
      details = "Missing values were detected; indices are calculated with available observations."
    )
    if (isTRUE(warn)) {
      warning("Missing values detected; some indices may not be calculable.", call. = FALSE)
    }
  }

  part_available <- c(".leaf_biomass", ".stem_biomass", ".root_biomass")
  if (all(part_available %in% names(dat)) && !all(is.na(dat$.leaf_biomass)) && !all(is.na(dat$.stem_biomass)) && !all(is.na(dat$.root_biomass))) {
    part_sum <- dat$.leaf_biomass + dat$.stem_biomass + dat$.root_biomass
    if (!all(is.na(dat$.reproductive_biomass))) {
      part_sum <- part_sum + dat$.reproductive_biomass
    }

    ok <- !is.na(part_sum) & !is.na(dat$.total_biomass) & dat$.total_biomass > 0
    rel_diff <- abs(dat$.total_biomass[ok] - part_sum[ok]) / dat$.total_biomass[ok]
    bad_n <- sum(rel_diff > total_tolerance, na.rm = TRUE)
    if (bad_n > 0) {
      checks <- .classical_add_check(
        checks,
        issue = "total_parts_inconsistency",
        severity = "warning",
        n = bad_n,
        details = "Total biomass differs substantially from the sum of components."
      )
      if (isTRUE(warn)) {
        warning("Total biomass differs from sum of parts for some rows.", call. = FALSE)
      }
    }
  }

  if (!all(is.na(dat$.leaf_area))) {
    la_med <- stats::median(dat$.leaf_area, na.rm = TRUE)
    if (is.finite(la_med) && la_med > 0 && la_med < 3) {
      checks <- .classical_add_check(
        checks,
        issue = "possible_leaf_area_unit_mismatch",
        severity = "info",
        n = sum(!is.na(dat$.leaf_area)),
        details = "Leaf area values appear very small for cm2; check units."
      )
    }
  }

  w_med <- stats::median(dat$.total_biomass, na.rm = TRUE)
  if (is.finite(w_med) && w_med > 1000) {
    checks <- .classical_add_check(
      checks,
      issue = "possible_biomass_unit_mismatch",
      severity = "info",
      n = sum(!is.na(dat$.total_biomass)),
      details = "Biomass values are high for g; check whether units are mg."
    )
  }

  # Interval selection ------------------------------------------------------
  int <- .classical_resolve_interval(dat, interval = interval)
  dat_int <- int$data

  delta_t <- int$t2_num - int$t1_num
  if (!is.finite(delta_t) || delta_t <= 0) {
    rlang::abort("Invalid interval: `t2 - t1` must be positive.")
  }

  if (length(group_cols) == 0L) {
    dat_int$.group_key <- "all"
  } else {
    dat_int$.group_key <- .make_group_key(dat_int, group_cols)
  }

  split_data <- split(dat_int, dat_int$.group_key)

  results_list <- vector("list", length(split_data))
  checks_list <- vector("list", length(split_data))
  info_list <- vector("list", length(split_data))

  i <- 0L
  for (key in names(split_data)) {
    i <- i + 1L
    gdat <- tibble::as_tibble(split_data[[key]])

    comp <- .classical_compute_group(
      gdat = gdat,
      t1_num = int$t1_num,
      t2_num = int$t2_num,
      conf_level = conf_level,
      nar_label = nar_label,
      allometry_method = allometry_method
    )

    gvals <- if (length(group_cols) > 0L) {
      dplyr::slice(gdat, 1) %>% dplyr::select(dplyr::all_of(group_cols))
    } else {
      tibble::tibble(group = "all")
    }

    if (nrow(comp$results) > 0) {
      comp$results <- dplyr::bind_cols(gvals[rep(1, nrow(comp$results)), , drop = FALSE], comp$results)
    }
    if (nrow(comp$checks) > 0) {
      comp$checks <- dplyr::bind_cols(gvals[rep(1, nrow(comp$checks)), , drop = FALSE], comp$checks)
    }
    if (nrow(comp$info) > 0) {
      comp$info <- dplyr::bind_cols(gvals, comp$info)
    }

    results_list[[i]] <- comp$results
    checks_list[[i]] <- comp$checks
    info_list[[i]] <- comp$info
  }

  results <- dplyr::bind_rows(results_list)
  checks <- dplyr::bind_rows(checks, dplyr::bind_rows(checks_list))
  interval_info <- dplyr::bind_rows(info_list)

  if (nrow(checks) == 0L) {
    checks <- tibble::tibble(
      issue = "no_issues_detected",
      severity = "ok",
      index = NA_character_,
      n = 0L,
      details = "No checks flagged for classical interval analysis."
    )
  }

  # Bootstrap CI ------------------------------------------------------------
  if (ci_method %in% c("bootstrap", "both")) {
    boot_tbl <- bootstrap_classical_growth(
      data = data,
      time = time_col,
      total_biomass = total_col,
      leaf_biomass = if (!is.null(leaf_col)) leaf_col else NULL,
      stem_biomass = if (!is.null(stem_col)) stem_col else NULL,
      root_biomass = if (!is.null(root_col)) root_col else NULL,
      reproductive_biomass = if (!is.null(repro_col)) repro_col else NULL,
      leaf_area = if (!is.null(leaf_area_col)) leaf_area_col else NULL,
      group_by = group_cols,
      interval = c(int$t1_raw, int$t2_raw),
      resample_unit = bootstrap_unit,
      stratify_by = bootstrap_stratify,
      n_boot = n_boot,
      conf_level = conf_level,
      seed = seed,
      nar_label = nar_label,
      allometry_method = allometry_method,
      warn = FALSE
    )

    if (ci_method == "bootstrap") {
      results <- boot_tbl
    } else {
      results <- dplyr::bind_rows(results, boot_tbl)
    }
  }

  out <- list(
    results = results,
    input_data = dat_int,
    checks = checks,
    interval_info = interval_info,
    formulas = .classical_formulas(),
    ci_method = ci_method,
    reference = .classical_reference_text()
  )

  class(out) <- "phytogrow_classical"
  out
}

#' @export
print.phytogrow_classical <- function(x, ...) {
  n_groups <- if (nrow(x$interval_info) > 0) nrow(x$interval_info) else 0
  n_idx <- if (nrow(x$results) > 0) length(unique(x$results$index)) else 0

  cat("<phytogrow_classical>\n")
  cat(" CI method:", x$ci_method, "\n")
  cat(" Groups   :", n_groups, "\n")
  cat(" Indices  :", n_idx, "\n")

  invisible(x)
}

#' @export
summary.phytogrow_classical <- function(object, ...) {
  res <- .classical_pick_ci(object$results, ci_source = "auto")

  group_cols <- .classical_result_cols(res)
  agg <- if (nrow(res) == 0) {
    tibble::tibble(index = character(), mean_estimate = numeric(), n_groups = integer())
  } else {
    res %>%
      dplyr::group_by(.data$index) %>%
      dplyr::summarise(
        mean_estimate = mean(.data$estimate, na.rm = TRUE),
        n_groups = if (length(group_cols) == 0) 1L else dplyr::n_distinct(interaction(!!!rlang::syms(group_cols), drop = TRUE)),
        .groups = "drop"
      )
  }

  out <- list(
    ci_method = object$ci_method,
    reference = object$reference,
    checks = object$checks,
    by_index = agg,
    interval_info = object$interval_info
  )

  class(out) <- "summary.phytogrow_classical"
  out
}

#' @export
print.summary.phytogrow_classical <- function(x, ...) {
  cat("phytogrow_classical summary\n")
  cat(" CI method:", x$ci_method, "\n")
  cat(" Groups   :", nrow(x$interval_info), "\n")
  cat(" Checks   :", nrow(x$checks), "\n")
  if (nrow(x$by_index) > 0) {
    print(x$by_index)
  }
  invisible(x)
}

#' @rdname tidy
#' @param ci_source For `phytogrow_classical`, choose `"auto"`, `"classical"`,
#' `"bootstrap"`, or `"all"` to filter returned rows.
#' @export
tidy.phytogrow_classical <- function(x, ci_source = c("auto", "classical", "bootstrap", "all"), ...) {
  ci_source <- match.arg(ci_source)
  .classical_pick_ci(x$results, ci_source = ci_source)
}

#' @export
plot.phytogrow_classical <- function(
    x,
    metric = c("rgr", "ulr", "nar", "sla", "lwf", "lar", "root_shoot_ratio", "root_shoot_allometric_coefficient"),
    ci_source = c("auto", "classical", "bootstrap"),
    group_var = NULL,
    show_ci = TRUE,
    ...
) {
  metric <- match.arg(metric)
  ci_source <- match.arg(ci_source)

  dat <- tidy.phytogrow_classical(x, ci_source = ci_source) %>%
    dplyr::filter(.data$index == metric)

  if (nrow(dat) == 0) {
    rlang::abort("No rows available for the selected metric/ci_source.")
  }

  group_cols <- .classical_result_cols(dat)
  if (!is.null(group_var) && group_var %in% names(dat)) {
    xcol <- group_var
  } else {
    xcol <- intersect(c("treatment", "genotype", "block", "plot_id", "group"), group_cols)
    xcol <- if (length(xcol) > 0) xcol[1] else NULL
  }

  if (is.null(xcol)) {
    dat$.group_axis <- "all"
  } else {
    dat$.group_axis <- as.character(dat[[xcol]])
  }

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$.group_axis, y = .data$estimate)) +
    ggplot2::geom_point(size = 2.8)

  if (isTRUE(show_ci)) {
    plt <- plt + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high),
      width = 0.15,
      na.rm = TRUE
    )
  }

  plt +
    ggplot2::labs(
      x = ifelse(is.null(xcol), "group", xcol),
      y = metric,
      title = paste("Classical interval index:", metric)
    ) +
    ggplot2::theme_minimal()
}
