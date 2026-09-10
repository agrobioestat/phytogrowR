# ---------------------------------------------------------------------------
# Formal test of coincidence of growth curves between treatments
# ---------------------------------------------------------------------------
#
# THE STATISTICAL IDEA (didactic note)
# ------------------------------------
# Comparing treatments by their growth *curves* (rather than by one index at a
# time) is a nested-model problem. Let the response of plant i in treatment g
# be described by a common functional form f(t; theta) with treatment-specific
# parameter vectors theta_g, and Gaussian, homoscedastic, independent errors:
#
#     y_ig = f(t_ig ; theta_g) + e_ig ,   e_ig ~ N(0, sigma^2)
#
# Two hypotheses are compared:
#
#   H0 (reduced / "coincident curves"): theta_1 = theta_2 = ... = theta_k
#        -> ONE curve describes every treatment;      p0 = p        parameters
#   H1 (full    / "separate curves"):   theta_g free for every g
#        -> ONE curve PER treatment;                  p1 = k * p    parameters
#
# H0 is nested inside H1 (it is H1 with k*p - p restrictions), which is exactly
# the situation the two classical tools below were built for.
#
#  (a) F test for nested models (Chow 1960; Graybill 1976; known in the
#      Brazilian biometrics literature as the "test of identity of models").
#      With RSS = residual sum of squares and df = n - p:
#
#              (RSS0 - RSS1) / (p1 - p0)
#         F = ----------------------------  ~  F(p1 - p0, n - p1)   under H0
#                  RSS1 / (n - p1)
#
#      The numerator is the extra lack of fit paid for forcing one single
#      curve; the denominator is pure error. For LINEAR models (the polynomial
#      family) this F test is EXACT under normality.
#
#  (b) Likelihood ratio test. With sigma^2 profiled out (sigma2_hat = RSS / n),
#      the Gaussian log-likelihood depends on the data only through RSS, so
#
#         G = -2 (logL0 - logL1) = n * log(RSS0 / RSS1)  ~  Chi2(p1 - p0)
#
#      This is ASYMPTOTIC. It is the natural choice for the non-linear
#      families, where the F test is itself only approximate.
#
# Both statistics come from the SAME pair of nested fits, so they can never
# disagree about WHICH models were compared - only about the reference
# distribution used to obtain the p-value.
#
# WHY THE FULL MODEL IS FITTED PER GROUP
# --------------------------------------
# Fitting f separately within each treatment and pooling the residual sums of
# squares (RSS1 = sum_g RSS_g, p1 = k * p) is algebraically identical to the
# joint model with every parameter indexed by treatment, because the least
# squares objective separates over groups. It is far more numerically stable
# for non-linear models, so it is used as the reference full fit; the joint
# indexed model is only fitted afterwards (started from those estimates) to
# obtain the parameter-by-parameter tests.

# Model catalogue ------------------------------------------------------------

.growth_model_params <- function(model) {
  switch(
    model,
    exponential = c("A", "r"),
    logistic = c("Asym", "xmid", "scal"),
    gompertz = c("Asym", "b", "c"),
    richards = c("Asym", "nu", "k", "xmid"),
    rlang::abort("Unsupported non-linear growth model.")
  )
}

# Builds the right-hand side of the nls formula. `par` is a named character
# vector giving the *expression* used for each parameter: either "Asym" (one
# common value) or "Asym[g_idx]" (one value per treatment).
.growth_model_rhs <- function(model, par) {
  switch(
    model,
    exponential = sprintf("%s * exp(%s * x)", par[["A"]], par[["r"]]),
    logistic = sprintf(
      "%s / (1 + exp((%s - x) / %s))",
      par[["Asym"]], par[["xmid"]], par[["scal"]]
    ),
    gompertz = sprintf(
      "%s * exp(-%s * exp(-%s * x))",
      par[["Asym"]], par[["b"]], par[["c"]]
    ),
    richards = sprintf(
      "%s / ((1 + %s * exp(-%s * (x - %s)))^(1 / %s))",
      par[["Asym"]], par[["nu"]], par[["k"]], par[["xmid"]], par[["nu"]]
    ),
    rlang::abort("Unsupported non-linear growth model.")
  )
}

.growth_model_start <- function(model, x, y) {
  rng <- diff(range(x, na.rm = TRUE))
  if (!is.finite(rng) || rng <= 0) rng <- 1
  ymax <- max(y, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  switch(
    model,
    exponential = {
      # Start from the log-linear (classical RGR) regression: ln(y) = ln(A) + r t
      pos <- is.finite(y) & y > 0
      cf <- if (sum(pos) >= 2) {
        stats::coef(stats::lm(log(y[pos]) ~ x[pos]))
      } else {
        c(log(max(ymax, 1e-6)), 0.05)
      }
      if (!all(is.finite(cf))) cf <- c(log(max(ymax, 1e-6)), 0.05)
      list(A = exp(unname(cf[1])), r = unname(cf[2]))
    },
    logistic = list(
      Asym = ymax * 1.05,
      xmid = stats::median(x, na.rm = TRUE),
      scal = rng / 4
    ),
    gompertz = list(
      Asym = ymax * 1.05,
      b = 2,
      c = 4 / rng
    ),
    richards = list(
      Asym = ymax * 1.1,
      nu = 1,
      k = 4 / rng,
      xmid = stats::median(x, na.rm = TRUE)
    )
  )
}

.nls_control <- function() {
  stats::nls.control(maxiter = 500, minFactor = 1e-10, warnOnly = FALSE)
}

# Fitting engines ------------------------------------------------------------

# Fits one non-linear curve to a single group. Returns NULL on failure.
.fit_group_nls <- function(df, model) {
  start <- .growth_model_start(model, df$x, df$y)
  par <- stats::setNames(names(start), names(start))
  form <- stats::as.formula(paste("y ~", .growth_model_rhs(model, par)))

  fit <- tryCatch(
    stats::nls(form, data = df, start = start, control = .nls_control()),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(fit)) {
    # Second attempt with the bounded "port" algorithm, which copes better with
    # poor starting values.
    fit <- tryCatch(
      stats::nls(
        form,
        data = df,
        start = start,
        algorithm = "port",
        control = .nls_control()
      ),
      error = function(e) NULL,
      warning = function(w) NULL
    )
  }

  fit
}

# Fits the joint model in which the `group_specific` parameters are indexed by
# treatment and the remaining ones are common. Returns NULL on failure.
.fit_joint_nls <- function(df, model, group_specific, start_list) {
  pars <- .growth_model_params(model)
  par_expr <- stats::setNames(
    ifelse(pars %in% group_specific, paste0(pars, "[g_idx]"), pars),
    pars
  )
  form <- stats::as.formula(paste("y ~", .growth_model_rhs(model, par_expr)))

  tryCatch(
    stats::nls(form, data = df, start = start_list, control = .nls_control()),
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

.rss <- function(fit) sum(stats::resid(fit)^2, na.rm = TRUE)

# Number of parameters that were actually estimated. `lm` reports it as `rank`;
# aliased (NA) coefficients must not be counted.
.model_rank <- function(fit) {
  if (!is.null(fit$rank)) {
    return(as.integer(fit$rank))
  }

  sum(!is.na(stats::coef(fit)))
}

# Test statistics ------------------------------------------------------------

# The single place where the arithmetic of sections (a)/(b) above is applied.
.nested_test <- function(rss0, rss1, p0, p1, n, hypothesis, term, tests) {
  df_num <- p1 - p0
  df_den <- n - p1

  invalid <- df_num <= 0 || df_den <= 0 ||
    !is.finite(rss0) || !is.finite(rss1) || rss1 <= 0 || rss0 < rss1

  if (invalid) {
    return(tibble::tibble(
      hypothesis = hypothesis, term = term, test = NA_character_,
      df_num = NA_real_, df_den = NA_real_,
      rss_reduced = as.numeric(rss0), rss_full = as.numeric(rss1),
      statistic = NA_real_, p_value = NA_real_
    ))
  }

  out <- list()

  if ("F" %in% tests) {
    f_stat <- ((rss0 - rss1) / df_num) / (rss1 / df_den)
    out[[length(out) + 1L]] <- tibble::tibble(
      hypothesis = hypothesis, term = term, test = "F",
      df_num = as.numeric(df_num), df_den = as.numeric(df_den),
      rss_reduced = rss0, rss_full = rss1,
      statistic = f_stat,
      # Upper tail only: an INCREASE in residual variation is the evidence
      # against the restriction.
      p_value = stats::pf(f_stat, df_num, df_den, lower.tail = FALSE)
    )
  }

  if ("LRT" %in% tests) {
    # G = n * log(RSS0 / RSS1) equals -2 * (logL0 - logL1) for Gaussian errors
    # with sigma^2 profiled out at RSS / n.
    g_stat <- n * log(rss0 / rss1)
    out[[length(out) + 1L]] <- tibble::tibble(
      hypothesis = hypothesis, term = term, test = "LRT",
      df_num = as.numeric(df_num), df_den = NA_real_,
      rss_reduced = rss0, rss_full = rss1,
      statistic = g_stat,
      p_value = stats::pchisq(g_stat, df = df_num, lower.tail = FALSE)
    )
  }

  dplyr::bind_rows(out)
}

# Polynomial (linear model) route --------------------------------------------

.poly_terms <- function(degree) {
  paste0("I(x^", seq_len(degree), ")", collapse = " + ")
}

.compare_poly <- function(df, degree, tests) {
  terms_rhs <- .poly_terms(degree)

  # Reduced  : one common polynomial        -> degree + 1      parameters
  # Parallel : common shape, own intercept  -> degree + k       parameters
  # Full     : one polynomial per treatment -> k * (degree + 1) parameters
  m_red <- stats::lm(stats::as.formula(paste("y ~", terms_rhs)), data = df)
  m_par <- stats::lm(stats::as.formula(paste("y ~", terms_rhs, "+ g")), data = df)
  m_full <- stats::lm(stats::as.formula(paste("y ~ (", terms_rhs, ") * g")), data = df)

  n <- nrow(df)

  # The RANK, not the length of the coefficient vector. When the design cannot
  # support every term (for example a quadratic fitted to two harvest dates),
  # lm() aliases the surplus coefficients to NA; counting them would inflate
  # the numerator df and deflate the denominator df, silently biasing the test.
  p_red <- .model_rank(m_red)
  p_par <- .model_rank(m_par)
  p_full <- .model_rank(m_full)

  res <- dplyr::bind_rows(
    .nested_test(
      .rss(m_red), .rss(m_full), p_red, p_full, n,
      hypothesis = "coincidence",
      term = "all coefficients equal across groups",
      tests = tests
    ),
    .nested_test(
      .rss(m_red), .rss(m_par), p_red, p_par, n,
      hypothesis = "level",
      term = "equal intercepts given a common shape",
      tests = tests
    ),
    .nested_test(
      .rss(m_par), .rss(m_full), p_par, p_full, n,
      hypothesis = "shape",
      term = "equal time coefficients (parallel curves)",
      tests = tests
    )
  )

  list(
    tests = res,
    models = list(reduced = m_red, parallel = m_par, full = m_full),
    n_par = c(reduced = p_red, parallel = p_par, full = p_full),
    status = "ok",
    message = "Polynomial (linear model) comparison; the F test is exact under normality."
  )
}

# Non-linear route -----------------------------------------------------------

.compare_nonlinear <- function(df, model, tests) {
  pars <- .growth_model_params(model)
  p <- length(pars)
  levs <- levels(df$g)
  k <- length(levs)
  n <- nrow(df)

  # --- Full model: one independent fit per treatment ------------------------
  group_fits <- lapply(levs, function(lv) {
    .fit_group_nls(df[df$g == lv, , drop = FALSE], model)
  })
  names(group_fits) <- levs

  failed <- levs[vapply(group_fits, is.null, logical(1))]
  if (length(failed) > 0) {
    rlang::abort(
      paste0(
        "The `", model, "` model did not converge for group(s): ",
        paste(failed, collapse = ", "),
        ". Try `model = 'poly'`, another non-linear family, or more harvests per group."
      )
    )
  }

  rss_full <- sum(vapply(group_fits, .rss, numeric(1)))
  p_full <- k * p

  # --- Reduced model: a single common curve ---------------------------------
  m_red <- .fit_group_nls(df, model)

  if (is.null(m_red)) {
    # Fall back to the average of the per-group estimates as starting values.
    coef_mat0 <- do.call(rbind, lapply(group_fits, function(f) stats::coef(f)[pars]))
    avg_start <- as.list(colMeans(coef_mat0, na.rm = TRUE))
    names(avg_start) <- pars
    par_common <- stats::setNames(pars, pars)
    form_common <- stats::as.formula(paste("y ~", .growth_model_rhs(model, par_common)))
    m_red <- tryCatch(
      stats::nls(form_common, data = df, start = avg_start, control = .nls_control()),
      error = function(e) NULL,
      warning = function(w) NULL
    )
  }

  if (is.null(m_red)) {
    rlang::abort(
      paste0(
        "The common (reduced) `", model,
        "` curve did not converge; the coincidence test cannot be computed."
      )
    )
  }

  res <- .nested_test(
    .rss(m_red), rss_full, p, p_full, n,
    hypothesis = "coincidence",
    term = "all parameters equal across groups",
    tests = tests
  )

  # --- Parameter-by-parameter tests ----------------------------------------
  # Restricting ONE parameter at a time to be common, while the others stay
  # treatment-specific, isolates which biological feature actually differs:
  # final size (Asym), timing (xmid, b) or rate (scal, c, k, r).
  coef_mat <- do.call(rbind, lapply(group_fits, function(f) stats::coef(f)[pars]))
  colnames(coef_mat) <- pars
  start_full <- lapply(pars, function(pp) unname(coef_mat[, pp]))
  names(start_full) <- pars

  joint_full <- .fit_joint_nls(df, model, group_specific = pars, start_list = start_full)

  status <- "ok"
  msg <- paste0(
    "Non-linear (", model, ") comparison; the F test is approximate and the LRT asymptotic."
  )

  if (is.null(joint_full)) {
    status <- "partial"
    msg <- paste(msg, "Parameter-wise tests unavailable (the joint model did not converge).")
  } else {
    rss_joint <- .rss(joint_full)
    p_joint <- length(stats::coef(joint_full))

    for (pp in pars) {
      gs <- setdiff(pars, pp)
      start_r <- start_full[gs]
      start_r[[pp]] <- mean(coef_mat[, pp], na.rm = TRUE)

      m_r <- .fit_joint_nls(df, model, group_specific = gs, start_list = start_r)

      if (is.null(m_r)) {
        res <- dplyr::bind_rows(res, tibble::tibble(
          hypothesis = "parameter", term = pp, test = NA_character_,
          df_num = NA_real_, df_den = NA_real_,
          rss_reduced = NA_real_, rss_full = rss_joint,
          statistic = NA_real_, p_value = NA_real_
        ))
        next
      }

      res <- dplyr::bind_rows(res, .nested_test(
        .rss(m_r), rss_joint,
        p0 = length(stats::coef(m_r)), p1 = p_joint,
        n = n, hypothesis = "parameter", term = pp, tests = tests
      ))
    }
  }

  list(
    tests = res,
    models = list(reduced = m_red, full = joint_full, group_fits = group_fits),
    n_par = c(reduced = p, full = p_full),
    status = status,
    message = msg
  )
}

# Data preparation -----------------------------------------------------------

.curve_test_frame <- function(data, response, time_col, group_var, log_response, epsilon) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  for (nm in c(response, time_col, group_var)) {
    if (!nm %in% names(data)) {
      rlang::abort(paste0("Column `", nm, "` was not found in `data`."))
    }
  }

  df <- tibble::tibble(
    x = as.numeric(data[[time_col]]),
    y = as.numeric(data[[response]]),
    g = as.character(data[[group_var]])
  )

  df <- df[stats::complete.cases(df) & is.finite(df$x) & is.finite(df$y), , drop = FALSE]

  if (nrow(df) == 0) {
    rlang::abort("No complete cases available after removing missing values.")
  }

  if (isTRUE(log_response)) {
    # ln(W) is the classical scale of functional growth analysis: the slope of
    # ln(W) against time IS the relative growth rate.
    df$y <- .safe_log(df$y, epsilon = epsilon, name = response)
    df <- df[is.finite(df$y), , drop = FALSE]
  }

  df$g <- factor(df$g, levels = sort(unique(df$g)))
  df
}

# Per-treatment parameter estimates ------------------------------------------

.curve_test_parameters <- function(fit_res, df, model, degree, levs) {
  if (identical(model, "poly")) {
    rows <- lapply(levs, function(lv) {
      sub <- df[df$g == lv, , drop = FALSE]
      m <- stats::lm(stats::as.formula(paste("y ~", .poly_terms(degree))), data = sub)
      cf <- stats::coef(m)
      tibble::tibble(
        group = lv,
        term = c("(Intercept)", paste0("t^", seq_len(degree))),
        estimate = unname(cf)
      )
    })
    return(dplyr::bind_rows(rows))
  }

  gf <- fit_res$models$group_fits
  if (is.null(gf)) {
    return(tibble::tibble(group = character(), term = character(), estimate = numeric()))
  }

  dplyr::bind_rows(lapply(names(gf), function(lv) {
    cf <- stats::coef(gf[[lv]])
    tibble::tibble(group = lv, term = names(cf), estimate = unname(cf))
  }))
}

# Main entry point -----------------------------------------------------------

#' Formal Test of Coincidence of Growth Curves Between Treatments
#'
#' Tests whether a single growth curve is enough to describe every treatment,
#' or whether each treatment needs its own curve, by comparing two nested
#' models with an F test and/or a likelihood ratio test.
#'
#' @param data A data frame with time, response and grouping columns. Raw data
#' is accepted; run [prepare_growth_data()] first to obtain the standard
#' column names.
#' @param response Response column, for example `"total_biomass_g"` or
#' `"leaf_area_cm2"`.
#' @param time_col Numeric time column.
#' @param group_var Grouping column holding the treatments to be compared.
#' @param model Functional form fitted to every treatment: `"poly"`
#' (polynomial in time, the default and most robust), `"exponential"`,
#' `"logistic"`, `"gompertz"` or `"richards"`.
#' @param degree Polynomial degree when `model = "poly"`. Using `degree = 2`
#' together with `log_response = TRUE` reproduces the classical log-quadratic
#' functional growth analysis.
#' @param log_response Logical; fit on the natural log of the response. On this
#' scale the first derivative of the curve is the relative growth rate.
#' @param epsilon Optional positive offset added before the log transform, for
#' data containing zeros.
#' @param test Which test(s) to report: `"both"` (default), `"F"` or `"LRT"`.
#' @param pairwise Logical; also run the coincidence test for every pair of
#' treatments.
#' @param p_adjust_method Multiplicity adjustment applied to the pairwise
#' p-values, passed to [stats::p.adjust()].
#' @param center_time Logical; center time at its mean before fitting the
#' polynomial. This improves the conditioning of the design matrix and changes
#' neither the fitted values nor any test statistic, only the meaning of the
#' intercept.
#'
#' @details
#' Let the response of observation \eqn{i} in treatment \eqn{g} follow a common
#' functional form with treatment-specific parameters,
#' \eqn{y_{ig} = f(t_{ig}; \theta_g) + e_{ig}}, with
#' \eqn{e_{ig} \sim N(0, \sigma^2)}. The reduced model imposes
#' \eqn{\theta_1 = \ldots = \theta_k} (coincident curves, \eqn{p_0 = p}
#' parameters); the full model leaves every \eqn{\theta_g} free
#' (\eqn{p_1 = kp} parameters). Writing \eqn{RSS} for the residual sum of
#' squares of each fit and \eqn{n} for the total number of observations:
#'
#' \deqn{F = \frac{(RSS_0 - RSS_1)/(p_1 - p_0)}{RSS_1/(n - p_1)}
#'   \sim F_{p_1 - p_0,\, n - p_1}}
#'
#' \deqn{G = -2(\ell_0 - \ell_1) = n \log(RSS_0/RSS_1) \sim \chi^2_{p_1 - p_0}}
#'
#' The F test is exact under normality for `model = "poly"` (a linear model)
#' and approximate for the non-linear families; the likelihood ratio test is
#' asymptotic in every case. This is the nested-model, or identity-of-models,
#' test of Chow (1960) and Graybill (1976).
#'
#' Beyond the global `"coincidence"` hypothesis the output decomposes the
#' difference between treatments:
#'
#' * `model = "poly"` adds `"level"` (equal intercepts given a common shape)
#'   and `"shape"` (equal time coefficients, i.e. parallel curves);
#' * the non-linear families add one `"parameter"` row per parameter, obtained
#'   by restricting that single parameter to be common while the others stay
#'   treatment-specific. This isolates whether treatments differ in final size
#'   (`Asym`), timing (`xmid`, `b`) or rate (`scal`, `c`, `k`, `r`).
#'
#' The full non-linear model is fitted independently within each treatment and
#' the residual sums of squares are pooled, which is algebraically equivalent
#' to the joint indexed model but numerically far more stable.
#'
#' Because a single residual variance is assumed for all treatments, inspect
#' the residuals before trusting a borderline p-value; strongly heteroscedastic
#' biomass data usually calls for `log_response = TRUE`.
#'
#' @return An object of class `phytogrow_curve_test`: a list whose main
#' elements are `tests` (tibble of hypotheses and statistics), `pairwise`
#' (tibble or `NULL`), `parameters` (per-treatment estimates), `models` and
#' fitting metadata. Use [tidy()], `summary()` or `plot()` on the result.
#'
#' @references
#' Chow, G. C. (1960). Tests of equality between sets of coefficients in two
#' linear regressions. *Econometrica*, 28(3), 591-605.
#' \doi{10.2307/1910133}
#'
#' Graybill, F. A. (1976). *Theory and Application of the Linear Model*.
#' Duxbury Press, North Scituate.
#'
#' Poorter, H., and Garnier, E. (1996). Plant growth analysis: an evaluation of
#' experimental design and computational methods. *Journal of Experimental
#' Botany*, 47(9), 1343-1351. \doi{10.1093/jxb/47.9.1343}
#'
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002). A modern
#' tool for classical plant growth analysis. *Annals of Botany*, 90(4),
#' 485-488. \doi{10.1093/aob/mcf214}
#'
#' @seealso [compare_growth()] for index-by-index pairwise contrasts and
#' [fit_growth_curve()] for the smoothing-based functional approach.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#'
#' # Classical log-quadratic functional growth analysis
#' ct <- compare_growth_curves(
#'   dat,
#'   response = "total_biomass_g",
#'   group_var = "treatment",
#'   model = "poly",
#'   degree = 2,
#'   log_response = TRUE,
#'   epsilon = 1e-6
#' )
#' ct
#' tidy(ct)
#' @export
compare_growth_curves <- function(
    data,
    response = "total_biomass_g",
    time_col = "time",
    group_var = "treatment",
    model = c("poly", "exponential", "logistic", "gompertz", "richards"),
    degree = 2,
    log_response = FALSE,
    epsilon = NULL,
    test = c("both", "F", "LRT"),
    pairwise = FALSE,
    p_adjust_method = "BH",
    center_time = TRUE
) {
  model <- match.arg(model)
  test <- match.arg(test)
  tests <- if (identical(test, "both")) c("F", "LRT") else test

  if (identical(model, "poly")) {
    .assert_positive_integer(degree, arg = "degree")
    if (degree > 5) {
      rlang::abort("`degree` above 5 is not supported; growth curves rarely need it.")
    }
  }

  df <- .curve_test_frame(data, response, time_col, group_var, log_response, epsilon)

  levs <- levels(df$g)
  if (length(levs) < 2) {
    rlang::abort(
      paste0("`", group_var, "` must have at least two levels to compare curves.")
    )
  }

  # Minimum sample size: each treatment needs more observations than the number
  # of parameters it spends, otherwise the full model has no residual df left.
  n_par_group <- if (identical(model, "poly")) {
    as.integer(degree) + 1L
  } else {
    length(.growth_model_params(model))
  }

  n_by_group <- table(df$g)
  short <- names(n_by_group)[n_by_group <= n_par_group]
  if (length(short) > 0) {
    rlang::abort(
      paste0(
        "Group(s) ", paste(short, collapse = ", "), " have too few observations for a ",
        n_par_group, "-parameter curve; at least ", n_par_group + 1L,
        " observations per group are required."
      )
    )
  }

  # Identifiability, which replication alone cannot buy: a curve with `q` free
  # time terms needs at least `q` DISTINCT times in every group. A two-harvest
  # design supports a straight line and nothing more, however many plants were
  # measured at each date.
  distinct_times <- tapply(df$x, df$g, function(z) length(unique(z)))
  min_times <- min(distinct_times, na.rm = TRUE)

  if (identical(model, "poly")) {
    max_degree <- as.integer(min_times) - 1L
    if (degree > max_degree) {
      rlang::abort(
        paste0(
          "A degree-", degree, " polynomial is not identifiable here: the smallest group has only ",
          min_times, " distinct time point(s), which supports at most degree ", max_degree,
          ". Use `degree = ", max(max_degree, 1L), "`, or add harvest dates."
        )
      )
    }
  } else if (min_times < n_par_group) {
    rlang::abort(
      paste0(
        "The `", model, "` model has ", n_par_group,
        " parameters but the smallest group has only ", min_times,
        " distinct time point(s). Use `model = 'poly'` with a low `degree`, or add harvest dates."
      )
    )
  }

  time_shift <- 0
  if (identical(model, "poly") && isTRUE(center_time)) {
    time_shift <- mean(df$x, na.rm = TRUE)
    df$x <- df$x - time_shift
  }

  df$g_idx <- as.integer(df$g)

  fit_res <- if (identical(model, "poly")) {
    .compare_poly(df, degree = degree, tests = tests)
  } else {
    .compare_nonlinear(df, model = model, tests = tests)
  }

  params <- .curve_test_parameters(fit_res, df, model, degree, levs)

  # Pairwise coincidence tests ------------------------------------------------
  pw <- NULL
  if (isTRUE(pairwise)) {
    pairs <- utils::combn(levs, 2, simplify = FALSE)

    pw <- dplyr::bind_rows(lapply(pairs, function(pr) {
      sub <- droplevels(df[df$g %in% pr, , drop = FALSE])
      sub$g_idx <- as.integer(sub$g)

      one <- tryCatch(
        if (identical(model, "poly")) {
          .compare_poly(sub, degree = degree, tests = tests)
        } else {
          .compare_nonlinear(sub, model = model, tests = tests)
        },
        error = function(e) NULL
      )

      if (is.null(one)) {
        return(tibble::tibble(
          group1 = pr[1], group2 = pr[2], test = NA_character_,
          df_num = NA_real_, df_den = NA_real_,
          statistic = NA_real_, p_value = NA_real_
        ))
      }

      one$tests %>%
        dplyr::filter(.data$hypothesis == "coincidence") %>%
        dplyr::transmute(
          group1 = pr[1],
          group2 = pr[2],
          test = .data$test,
          df_num = .data$df_num,
          df_den = .data$df_den,
          statistic = .data$statistic,
          p_value = .data$p_value
        )
    }))

    if (nrow(pw) > 0) {
      # Adjust within each test family, so F and LRT p-values are never mixed
      # in the same multiplicity correction.
      pw <- pw %>%
        dplyr::group_by(.data$test) %>%
        dplyr::mutate(p_adj = stats::p.adjust(.data$p_value, method = p_adjust_method)) %>%
        dplyr::ungroup()
    }
  }

  out <- list(
    call = match.call(),
    model = model,
    degree = if (identical(model, "poly")) as.integer(degree) else NA_integer_,
    response = response,
    time_col = time_col,
    group_var = group_var,
    groups = levs,
    n_obs = nrow(df),
    n_groups = length(levs),
    log_response = isTRUE(log_response),
    center_time = isTRUE(center_time) && identical(model, "poly"),
    time_shift = time_shift,
    test = test,
    p_adjust_method = p_adjust_method,
    tests = tibble::as_tibble(fit_res$tests),
    pairwise = pw,
    parameters = params,
    models = fit_res$models,
    n_par = fit_res$n_par,
    status = fit_res$status,
    message = fit_res$message,
    data = df
  )

  class(out) <- c("phytogrow_curve_test", "list")
  out
}
