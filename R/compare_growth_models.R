# Choosing the functional form ------------------------------------------------
#
# `compare_growth_curves()` tests treatments against each other but takes the
# functional form as given. Picking logistic over Gompertz over Richards by
# habit is a real source of arbitrariness in growth analysis, so this function
# fits the candidates to the same data and ranks them on the usual criteria.
#
# The comparison is only meaningful because every candidate is fitted to the
# SAME response on the SAME scale with Gaussian errors: AIC and BIC are then
# comparable across the polynomial (lm) and non-linear (nls) families alike.

.candidate_label <- function(model, degree) {
  if (identical(model, "poly")) paste0("poly", degree) else model
}

# Fits one candidate to one group and returns its fit statistics, or a row
# recording why it failed.
.fit_candidate <- function(df, model, degree) {
  label <- .candidate_label(model, degree)
  n <- nrow(df)

  fit <- if (identical(model, "poly")) {
    if (length(unique(df$x)) <= degree) {
      NULL
    } else {
      tryCatch(
        stats::lm(stats::as.formula(paste("y ~", .poly_terms(degree))), data = df),
        error = function(e) NULL
      )
    }
  } else {
    .fit_group_nls(df, model)
  }

  if (is.null(fit)) {
    return(tibble::tibble(
      model = label, converged = FALSE, n_par = NA_integer_, n_obs = n,
      rss = NA_real_, rmse = NA_real_, r_squared = NA_real_,
      aic = NA_real_, bic = NA_real_
    ))
  }

  rss <- .rss(fit)
  p <- if (identical(model, "poly")) .model_rank(fit) else length(stats::coef(fit))
  tss <- sum((df$y - mean(df$y, na.rm = TRUE))^2, na.rm = TRUE)

  tibble::tibble(
    model = label,
    converged = TRUE,
    n_par = as.integer(p),
    n_obs = n,
    rss = rss,
    rmse = sqrt(rss / n),
    # Not a true R^2 for a non-linear model; reported as the proportion of
    # variation the curve accounts for, which is how it is read in practice.
    r_squared = if (is.finite(tss) && tss > 0) 1 - rss / tss else NA_real_,
    aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  )
}

#' Compare Candidate Growth Models
#'
#' Fits several functional forms to the same data and ranks them, so the choice
#' between a logistic, a Gompertz, a Richards or a polynomial curve rests on
#' the data rather than on habit.
#'
#' @param data A data frame with time, response and (optionally) grouping
#' columns.
#' @param response Response column, for example `"total_biomass_g"`.
#' @param time_col Numeric time column.
#' @param group_var Optional grouping column. When supplied, every candidate is
#' fitted and ranked within each group; when `NULL`, one ranking is produced
#' for the pooled data.
#' @param models Non-linear candidates among `"exponential"`, `"logistic"`,
#' `"gompertz"` and `"richards"`.
#' @param poly_degrees Polynomial degrees to include as candidates. Use
#' `integer(0)` to drop them.
#' @param log_response Logical; fit every candidate to the natural log of the
#' response. Applied to all candidates alike, since criteria computed on
#' different response scales are not comparable.
#' @param epsilon Optional positive offset used before the log transform.
#' @param criterion Criterion used to flag the best candidate: `"aic"`
#' (default), `"bic"` or `"rmse"`.
#'
#' @details
#' Every candidate is fitted to the same observations, on the same scale, with
#' Gaussian errors, which is what makes AIC and BIC comparable across the `lm`
#' and `nls` families. Candidates that fail to converge are kept in the output
#' with `converged = FALSE` rather than dropped, because a family failing to
#' converge is itself informative.
#'
#' A word of caution on `r_squared`: for non-linear models it is not a genuine
#' coefficient of determination and cannot be used to compare models with
#' different numbers of parameters. Rank on `aic` or `bic`; read `r_squared`
#' only as a rough indication of fit.
#'
#' `delta` is the difference to the best candidate on the chosen criterion.
#' A difference below about 2 is usually taken as "no meaningful distinction",
#' in which case the simpler or more interpretable family is the better choice.
#'
#' @return A tibble with one row per group and candidate, ordered by the chosen
#' criterion, with columns `model`, `converged`, `n_par`, `n_obs`, `rss`,
#' `rmse`, `r_squared`, `aic`, `bic`, `delta` and `best`.
#'
#' @seealso [compare_growth_curves()] to test treatments once a form is chosen,
#' [fit_growth_curve()] and [growth_curve_params()].
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' compare_growth_models(
#'   dat,
#'   response = "total_biomass_g",
#'   group_var = "treatment",
#'   models = c("logistic", "gompertz")
#' )
#' @export
compare_growth_models <- function(
    data,
    response = "total_biomass_g",
    time_col = "time",
    group_var = NULL,
    models = c("exponential", "logistic", "gompertz", "richards"),
    poly_degrees = 1:3,
    log_response = FALSE,
    epsilon = NULL,
    criterion = c("aic", "bic", "rmse")
) {
  criterion <- match.arg(criterion)

  models <- if (length(models) == 0) {
    character(0)
  } else {
    match.arg(models, c("exponential", "logistic", "gompertz", "richards"), several.ok = TRUE)
  }

  poly_degrees <- as.integer(poly_degrees)
  if (length(poly_degrees) > 0) {
    if (any(is.na(poly_degrees)) || any(poly_degrees < 1) || any(poly_degrees > 5)) {
      rlang::abort("`poly_degrees` must be whole numbers between 1 and 5.")
    }
  }

  if (length(models) == 0 && length(poly_degrees) == 0) {
    rlang::abort("At least one candidate is required in `models` or `poly_degrees`.")
  }

  # A single grouping level keeps one code path for the grouped and pooled cases.
  gv <- group_var %||% ".all"
  if (is.null(group_var)) {
    data <- tibble::as_tibble(data)
    data[[gv]] <- "all"
  }

  df <- .curve_test_frame(data, response, time_col, gv, log_response, epsilon)

  candidates <- c(
    as.list(models),
    lapply(poly_degrees, function(d) list(model = "poly", degree = d))
  )

  rows <- lapply(levels(df$g), function(lv) {
    sub <- df[df$g == lv, , drop = FALSE]

    per_model <- lapply(candidates, function(cand) {
      if (is.list(cand)) {
        .fit_candidate(sub, cand$model, cand$degree)
      } else {
        .fit_candidate(sub, cand, NA_integer_)
      }
    })

    out <- dplyr::bind_rows(per_model)
    out[[gv]] <- lv
    out
  })

  out <- dplyr::bind_rows(rows)

  crit <- out[[criterion]]
  out$delta <- NA_real_

  # delta and best are computed within each group.
  for (lv in unique(out[[gv]])) {
    sel <- out[[gv]] == lv & out$converged & is.finite(crit)
    if (any(sel)) {
      out$delta[sel] <- crit[sel] - min(crit[sel], na.rm = TRUE)
    }
  }

  out$best <- !is.na(out$delta) & out$delta == 0

  out <- out %>%
    dplyr::select(dplyr::all_of(c(
      gv, "model", "converged", "n_par", "n_obs",
      "rss", "rmse", "r_squared", "aic", "bic", "delta", "best"
    ))) %>%
    dplyr::arrange(.data[[gv]], dplyr::desc(.data$converged), .data$delta)

  if (is.null(group_var)) {
    out[[gv]] <- NULL
  }

  attr(out, "criterion") <- criterion
  attr(out, "log_response") <- isTRUE(log_response)
  out
}
