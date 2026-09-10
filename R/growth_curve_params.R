# Biologically interpretable landmarks of a fitted growth curve ---------------
#
# `fit_growth_curve()` produces a trajectory and `calc_instant_rates()` gives
# rates on a grid, but neither answers the questions an experiment is usually
# designed around: when did growth peak, how fast was it then, how long did the
# active phase last, and did the crop plateau at all within the observed window.
#
# Everything below is read off the prediction grid rather than from closed-form
# parameter expressions. That keeps one implementation for every method the
# package can fit - GAM, spline, LOESS, logistic, Gompertz, Richards - at the
# cost of a resolution that depends on `n_grid`, which is reported so the user
# can judge it.

# First time the curve reaches `level`, with linear interpolation between the
# two bracketing grid points. Returns NA when the level is never reached.
.first_crossing <- function(time, value, level) {
  ok <- is.finite(time) & is.finite(value)
  time <- time[ok]
  value <- value[ok]

  if (length(time) < 2 || !any(value >= level)) {
    return(NA_real_)
  }

  idx <- which(value >= level)[1]

  if (idx == 1L) {
    return(time[1])
  }

  y0 <- value[idx - 1L]
  y1 <- value[idx]

  if (!is.finite(y1 - y0) || y1 == y0) {
    return(time[idx])
  }

  time[idx - 1L] + (level - y0) * (time[idx] - time[idx - 1L]) / (y1 - y0)
}

.curve_landmarks <- function(pred, asym_param = NA_real_, plateau_tol = 0.05) {
  pred <- pred[is.finite(pred$time) & is.finite(pred$fitted), , drop = FALSE]
  pred <- pred[order(pred$time), , drop = FALSE]

  if (nrow(pred) < 3) {
    return(tibble::tibble(
      asymptote = NA_real_, asymptote_source = NA_character_,
      t_inflection = NA_real_, y_inflection = NA_real_,
      max_agr = NA_real_, max_rgr = NA_real_, t_max_rgr = NA_real_,
      t10 = NA_real_, t50 = NA_real_, t90 = NA_real_,
      active_phase = NA_real_,
      final_time = NA_real_, final_value = NA_real_,
      plateau_reached = NA, n_grid = nrow(pred)
    ))
  }

  agr <- pred$deriv
  has_agr <- any(is.finite(agr))

  # The asymptote of a parametric fit is a parameter; for a smoother there is
  # none, so the highest fitted value stands in and the source is recorded.
  # Calling the last observed value an "asymptote" without that caveat would
  # invent a plateau the experiment may never have reached.
  if (is.finite(asym_param) && asym_param > 0) {
    asym <- asym_param
    asym_src <- "model parameter"
  } else {
    asym <- max(pred$fitted, na.rm = TRUE)
    asym_src <- "maximum fitted value"
  }

  if (has_agr) {
    i_max <- which.max(agr)
    t_inf <- pred$time[i_max]
    y_inf <- pred$fitted[i_max]
    max_agr <- agr[i_max]

    # RGR = (dW/dt) / W, undefined where the fitted curve is not positive.
    rgr <- ifelse(pred$fitted > 0, agr / pred$fitted, NA_real_)
    i_rgr <- if (any(is.finite(rgr))) which.max(rgr) else NA_integer_
    max_rgr <- if (is.na(i_rgr)) NA_real_ else rgr[i_rgr]
    t_max_rgr <- if (is.na(i_rgr)) NA_real_ else pred$time[i_rgr]

    # A plateau means the curve has stopped growing relative to its own peak
    # speed, not merely that the last harvest happened.
    last_agr <- utils::tail(agr[is.finite(agr)], 1)
    plateau <- is.finite(max_agr) && max_agr > 0 &&
      is.finite(last_agr) && (last_agr / max_agr) < plateau_tol
  } else {
    t_inf <- NA_real_; y_inf <- NA_real_; max_agr <- NA_real_
    max_rgr <- NA_real_; t_max_rgr <- NA_real_; plateau <- NA
  }

  t10 <- .first_crossing(pred$time, pred$fitted, 0.10 * asym)
  t50 <- .first_crossing(pred$time, pred$fitted, 0.50 * asym)
  t90 <- .first_crossing(pred$time, pred$fitted, 0.90 * asym)

  tibble::tibble(
    asymptote = asym,
    asymptote_source = asym_src,
    t_inflection = t_inf,
    y_inflection = y_inf,
    max_agr = max_agr,
    max_rgr = max_rgr,
    t_max_rgr = t_max_rgr,
    t10 = t10,
    t50 = t50,
    t90 = t90,
    active_phase = t90 - t10,
    final_time = utils::tail(pred$time, 1),
    final_value = utils::tail(pred$fitted, 1),
    plateau_reached = plateau,
    n_grid = nrow(pred)
  )
}

#' Biologically Interpretable Parameters of a Fitted Growth Curve
#'
#' Extracts the landmarks an experiment is usually designed to estimate -
#' when growth peaked, how fast it was then, how long the active phase lasted,
#' and whether the curve plateaued at all - from an object returned by
#' [fit_growth_curve()].
#'
#' @param object An object of class `phytogrow_fit`.
#' @param plateau_tol Fraction of the maximum absolute growth rate below which
#' the curve is considered to have plateaued at the last observed time.
#'
#' @details
#' All quantities are read off the prediction grid, so one implementation
#' serves every method the package can fit. The reported `n_grid` is the
#' resolution behind them: a coarse grid rounds `t_inflection` to the nearest
#' grid step, so increase `n_grid` in [fit_growth_curve()] when the timing
#' itself is the result of interest.
#'
#' The returned columns are:
#'
#' \describe{
#'   \item{`asymptote`, `asymptote_source`}{Final size. For `logistic`,
#'     `gompertz` and `richards` fits this is the model's `Asym` parameter; for
#'     a smoother there is no asymptote parameter and the maximum fitted value
#'     stands in, which `asymptote_source` records. Treat the second case as a
#'     lower bound, not as an estimated plateau.}
#'   \item{`t_inflection`, `y_inflection`, `max_agr`}{Time of maximum absolute
#'     growth rate, the size reached there, and that maximum rate. For a
#'     sigmoid curve this is the inflection point.}
#'   \item{`max_rgr`, `t_max_rgr`}{Maximum relative growth rate and when it
#'     occurs; typically early, well before `t_inflection`.}
#'   \item{`t10`, `t50`, `t90`}{First times at which 10\%, 50\% and 90\% of the
#'     asymptote are reached, linearly interpolated between grid points.}
#'   \item{`active_phase`}{`t90 - t10`, the duration of the main growth period.}
#'   \item{`plateau_reached`}{`TRUE` when the growth rate at the last time has
#'     fallen below `plateau_tol` of its maximum. When `FALSE`, the curve was
#'     still rising when the experiment ended and `asymptote` is not an
#'     estimate of final size.}
#' }
#'
#' @return A tibble with one row per group and the landmark columns described
#' above, preceded by the grouping columns of the fit.
#'
#' @seealso [fit_growth_curve()], [calc_instant_rates()],
#' [compare_growth_models()].
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' fit <- fit_growth_curve(
#'   dat,
#'   response = "total_biomass_g",
#'   group_cols = "treatment",
#'   method = "logistic"
#' )
#' growth_curve_params(fit)
#' @export
growth_curve_params <- function(object, plateau_tol = 0.05) {
  if (!inherits(object, "phytogrow_fit")) {
    rlang::abort("`object` must be created by `fit_growth_curve()`.")
  }

  if (!is.numeric(plateau_tol) || length(plateau_tol) != 1L ||
      is.na(plateau_tol) || plateau_tol <= 0 || plateau_tol >= 1) {
    rlang::abort("`plateau_tol` must be a single number between 0 and 1.")
  }

  gcols <- object$group_cols
  pred <- object$predictions

  if (is.null(pred) || nrow(pred) == 0) {
    return(tibble::tibble())
  }

  keys <- if (length(gcols) > 0) {
    dplyr::distinct(dplyr::select(pred, dplyr::all_of(gcols)))
  } else {
    tibble::tibble(.all = "all")
  }

  # The asymptote parameter, when the fitted family has one.
  asym_of <- function(i) {
    mod <- object$models[[i]]
    if (is.null(mod)) return(NA_real_)
    cf <- tryCatch(stats::coef(mod), error = function(e) NULL)
    if (is.null(cf) || !"Asym" %in% names(cf)) return(NA_real_)
    unname(cf[["Asym"]])
  }

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    if (length(gcols) > 0) {
      key <- keys[i, , drop = FALSE]
      sel <- rep(TRUE, nrow(pred))
      for (nm in gcols) {
        sel <- sel & (as.character(pred[[nm]]) == as.character(key[[nm]]))
      }
      sub <- pred[sel, , drop = FALSE]
    } else {
      key <- tibble::tibble()
      sub <- pred
    }

    out <- .curve_landmarks(sub, asym_param = asym_of(i), plateau_tol = plateau_tol)

    if (nrow(key) > 0) dplyr::bind_cols(key, out) else out
  })

  out <- dplyr::bind_rows(rows)

  out$response <- object$response
  out$method <- object$method
  out
}
