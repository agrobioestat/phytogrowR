# Thermal time -----------------------------------------------------------------
#
# Growth analysis on calendar days silently assumes that a day in a cool week
# and a day in a warm week are worth the same to the plant. They are not, which
# is why agronomic growth analysis is so often done on accumulated degree-days.
# Expressing `time` in thermal units makes trials run in different seasons or
# sites comparable, and usually straightens the growth curve considerably.

#' Accumulated Thermal Time (Growing Degree Days)
#'
#' Converts a daily temperature series into daily and accumulated thermal time,
#' so that growth analysis can be run against degree-days instead of calendar
#' days.
#'
#' @param data A data frame with one row per day.
#' @param date Name of the date column (a `Date`, or anything coercible).
#' @param tmin Name of the daily minimum temperature column. Optional when
#' `tmean` is supplied.
#' @param tmax Name of the daily maximum temperature column. Optional when
#' `tmean` is supplied.
#' @param tmean Name of a daily mean temperature column. When `NULL`, the mean
#' is computed as `(tmin + tmax) / 2`.
#' @param t_base Base temperature below which development is taken to stop, in
#' the same unit as the temperatures.
#' @param t_upper Optional upper threshold. Ignored when `method = "simple"`.
#' @param method Accumulation method: `"simple"`, `"cutoff"` or `"modified"`.
#' See Details.
#' @param group_var Optional grouping column (site, season, sowing date, ...).
#' Accumulation restarts within each group.
#' @param origin Optional date at which accumulation starts (sowing or
#' emergence). Days before it contribute zero. When `NULL`, accumulation starts
#' at the first date available in each group.
#'
#' @details
#' Writing \eqn{T_{mean}} for the daily mean temperature, the three methods are:
#'
#' \describe{
#'   \item{`"simple"`}{\eqn{GDD = \max(0,\; T_{mean} - T_{base})}. The upper
#'     threshold is ignored. This is the standard textbook formulation and the
#'     right default when temperatures stay well below the upper limit of the
#'     species.}
#'   \item{`"cutoff"`}{`tmax` is capped at `t_upper` before the mean is taken,
#'     so hot afternoons stop contributing. Use it where daytime temperatures
#'     exceed the optimum for the crop.}
#'   \item{`"modified"`}{`tmin` is additionally raised to `t_base` before the
#'     mean is taken, in addition to the `"cutoff"` cap. This is the form used
#'     in much of the maize and wheat literature.}
#' }
#'
#' All three are approximations of an integral over the daily temperature
#' course, computed from two readings a day. They diverge whenever a day
#' crosses `t_base` or `t_upper`, so the method should be reported alongside
#' any thermal time that is published.
#'
#' To use the result, join it onto the growth data by date and map the
#' accumulated column as `time`:
#'
#' ```
#' tt <- thermal_time(weather, date = date, tmin = tmin, tmax = tmax, t_base = 10)
#' growth <- merge(growth, tt[, c("date", "thermal_time")], by = "date")
#' dat <- prepare_growth_data(growth, mapping = list(time = "thermal_time"))
#' ```
#'
#' @return The input data with three columns added: `t_mean_effective` (the
#' daily mean actually used, after any capping), `gdd` (the daily contribution)
#' and `thermal_time` (the running total).
#'
#' @seealso [prepare_growth_data()] for mapping the result onto `time`.
#'
#' @examples
#' weather <- data.frame(
#'   date = as.Date("2024-01-01") + 0:29,
#'   tmin = 12 + 5 * sin(seq(0, 3, length.out = 30)),
#'   tmax = 26 + 5 * sin(seq(0, 3, length.out = 30))
#' )
#'
#' tt <- thermal_time(weather, date = date, tmin = tmin, tmax = tmax, t_base = 10)
#' head(tt)
#'
#' # Capping hot days at 30 degrees accumulates less thermal time
#' tt2 <- thermal_time(
#'   weather, date = date, tmin = tmin, tmax = tmax,
#'   t_base = 10, t_upper = 30, method = "cutoff"
#' )
#' tail(tt2$thermal_time, 1) < tail(tt$thermal_time, 1)
#' @export
thermal_time <- function(
    data,
    date = "date",
    tmin = "tmin",
    tmax = "tmax",
    tmean = NULL,
    t_base = 10,
    t_upper = NULL,
    method = c("simple", "cutoff", "modified"),
    group_var = NULL,
    origin = NULL
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  # Column names may be given bare or as strings. The quosures must be captured
  # here, in this frame: capturing inside a helper would enquote the helper's
  # own argument and yield the literal name "x" instead of the user's column.
  resolve_col <- function(q) {
    if (rlang::quo_is_null(q)) return(NULL)
    if (rlang::quo_is_symbol(q)) rlang::as_name(q) else rlang::eval_tidy(q)
  }

  date_col <- resolve_col(rlang::enquo(date))
  tmin_col <- resolve_col(rlang::enquo(tmin))
  tmax_col <- resolve_col(rlang::enquo(tmax))
  tmean_col <- resolve_col(rlang::enquo(tmean))
  group_col <- resolve_col(rlang::enquo(group_var))

  if (!date_col %in% names(data)) {
    rlang::abort(paste0("Date column `", date_col, "` was not found in `data`."))
  }

  if (!is.numeric(t_base) || length(t_base) != 1L || is.na(t_base)) {
    rlang::abort("`t_base` must be a single number.")
  }

  # An explicit `tmean` that is absent is a mistake, not a reason to quietly
  # fall back to tmin/tmax.
  if (!is.null(tmean_col) && !tmean_col %in% names(data)) {
    rlang::abort(paste0("Mean temperature column `", tmean_col, "` was not found in `data`."))
  }

  use_mean <- !is.null(tmean_col)

  if (!use_mean) {
    missing <- setdiff(c(tmin_col, tmax_col), names(data))
    if (length(missing) > 0) {
      rlang::abort(
        paste0(
          "Supply either `tmean`, or both `tmin` and `tmax`. Missing: ",
          paste(missing, collapse = ", "), "."
        )
      )
    }
  }

  if (!identical(method, "simple")) {
    if (is.null(t_upper)) {
      rlang::abort(paste0("`method = \"", method, "\"` requires `t_upper`."))
    }
    if (!is.numeric(t_upper) || length(t_upper) != 1L || is.na(t_upper) || t_upper <= t_base) {
      rlang::abort("`t_upper` must be a single number greater than `t_base`.")
    }
    if (use_mean) {
      rlang::abort(
        paste0(
          "`method = \"", method, "\"` caps the daily maximum, so it needs `tmin` and `tmax` ",
          "rather than a pre-computed `tmean`."
        )
      )
    }
  }

  out <- tibble::as_tibble(data)

  d <- out[[date_col]]
  if (!inherits(d, "Date")) {
    d <- suppressWarnings(as.Date(d))
  }
  if (all(is.na(d))) {
    rlang::abort(paste0("Column `", date_col, "` could not be interpreted as dates."))
  }

  t_eff <- if (use_mean) {
    as.numeric(out[[tmean_col]])
  } else {
    lo <- as.numeric(out[[tmin_col]])
    hi <- as.numeric(out[[tmax_col]])

    if (identical(method, "cutoff") || identical(method, "modified")) {
      hi <- pmin(hi, t_upper)
    }
    if (identical(method, "modified")) {
      lo <- pmax(lo, t_base)
      lo <- pmin(lo, t_upper)
    }

    (lo + hi) / 2
  }

  gdd <- pmax(0, t_eff - t_base)

  # A day with a missing temperature contributes nothing rather than breaking
  # the running total; the gap stays visible in `gdd`.
  gdd_filled <- ifelse(is.na(gdd), 0, gdd)

  grp <- if (!is.null(group_col)) {
    if (!group_col %in% names(out)) {
      rlang::abort(paste0("Grouping column `", group_col, "` was not found in `data`."))
    }
    as.character(out[[group_col]])
  } else {
    rep("all", nrow(out))
  }

  if (!is.null(origin)) {
    org <- if (inherits(origin, "Date")) origin else suppressWarnings(as.Date(origin))
    if (is.na(org)) {
      rlang::abort("`origin` could not be interpreted as a date.")
    }
    gdd_filled[!is.na(d) & d < org] <- 0
  }

  cum <- rep(NA_real_, nrow(out))
  for (lv in unique(grp)) {
    sel <- which(grp == lv)
    sel <- sel[order(d[sel])]
    cum[sel] <- cumsum(gdd_filled[sel])
  }

  out$t_mean_effective <- t_eff
  out$gdd <- gdd
  out$thermal_time <- cum

  attr(out, "thermal_method") <- method
  attr(out, "t_base") <- t_base
  attr(out, "t_upper") <- t_upper
  out
}
