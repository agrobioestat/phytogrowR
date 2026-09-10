#' Calculate Relative Growth Rate by Hunt's Interval Mean-Log Approach
#'
#' Compute classical interval RGR using the difference between the
#' mean logarithms of biomass at two harvests.
#'
#' @param w1 Biomass values at harvest 1 (must be positive).
#' @param w2 Biomass values at harvest 2 (must be positive).
#' @param delta_t Time interval (`t2 - t1`) in days or other consistent unit.
#'
#' @return Numeric scalar with interval-average RGR.
#'
#' @details
#' Formula:
#'
#' \deqn{
#' RGR = \frac{\overline{\log(W_2)} - \overline{\log(W_1)}}{t_2 - t_1}
#' }
#'
#' This uses `mean(log(W))`, not `log(mean(W))`, to avoid bias.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_rgr_hunt(w1 = c(1.2, 1.4, 1.5), w2 = c(2.0, 2.2, 2.3), delta_t = 10)
#'
#' @section Interpretation:
#' RGR is an interval mean and should not be interpreted as an
#' instantaneous derivative.
#'
#' @section Limitations:
#' Requires strictly positive biomass in both harvests.
#'
#' @export
calc_rgr_hunt <- function(w1, w2, delta_t) {
  if (!is.numeric(delta_t) || length(delta_t) != 1L || is.na(delta_t) || delta_t <= 0) {
    rlang::abort("`delta_t` must be a single positive numeric value.")
  }

  w1 <- as.numeric(w1)
  w2 <- as.numeric(w2)
  w1 <- w1[!is.na(w1)]
  w2 <- w2[!is.na(w2)]

  if (length(w1) == 0L || length(w2) == 0L) {
    return(NA_real_)
  }

  if (any(w1 <= 0) || any(w2 <= 0)) {
    rlang::abort("`w1` and `w2` must contain only positive values.")
  }

  (mean(log(w2)) - mean(log(w1))) / delta_t
}
