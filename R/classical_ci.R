#' Classical Confidence Intervals for Interval Growth Indices
#'
#' Compute standard confidence limits from an estimate, standard error,
#' and degrees of freedom using the Student t distribution.
#'
#' @param estimate Numeric estimate.
#' @param standard_error Numeric standard error.
#' @param degrees_freedom Degrees of freedom.
#' @param conf_level Confidence level between 0 and 1.
#'
#' @return A tibble with `estimate`, `standard_error`, `conf_low`,
#' `conf_high`, and `degrees_freedom`.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' classical_ci(estimate = 0.05, standard_error = 0.01, degrees_freedom = 8)
#'
#' @section Interpretation:
#' The interval indicates uncertainty around interval-average estimates
#' (not instantaneous rates).
#'
#' @section Limitations:
#' The approach assumes approximately normal sampling distributions.
#'
#' @export
classical_ci <- function(
    estimate,
    standard_error,
    degrees_freedom,
    conf_level = 0.95
) {
  .assert_conf_level(conf_level)

  if (!is.numeric(estimate) || length(estimate) != 1L) {
    rlang::abort("`estimate` must be a single numeric value.")
  }
  if (!is.numeric(standard_error) || length(standard_error) != 1L) {
    rlang::abort("`standard_error` must be a single numeric value.")
  }
  if (!is.numeric(degrees_freedom) || length(degrees_freedom) != 1L) {
    rlang::abort("`degrees_freedom` must be a single numeric value.")
  }

  if (!is.finite(estimate) || !is.finite(standard_error) ||
      standard_error < 0 || !is.finite(degrees_freedom) || degrees_freedom <= 0) {
    return(
      tibble::tibble(
        estimate = estimate,
        standard_error = standard_error,
        conf_low = NA_real_,
        conf_high = NA_real_,
        degrees_freedom = degrees_freedom
      )
    )
  }

  alpha <- (1 - conf_level) / 2
  t_crit <- stats::qt(1 - alpha, df = degrees_freedom)

  tibble::tibble(
    estimate = estimate,
    standard_error = standard_error,
    conf_low = estimate - t_crit * standard_error,
    conf_high = estimate + t_crit * standard_error,
    degrees_freedom = degrees_freedom
  )
}
