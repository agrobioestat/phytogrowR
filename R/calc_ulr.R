#' Calculate Unit Leaf Rate (ULR) for Two Harvests
#'
#' Compute classical ULR from biomass and leaf area between two harvests.
#'
#' @param w1 Biomass values at harvest 1.
#' @param w2 Biomass values at harvest 2.
#' @param la1 Leaf area values at harvest 1.
#' @param la2 Leaf area values at harvest 2.
#' @param delta_t Time interval (`t2 - t1`) in consistent time units.
#'
#' @return Numeric scalar with interval-average ULR.
#'
#' @details
#' Formula:
#'
#' \deqn{
#' ULR = \left(\frac{\overline{W_2} - \overline{W_1}}{t_2 - t_1}\right)
#' \left(\frac{\log(\overline{LA_2}) - \log(\overline{LA_1})}{\overline{LA_2} - \overline{LA_1}}\right)
#' }
#'
#' In many classical analyses, ULR and NAR are operationally equivalent.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_ulr(
#'   w1 = c(1.2, 1.4, 1.5),
#'   w2 = c(2.0, 2.2, 2.4),
#'   la1 = c(40, 42, 39),
#'   la2 = c(62, 65, 63),
#'   delta_t = 10
#' )
#'
#' @section Interpretation:
#' ULR is an interval-average quantity and should not be interpreted
#' as an instantaneous rate.
#'
#' @section Limitations:
#' Requires positive leaf area means and non-zero change in mean leaf area.
#'
#' @export
calc_ulr <- function(w1, w2, la1, la2, delta_t) {
  if (!is.numeric(delta_t) || length(delta_t) != 1L || is.na(delta_t) || delta_t <= 0) {
    rlang::abort("`delta_t` must be a single positive numeric value.")
  }

  w1 <- as.numeric(w1)
  w2 <- as.numeric(w2)
  la1 <- as.numeric(la1)
  la2 <- as.numeric(la2)

  w1 <- w1[!is.na(w1)]
  w2 <- w2[!is.na(w2)]
  la1 <- la1[!is.na(la1)]
  la2 <- la2[!is.na(la2)]

  if (length(w1) == 0L || length(w2) == 0L || length(la1) == 0L || length(la2) == 0L) {
    return(NA_real_)
  }

  m_w1 <- mean(w1)
  m_w2 <- mean(w2)
  m_la1 <- mean(la1)
  m_la2 <- mean(la2)

  if (m_la1 <= 0 || m_la2 <= 0) {
    rlang::abort("Mean leaf area must be positive for ULR/NAR calculations.")
  }

  if (abs(m_la2 - m_la1) < 1e-12) {
    return(NA_real_)
  }

  ((m_w2 - m_w1) / delta_t) * ((log(m_la2) - log(m_la1)) / (m_la2 - m_la1))
}

#' Calculate Net Assimilation Rate (NAR)
#'
#' Alias of [calc_ulr()] for classical two-harvest workflows.
#'
#' @inheritParams calc_ulr
#'
#' @return Numeric scalar with interval-average NAR.
#'
#' @details
#' In this classical module, NAR is treated as operationally equivalent
#' to ULR.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_nar(
#'   w1 = c(1.2, 1.4, 1.5),
#'   w2 = c(2.0, 2.2, 2.4),
#'   la1 = c(40, 42, 39),
#'   la2 = c(62, 65, 63),
#'   delta_t = 10
#' )
#'
#' @section Interpretation:
#' NAR and ULR are reported as interval means in this implementation.
#'
#' @section Limitations:
#' Same assumptions as [calc_ulr()].
#'
#' @export
calc_nar <- function(w1, w2, la1, la2, delta_t) {
  calc_ulr(w1 = w1, w2 = w2, la1 = la1, la2 = la2, delta_t = delta_t)
}
