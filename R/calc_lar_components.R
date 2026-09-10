#' Decompose LAR Into SLA and LWF Components
#'
#' Calculate interval-average LAR, SLA, LWF (and LWR alias), and report
#' the numerical difference between `LAR` and `SLA * LWF`.
#'
#' @param leaf_area Numeric leaf area vector.
#' @param leaf_biomass Numeric leaf biomass vector.
#' @param total_biomass Numeric total biomass vector.
#' @param na_rm Logical; remove missing values.
#' @param tolerance Absolute tolerance used to flag decomposition mismatch.
#'
#' @return A one-row tibble with:
#' `lar`, `sla`, `lwf`, `lwr`, `lar_minus_sla_lwf`, and `warning`.
#'
#' @details
#' Formulas:
#'
#' - `LAR = leaf_area / total_biomass`
#' - `SLA = leaf_area / leaf_biomass`
#' - `LWF = leaf_biomass / total_biomass`
#' - `LWR = LWF`
#'
#' and decomposition check:
#'
#' - `LAR - (SLA * LWF)`
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_lar_components(
#'   leaf_area = c(40, 44, 46),
#'   leaf_biomass = c(1.2, 1.3, 1.4),
#'   total_biomass = c(3.0, 3.2, 3.4)
#' )
#'
#' @section Interpretation:
#' Differences between `LAR` and `SLA * LWF` can arise from averaging scale,
#' missingness, or aggregation.
#'
#' @section Limitations:
#' Requires positive denominators for stable component ratios.
#'
#' @export
calc_lar_components <- function(
    leaf_area,
    leaf_biomass,
    total_biomass,
    na_rm = TRUE,
    tolerance = 0.05
) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0) {
    rlang::abort("`tolerance` must be a single non-negative numeric value.")
  }

  leaf_area <- as.numeric(leaf_area)
  leaf_biomass <- as.numeric(leaf_biomass)
  total_biomass <- as.numeric(total_biomass)

  lar_vec <- .safe_div(leaf_area, total_biomass)
  sla_vec <- .safe_div(leaf_area, leaf_biomass)
  lwf_vec <- calc_lwf(leaf_biomass = leaf_biomass, total_biomass = total_biomass)

  lar <- mean(lar_vec, na.rm = na_rm)
  sla <- mean(sla_vec, na.rm = na_rm)
  lwf <- mean(lwf_vec, na.rm = na_rm)
  lwr <- lwf

  if (!is.finite(lar)) lar <- NA_real_
  if (!is.finite(sla)) sla <- NA_real_
  if (!is.finite(lwf)) lwf <- NA_real_

  diff <- lar - (sla * lwf)
  if (!is.finite(diff)) diff <- NA_real_

  warning_text <- NA_character_
  if (is.finite(diff) && abs(diff) > tolerance) {
    warning_text <- paste0(
      "Large |LAR - SLA*LWF| detected (", signif(diff, 5),
      "). This may result from averaging scale, missingness, or aggregation."
    )
  }

  tibble::tibble(
    lar = lar,
    sla = sla,
    lwf = lwf,
    lwr = lwr,
    lar_minus_sla_lwf = diff,
    warning = warning_text
  )
}
