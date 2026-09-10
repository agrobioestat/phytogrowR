#' Calculate Leaf Weight Fraction (LWF)
#'
#' Compute the leaf biomass proportion in total biomass.
#'
#' @param leaf_biomass Numeric vector of leaf biomass.
#' @param total_biomass Numeric vector of total biomass.
#'
#' @return Numeric vector with `leaf_biomass / total_biomass`.
#'
#' @details
#' \deqn{LWF = \frac{leaf\_biomass}{total\_biomass}}
#'
#' In this classical module, LWF and LWR are treated as synonyms.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_lwf(leaf_biomass = c(1.0, 1.4), total_biomass = c(2.5, 3.2))
#'
#' @section Interpretation:
#' LWF/LWR quantify biomass allocation to leaves.
#'
#' @section Limitations:
#' Division by zero returns `NA`.
#'
#' @export
calc_lwf <- function(leaf_biomass, total_biomass) {
  .safe_div(as.numeric(leaf_biomass), as.numeric(total_biomass))
}

#' Calculate Leaf Weight Ratio (LWR)
#'
#' Alias of [calc_lwf()] in this classical module.
#'
#' @inheritParams calc_lwf
#'
#' @return Numeric vector with `leaf_biomass / total_biomass`.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_lwr(leaf_biomass = c(1.0, 1.4), total_biomass = c(2.5, 3.2))
#'
#' @section Interpretation:
#' LWR is treated as equivalent to LWF in this context.
#'
#' @section Limitations:
#' Same assumptions as [calc_lwf()].
#'
#' @export
calc_lwr <- function(leaf_biomass, total_biomass) {
  calc_lwf(leaf_biomass = leaf_biomass, total_biomass = total_biomass)
}
