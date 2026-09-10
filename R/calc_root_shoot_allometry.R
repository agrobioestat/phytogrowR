#' Calculate Root:Shoot Ratio
#'
#' Compute root-to-shoot biomass ratio using shoot biomass as
#' `leaf + stem (+ reproductive, if available)`.
#'
#' @param root_biomass Root biomass vector.
#' @param leaf_biomass Leaf biomass vector.
#' @param stem_biomass Stem biomass vector.
#' @param reproductive_biomass Optional reproductive biomass vector.
#'
#' @return Numeric vector with root:shoot ratio.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_root_shoot_ratio(
#'   root_biomass = c(0.8, 1.0),
#'   leaf_biomass = c(1.1, 1.3),
#'   stem_biomass = c(0.9, 1.0)
#' )
#'
#' @section Interpretation:
#' Values above 1 indicate greater biomass allocation to roots than shoots.
#'
#' @section Limitations:
#' Division by zero yields `NA`.
#'
#' @export
calc_root_shoot_ratio <- function(
    root_biomass,
    leaf_biomass,
    stem_biomass,
    reproductive_biomass = NULL
) {
  shoot <- as.numeric(leaf_biomass) + as.numeric(stem_biomass)
  if (!is.null(reproductive_biomass)) {
    shoot <- shoot + as.numeric(reproductive_biomass)
  }
  .safe_div(as.numeric(root_biomass), shoot)
}

#' Calculate Shoot:Root Ratio
#'
#' Compute shoot-to-root biomass ratio.
#'
#' @inheritParams calc_root_shoot_ratio
#'
#' @return Numeric vector with shoot:root ratio.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_shoot_root_ratio(
#'   root_biomass = c(0.8, 1.0),
#'   leaf_biomass = c(1.1, 1.3),
#'   stem_biomass = c(0.9, 1.0)
#' )
#'
#' @section Interpretation:
#' Higher values indicate proportionally greater allocation to shoot biomass.
#'
#' @section Limitations:
#' Division by zero yields `NA`.
#'
#' @export
calc_shoot_root_ratio <- function(
    root_biomass,
    leaf_biomass,
    stem_biomass,
    reproductive_biomass = NULL
) {
  shoot <- as.numeric(leaf_biomass) + as.numeric(stem_biomass)
  if (!is.null(reproductive_biomass)) {
    shoot <- shoot + as.numeric(reproductive_biomass)
  }
  .safe_div(shoot, as.numeric(root_biomass))
}

#' Estimate Root-Shoot Allometric Coefficient
#'
#' Fit:
#' \deqn{log(root\_biomass) = alpha + beta * log(shoot\_biomass)}
#'
#' The estimated `beta` is the root-shoot allometric coefficient.
#'
#' @param root_biomass Root biomass vector (positive values required).
#' @param shoot_biomass Shoot biomass vector (positive values required).
#' @param method Either `"lm"` or `"bivariate_ml"`.
#' @param conf_level Confidence level.
#'
#' @return A one-row tibble with:
#' `intercept`, `beta`, `standard_error`, `conf_low`, `conf_high`, `p_value`,
#' `r_squared`, `degrees_freedom`, `n`, and `method`.
#'
#' @details
#' Two methods are available:
#'
#' - `"lm"`: ordinary least squares on `log(root)` against `log(shoot)`.
#' - `"bivariate_ml"`: a bivariate approximation using reduced major axis (RMA,
#'   Model II) on log-transformed data, with jackknife-based standard error.
#'
#' The `"bivariate_ml"` implementation is an approximation intended to be
#' compatible with classical allometric workflows where both axes may contain
#' measurement error.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' calc_root_shoot_allometry(
#'   root_biomass = c(0.5, 0.7, 0.8, 1.1, 1.3, 1.6),
#'   shoot_biomass = c(1.2, 1.5, 1.6, 2.1, 2.4, 2.9),
#'   method = "lm"
#' )
#'
#' @section Interpretation:
#' `beta > 1` suggests relatively faster root accumulation as shoot biomass
#' increases; `beta < 1` suggests the opposite.
#'
#' @section Limitations:
#' At least five valid plants are required for stable estimation.
#'
#' @export
calc_root_shoot_allometry <- function(
    root_biomass,
    shoot_biomass,
    method = c("lm", "bivariate_ml"),
    conf_level = 0.95
) {
  method <- match.arg(method)
  .assert_conf_level(conf_level)

  dat <- tibble::tibble(
    root = as.numeric(root_biomass),
    shoot = as.numeric(shoot_biomass)
  ) %>%
    dplyr::filter(!is.na(.data$root), !is.na(.data$shoot), .data$root > 0, .data$shoot > 0)

  n <- nrow(dat)
  if (n < 5) {
    return(
      tibble::tibble(
        intercept = NA_real_,
        beta = NA_real_,
        standard_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_,
        degrees_freedom = NA_real_,
        n = n,
        method = method
      )
    )
  }

  df <- max(n - 4, 1)

  if (method == "lm") {
    fit <- stats::lm(log(root) ~ log(shoot), data = dat)
    sm <- summary(fit)

    coef_tbl <- sm$coefficients
    beta <- unname(coef_tbl["log(shoot)", "Estimate"])
    intercept <- unname(coef_tbl["(Intercept)", "Estimate"])
    se <- unname(coef_tbl["log(shoot)", "Std. Error"])
    p_val <- unname(coef_tbl["log(shoot)", "Pr(>|t|)"])
    r2 <- unname(sm$r.squared)

    ci <- classical_ci(
      estimate = beta,
      standard_error = se,
      degrees_freedom = df,
      conf_level = conf_level
    )

    return(
      tibble::tibble(
        intercept = intercept,
        beta = beta,
        standard_error = ci$standard_error,
        conf_low = ci$conf_low,
        conf_high = ci$conf_high,
        p_value = p_val,
        r_squared = r2,
        degrees_freedom = df,
        n = n,
        method = "lm"
      )
    )
  }

  # Bivariate approximation (Model II / RMA) on log-transformed data.
  x <- log(dat$shoot)
  y <- log(dat$root)
  r <- suppressWarnings(stats::cor(x, y, use = "complete.obs"))
  sx <- stats::sd(x)
  sy <- stats::sd(y)

  if (!is.finite(r) || !is.finite(sx) || !is.finite(sy) || sx <= 0 || sy <= 0) {
    return(
      tibble::tibble(
        intercept = NA_real_,
        beta = NA_real_,
        standard_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_,
        degrees_freedom = df,
        n = n,
        method = "bivariate_ml (rma_approx)"
      )
    )
  }

  beta <- sign(r) * (sy / sx)
  intercept <- mean(y) - beta * mean(x)
  r2 <- r^2

  p_val <- tryCatch(
    stats::cor.test(x, y)$p.value,
    error = function(e) NA_real_
  )

  jack_beta <- vapply(
    seq_len(n),
    FUN.VALUE = numeric(1),
    FUN = function(i) {
      xi <- x[-i]
      yi <- y[-i]
      ri <- suppressWarnings(stats::cor(xi, yi, use = "complete.obs"))
      sxi <- stats::sd(xi)
      syi <- stats::sd(yi)
      if (!is.finite(ri) || !is.finite(sxi) || !is.finite(syi) || sxi <= 0 || syi <= 0) {
        return(NA_real_)
      }
      sign(ri) * (syi / sxi)
    }
  )

  jack_beta <- jack_beta[is.finite(jack_beta)]
  se <- if (length(jack_beta) >= 3) {
    sqrt(((length(jack_beta) - 1) / length(jack_beta)) * sum((jack_beta - mean(jack_beta))^2))
  } else {
    NA_real_
  }

  ci <- classical_ci(
    estimate = beta,
    standard_error = se,
    degrees_freedom = df,
    conf_level = conf_level
  )

  tibble::tibble(
    intercept = intercept,
    beta = beta,
    standard_error = ci$standard_error,
    conf_low = ci$conf_low,
    conf_high = ci$conf_high,
    p_value = p_val,
    r_squared = r2,
    degrees_freedom = df,
    n = n,
    method = "bivariate_ml (rma_approx)"
  )
}
