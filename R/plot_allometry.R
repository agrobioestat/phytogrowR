#' Plot Root-Shoot Allometry
#'
#' Create a root-shoot allometry plot with:
#'
#' - `x = log(shoot_biomass)`
#' - `y = log(root_biomass)`
#' - points
#' - fitted line
#' - confidence band
#' - optional facets by treatment and/or genotype
#' - optional equation annotation (overall fit)
#'
#' @param data Data frame with root and shoot biomass columns.
#' @param shoot_biomass Column name for shoot biomass.
#' @param root_biomass Column name for root biomass.
#' @param treatment Optional treatment column name.
#' @param genotype Optional genotype column name.
#' @param conf_level Confidence level for fitted interval.
#' @param show_equation Logical; show equation when no faceting is used.
#' @param point_alpha Point transparency.
#'
#' @return A `ggplot2` object.
#'
#' @references
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' dat <- hunt_classical_example
#' plot_allometry(
#'   data = dat,
#'   shoot_biomass = "shoot_biomass_g",
#'   root_biomass = "root_biomass_g",
#'   treatment = "treatment"
#' )
#'
#' @section Interpretation:
#' The slope of the fitted line corresponds to the allometric coefficient.
#'
#' @section Limitations:
#' Requires strictly positive root and shoot biomass for log transforms.
#'
#' @export
plot_allometry <- function(
    data,
    shoot_biomass = "shoot_biomass_g",
    root_biomass = "root_biomass_g",
    treatment = NULL,
    genotype = NULL,
    conf_level = 0.95,
    show_equation = TRUE,
    point_alpha = 0.65
) {
  .assert_conf_level(conf_level)

  dat <- tibble::as_tibble(data)

  if (!shoot_biomass %in% names(dat)) {
    rlang::abort("`shoot_biomass` column was not found in `data`.")
  }
  if (!root_biomass %in% names(dat)) {
    rlang::abort("`root_biomass` column was not found in `data`.")
  }

  facet_cols <- intersect(c(treatment, genotype), names(dat))

  dat <- dat %>%
    dplyr::mutate(
      .shoot = as.numeric(.data[[shoot_biomass]]),
      .root = as.numeric(.data[[root_biomass]])
    ) %>%
    dplyr::filter(!is.na(.data$.shoot), !is.na(.data$.root), .data$.shoot > 0, .data$.root > 0) %>%
    dplyr::mutate(
      log_shoot = log(.data$.shoot),
      log_root = log(.data$.root)
    )

  if (nrow(dat) < 3) {
    rlang::abort("At least three valid observations are required for allometry plotting.")
  }

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$log_shoot, y = .data$log_root))

  if (!is.null(treatment) && treatment %in% names(dat)) {
    plt <- plt + ggplot2::aes(color = .data[[treatment]])
  }

  plt <- plt +
    ggplot2::geom_point(alpha = point_alpha) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, level = conf_level) +
    ggplot2::labs(
      x = "log(shoot_biomass)",
      y = "log(root_biomass)",
      color = treatment,
      title = "Root-shoot allometry"
    ) +
    ggplot2::theme_minimal()

  if (length(facet_cols) == 1L) {
    plt <- plt + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_cols[1])))
  } else if (length(facet_cols) == 2L) {
    plt <- plt + ggplot2::facet_grid(stats::as.formula(paste(facet_cols[1], "~", facet_cols[2])))
  }

  if (isTRUE(show_equation) && length(facet_cols) == 0L) {
    fit <- stats::lm(log_root ~ log_shoot, data = dat)
    cf <- stats::coef(fit)
    r2 <- summary(fit)$r.squared
    eq <- paste0(
      "log(root) = ",
      signif(cf[1], 4),
      if (cf[2] >= 0) " + " else " - ",
      signif(abs(cf[2]), 4),
      " * log(shoot)\nR2 = ",
      signif(r2, 4)
    )

    xr <- range(dat$log_shoot, na.rm = TRUE)
    yr <- range(dat$log_root, na.rm = TRUE)
    plt <- plt + ggplot2::annotate(
      "text",
      x = xr[1] + 0.02 * diff(xr),
      y = yr[2] - 0.03 * diff(yr),
      label = eq,
      hjust = 0,
      vjust = 1,
      size = 3.4
    )
  }

  plt
}
