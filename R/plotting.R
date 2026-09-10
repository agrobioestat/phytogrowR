#' Plot Fitted Growth Curves
#'
#' Generate publication-oriented growth trajectory plots from raw data and fitted
#' curves.
#'
#' @param x A `phytogrow_fit` object.
#' @param show_points Logical; draw observed points.
#' @param show_ci Logical; draw confidence bands when available.
#' @param facet_var Optional column name to facet by.
#' @param y_scale Either `"linear"` or `"log"`.
#' @param point_alpha Point transparency.
#' @param line_size Curve line size.
#' @param theme_fn Theme function applied to the plot.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' fit <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = "treatment")
#' plot_growth_curve(fit)
#' @export
plot_growth_curve <- function(
    x,
    show_points = TRUE,
    show_ci = TRUE,
    facet_var = NULL,
    y_scale = c("linear", "log"),
    point_alpha = 0.6,
    line_size = 1.0,
    theme_fn = ggplot2::theme_minimal
) {
  if (!inherits(x, "phytogrow_fit")) {
    rlang::abort("`x` must be a `phytogrow_fit` object.")
  }

  y_scale <- match.arg(y_scale)

  pred <- x$predictions %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$fitted))

  if (nrow(pred) == 0) {
    rlang::abort("No valid predictions available for plotting.")
  }

  gcols <- x$group_cols
  color_col <- if (length(gcols) > 0) gcols[1] else NULL

  plt <- ggplot2::ggplot(pred, ggplot2::aes(x = .data$time, y = .data$fitted))

  if (!is.null(color_col)) {
    plt <- plt + ggplot2::aes(color = .data[[color_col]], fill = .data[[color_col]])
  }

  if (show_ci && all(c("conf.low", "conf.high") %in% names(pred)) && any(!is.na(pred$conf.low))) {
    plt <- plt + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
      alpha = 0.18,
      linewidth = 0,
      show.legend = FALSE
    )
  }

  plt <- plt + ggplot2::geom_line(linewidth = line_size)

  if (show_points) {
    raw <- x$data %>%
      dplyr::mutate(
        .time = as.numeric(.data[[x$time_col]]),
        .response = as.numeric(.data[[x$response]])
      )

    if (!is.null(color_col)) {
      plt <- plt + ggplot2::geom_point(
        data = raw,
        ggplot2::aes(x = .data$.time, y = .data$.response, color = .data[[color_col]]),
        alpha = point_alpha,
        inherit.aes = FALSE
      )
    } else {
      plt <- plt + ggplot2::geom_point(
        data = raw,
        ggplot2::aes(x = .data$.time, y = .data$.response),
        alpha = point_alpha,
        inherit.aes = FALSE
      )
    }
  }

  if (!is.null(facet_var) && facet_var %in% names(pred)) {
    plt <- plt + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
  }

  if (y_scale == "log") {
    plt <- plt + ggplot2::scale_y_log10()
  }

  plt +
    ggplot2::labs(
      x = "Time",
      y = x$response,
      color = color_col,
      title = paste("Growth curve fit (", x$method, ")", sep = "")
    ) +
    theme_fn()
}

#' Plot Growth Rates
#'
#' Plot classical interval rates or instantaneous rates in tidy long format.
#'
#' @param data Output from [calc_classic_growth()] or [calc_instant_rates()].
#' @param type Either `"instantaneous"` or `"classic"`.
#' @param metrics Character vector of metrics to show.
#' @param facet_var Optional facet variable.
#' @param theme_fn Theme function.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"))
#' plot_growth_rates(classic, type = "classic", metrics = c("rgr", "agr"))
#' @export
plot_growth_rates <- function(
    data,
    type = c("instantaneous", "classic"),
    metrics = NULL,
    facet_var = NULL,
    theme_fn = ggplot2::theme_minimal
) {
  type <- match.arg(type)
  dat <- tibble::as_tibble(data)

  if (type == "classic") {
    default_metrics <- c("rgr", "agr", "nar", "algr", "rlgr", "lar", "lai", "sla", "cgr")
    metrics <- metrics %||% default_metrics
    metrics <- intersect(metrics, names(dat))

    if (length(metrics) == 0) {
      rlang::abort("No requested classic metrics were found in `data`.")
    }

    if (!"time_start" %in% names(dat) || !"time_end" %in% names(dat)) {
      rlang::abort("Classic data must include `time_start` and `time_end`.")
    }

    long <- dat %>%
      dplyr::mutate(time = (.data$time_start + .data$time_end) / 2) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(metrics),
        names_to = "metric",
        values_to = "value"
      )

    color_col <- if ("treatment" %in% names(long)) "treatment" else if ("genotype" %in% names(long)) "genotype" else NULL

    plt <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = .data$value))
    if (!is.null(color_col)) {
      plt <- plt + ggplot2::aes(color = .data[[color_col]])
    }

    plt <- plt + ggplot2::geom_line(alpha = 0.7) + ggplot2::geom_point(size = 1.3)
  } else {
    default_metrics <- c("agr_inst", "rgr_inst", "algr_inst", "rlgr_inst", "nar_inst", "lar_inst", "lai_inst", "sla_inst", "cgr_inst")
    metrics <- metrics %||% default_metrics
    metrics <- intersect(metrics, names(dat))

    if (length(metrics) == 0) {
      rlang::abort("No requested instantaneous metrics were found in `data`.")
    }

    if (!"time" %in% names(dat)) {
      rlang::abort("Instantaneous data must include a `time` column.")
    }

    long <- dat %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(metrics),
        names_to = "metric",
        values_to = "value"
      )

    color_col <- if ("treatment" %in% names(long)) "treatment" else if ("genotype" %in% names(long)) "genotype" else NULL

    plt <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = .data$value))
    if (!is.null(color_col)) {
      plt <- plt + ggplot2::aes(color = .data[[color_col]])
    }

    plt <- plt + ggplot2::geom_line(linewidth = 0.9)
  }

  plt <- plt + ggplot2::facet_wrap(~metric, scales = "free_y")

  if (!is.null(facet_var) && facet_var %in% names(long)) {
    plt <- plt + ggplot2::facet_grid(stats::as.formula(paste(facet_var, "~ metric")), scales = "free_y")
  }

  plt +
    ggplot2::labs(x = "Time", y = "Rate / Index", color = NULL) +
    theme_fn()
}

#' Plot Biomass Partition Through Time
#'
#' Create partition plots for leaf, stem, root, and reproductive fractions.
#'
#' @param data Output from [biomass_partition()] or any table containing
#' `prop_leaf`, `prop_stem`, `prop_root`, and optionally `prop_reproductive`.
#' @param x_col Name of time column.
#' @param stacked Logical; use stacked area plot if `TRUE`.
#' @param facet_var Optional facet variable.
#' @param theme_fn Theme function.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' part <- biomass_partition(dat, group_cols = c("treatment", "time"))
#' plot_partition(part, facet_var = "treatment")
#' @export
plot_partition <- function(
    data,
    x_col = "time",
    stacked = TRUE,
    facet_var = NULL,
    theme_fn = ggplot2::theme_minimal
) {
  dat <- tibble::as_tibble(data)

  req <- intersect(
    c("prop_leaf", "prop_stem", "prop_root", "prop_reproductive"),
    names(dat)
  )

  if (length(req) < 3) {
    rlang::abort("`data` must contain partition proportion columns (prop_leaf, prop_stem, prop_root).")
  }

  if (!x_col %in% names(dat)) {
    rlang::abort(paste0("`", x_col, "` not found in `data`."))
  }

  long <- dat %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(req),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$component,
        prop_leaf = "Leaf",
        prop_stem = "Stem",
        prop_root = "Root",
        prop_reproductive = "Reproductive"
      )
    )

  plt <- ggplot2::ggplot(long, ggplot2::aes(x = .data[[x_col]], y = .data$value, fill = .data$component, color = .data$component))

  if (stacked) {
    plt <- plt + ggplot2::geom_area(alpha = 0.75, position = "stack")
  } else {
    plt <- plt + ggplot2::geom_line(linewidth = 1)
  }

  if (!is.null(facet_var) && facet_var %in% names(long)) {
    plt <- plt + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
  }

  plt +
    ggplot2::labs(x = x_col, y = "Biomass proportion", fill = "Component", color = "Component") +
    theme_fn()
}
