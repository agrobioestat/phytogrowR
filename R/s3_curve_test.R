# S3 methods for the coincidence-of-curves test -------------------------------

.curve_test_label <- function(x) {
  base <- if (identical(x$model, "poly")) {
    paste0("polynomial of degree ", x$degree)
  } else {
    paste0(x$model, " curve")
  }

  if (isTRUE(x$log_response)) paste0(base, " on ln(", x$response, ")") else base
}

# Prediction grid used by plot() and by the Shiny module ----------------------

.curve_test_predict <- function(x, n_grid = 120) {
  df <- x$data
  grid <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = n_grid)

  # NOTE: `tibble::tibble()` evaluates its arguments sequentially inside a data
  # mask, so a column literally named `x` would shadow the `x` object for every
  # later argument. Predictions are therefore computed BEFORE the tibble is
  # assembled, and the grid column is only named `x` at the very end.
  grid_frame <- function(group, fitted) {
    tibble::tibble(group = group, x = grid, fitted = as.numeric(fitted))
  }

  reduced_fitted <- tryCatch(
    stats::predict(x$models$reduced, newdata = data.frame(x = grid)),
    error = function(e) NULL
  )
  common <- if (is.null(reduced_fitted)) NULL else grid_frame("common curve (H0)", reduced_fitted)

  per_group <- if (identical(x$model, "poly")) {
    full_model <- x$models$full
    levs <- x$groups
    tryCatch(
      dplyr::bind_rows(lapply(levs, function(lv) {
        nd <- data.frame(x = grid, g = factor(lv, levels = levs))
        grid_frame(lv, stats::predict(full_model, newdata = nd))
      })),
      error = function(e) NULL
    )
  } else {
    gf <- x$models$group_fits
    tryCatch(
      dplyr::bind_rows(lapply(names(gf), function(lv) {
        grid_frame(lv, stats::predict(gf[[lv]], newdata = data.frame(x = grid)))
      })),
      error = function(e) NULL
    )
  }

  out <- dplyr::bind_rows(per_group, common)

  if (nrow(out) == 0) {
    return(out)
  }

  # Undo the internal centring so the axis is on the user's time scale again.
  out$time <- out$x + x$time_shift
  out$hypothesis <- ifelse(out$group == "common curve (H0)", "reduced", "full")
  out
}

#' @export
print.phytogrow_curve_test <- function(x, ...) {
  cat("Coincidence of growth curves between treatments\n")
  cat("-----------------------------------------------\n")
  cat("Response       : ", x$response,
      if (isTRUE(x$log_response)) " (natural log scale)" else "", "\n", sep = "")
  cat("Time           : ", x$time_col, "\n", sep = "")
  cat("Grouping       : ", x$group_var, " (", x$n_groups, " levels: ",
      paste(x$groups, collapse = ", "), ")\n", sep = "")
  cat("Model          : ", .curve_test_label(x), "\n", sep = "")
  cat("Observations   : ", x$n_obs, "\n", sep = "")
  cat("Parameters     : reduced = ", x$n_par[["reduced"]],
      ", full = ", x$n_par[["full"]], "\n", sep = "")
  cat("\n")

  tst <- x$tests
  if (is.null(tst) || nrow(tst) == 0) {
    cat("No test could be computed.\n")
    return(invisible(x))
  }

  show <- as.data.frame(tst[, c(
    "hypothesis", "term", "test", "df_num", "df_den", "statistic", "p_value"
  ), drop = FALSE])

  show$statistic <- format(round(show$statistic, 4), nsmall = 4)
  show$p_value <- format.pval(tst$p_value, digits = 4, eps = 1e-12)
  show$df_num <- ifelse(is.na(show$df_num), "", format(show$df_num))
  show$df_den <- ifelse(is.na(show$df_den), "", format(show$df_den))

  print(show, row.names = FALSE)

  coincidence <- tst[tst$hypothesis == "coincidence" & !is.na(tst$p_value), , drop = FALSE]
  if (nrow(coincidence) > 0) {
    p_ref <- coincidence$p_value[1]
    cat("\n")
    if (p_ref < 0.05) {
      cat("H0 of coincident curves is REJECTED at 5%: treatments require separate curves.\n")
    } else {
      cat("H0 of coincident curves is NOT rejected at 5%: a single curve describes all treatments.\n")
    }
  }

  if (!is.null(x$pairwise) && nrow(x$pairwise) > 0) {
    cat("\nPairwise coincidence tests (p adjusted by '", x$p_adjust_method, "'):\n", sep = "")
    pw <- as.data.frame(x$pairwise)
    pw$statistic <- format(round(pw$statistic, 4), nsmall = 4)
    pw$df_num <- ifelse(is.na(pw$df_num), "", format(pw$df_num))
    pw$df_den <- ifelse(is.na(pw$df_den), "", format(pw$df_den))
    pw$p_value <- format.pval(x$pairwise$p_value, digits = 4, eps = 1e-12)
    pw$p_adj <- format.pval(x$pairwise$p_adj, digits = 4, eps = 1e-12)
    print(pw, row.names = FALSE)
  }

  if (!identical(x$status, "ok")) {
    cat("\nNote: ", x$message, "\n", sep = "")
  }

  invisible(x)
}

#' @export
summary.phytogrow_curve_test <- function(object, ...) {
  out <- list(
    object = object,
    tests = object$tests,
    pairwise = object$pairwise,
    parameters = object$parameters
  )
  class(out) <- "summary.phytogrow_curve_test"
  out
}

#' @export
print.summary.phytogrow_curve_test <- function(x, ...) {
  print(x$object)

  if (!is.null(x$parameters) && nrow(x$parameters) > 0) {
    cat("\nPer-treatment parameter estimates:\n")
    print(as.data.frame(x$parameters), row.names = FALSE)
  }

  cat("\nInterpretation guide:\n")
  cat("  coincidence : H0 = one single curve fits every treatment.\n")

  if (identical(x$object$model, "poly")) {
    cat("  level       : H0 = same intercept, given a common shape.\n")
    cat("  shape       : H0 = same time coefficients (parallel curves).\n")
  } else {
    cat("  parameter   : H0 = that single parameter is common to all treatments.\n")
  }

  cat("  ", x$object$message, "\n", sep = "")
  invisible(x)
}

#' @rdname tidy
#' @export
tidy.phytogrow_curve_test <- function(x, ...) {
  x$tests %>%
    dplyr::mutate(
      model = x$model,
      response = x$response,
      group_var = x$group_var,
      log_response = x$log_response
    )
}

#' @export
glance.phytogrow_curve_test <- function(x, ...) {
  coincidence <- x$tests %>%
    dplyr::filter(.data$hypothesis == "coincidence")

  f_row <- coincidence %>% dplyr::filter(.data$test == "F")
  l_row <- coincidence %>% dplyr::filter(.data$test == "LRT")

  tibble::tibble(
    model = x$model,
    response = x$response,
    group_var = x$group_var,
    n_groups = x$n_groups,
    n_obs = x$n_obs,
    log_response = x$log_response,
    statistic_F = if (nrow(f_row) > 0) f_row$statistic[1] else NA_real_,
    p_value_F = if (nrow(f_row) > 0) f_row$p_value[1] else NA_real_,
    statistic_LRT = if (nrow(l_row) > 0) l_row$statistic[1] else NA_real_,
    p_value_LRT = if (nrow(l_row) > 0) l_row$p_value[1] else NA_real_,
    rss_reduced = if (nrow(coincidence) > 0) coincidence$rss_reduced[1] else NA_real_,
    rss_full = if (nrow(coincidence) > 0) coincidence$rss_full[1] else NA_real_
  )
}

#' Plot the Coincidence-of-Curves Test
#'
#' Draws the observed data, the treatment-specific curves (the full model) and
#' the single common curve implied by the null hypothesis, so the magnitude of
#' the lack of fit that the test quantifies becomes visible.
#'
#' @param x An object returned by [compare_growth_curves()].
#' @param n_grid Number of prediction points per curve.
#' @param show_common Logical; draw the common (reduced-model) curve as a
#' dashed black line.
#' @param point_alpha Transparency of the observed points.
#' @param ... Currently ignored.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' ct <- compare_growth_curves(dat, group_var = "treatment", degree = 2)
#' plot(ct)
#' @export
plot.phytogrow_curve_test <- function(x, n_grid = 120, show_common = TRUE,
                                      point_alpha = 0.45, ...) {
  pred <- .curve_test_predict(x, n_grid = n_grid)

  obs <- x$data
  obs$time <- obs$x + x$time_shift

  y_lab <- if (isTRUE(x$log_response)) paste0("ln(", x$response, ")") else x$response

  coincidence <- x$tests %>%
    dplyr::filter(.data$hypothesis == "coincidence", !is.na(.data$p_value))

  subtitle <- if (nrow(coincidence) > 0) {
    paste0(
      vapply(
        seq_len(nrow(coincidence)),
        function(i) {
          sprintf(
            "%s = %.3f (p = %s)",
            coincidence$test[i],
            coincidence$statistic[i],
            format.pval(coincidence$p_value[i], digits = 3, eps = 1e-12)
          )
        },
        character(1)
      ),
      collapse = "   |   "
    )
  } else {
    NULL
  }

  plt <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(x = .data$time, y = .data$y, colour = .data$g),
      alpha = point_alpha,
      size = 1.9
    )

  if (nrow(pred) > 0) {
    plt <- plt +
      ggplot2::geom_line(
        data = pred[pred$hypothesis == "full", , drop = FALSE],
        ggplot2::aes(x = .data$time, y = .data$fitted, colour = .data$group),
        linewidth = 1
      )

    if (isTRUE(show_common)) {
      plt <- plt +
        ggplot2::geom_line(
          data = pred[pred$hypothesis == "reduced", , drop = FALSE],
          ggplot2::aes(x = .data$time, y = .data$fitted),
          colour = "black",
          linetype = "dashed",
          linewidth = 0.9
        )
    }
  }

  plt +
    ggplot2::labs(
      title = paste0("Coincidence of growth curves - ", .curve_test_label(x)),
      subtitle = subtitle,
      caption = if (isTRUE(show_common)) {
        "Dashed black line: single common curve under H0."
      } else {
        NULL
      },
      x = x$time_col,
      y = y_lab,
      colour = x$group_var
    ) +
    ggplot2::theme_bw(base_size = 12)
}
