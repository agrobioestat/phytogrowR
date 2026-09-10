.group_match_rows <- function(data, group_row, gcols) {
  if (length(gcols) == 0) {
    return(rep(TRUE, nrow(data)))
  }

  keep <- rep(TRUE, nrow(data))
  for (g in gcols) {
    keep <- keep & (as.character(data[[g]]) == as.character(group_row[[g]]))
  }
  keep
}

#' @export
print.phytogrow_fit <- function(x, ...) {
  n_groups <- nrow(x$messages)
  n_ok <- sum(x$messages$status == "ok", na.rm = TRUE)
  n_fail <- sum(x$messages$status != "ok", na.rm = TRUE)

  cat("<phytogrow_fit>\n")
  cat(" Method  :", x$method, "\n")
  cat(" Response:", x$response, "\n")
  cat(" Groups  :", n_groups, "(", n_ok, "ok /", n_fail, "failed)\n")

  invisible(x)
}

#' @export
summary.phytogrow_fit <- function(object, ...) {
  pred <- object$predictions

  time_rng <- range(pred$time, na.rm = TRUE)
  if (!all(is.finite(time_rng))) {
    time_rng <- c(NA_real_, NA_real_)
  }

  out <- list(
    method = object$method,
    response = object$response,
    group_cols = object$group_cols,
    n_groups = nrow(object$messages),
    n_success = sum(object$messages$status == "ok", na.rm = TRUE),
    n_failed = sum(object$messages$status != "ok", na.rm = TRUE),
    time_range = time_rng,
    messages = object$messages
  )

  class(out) <- "summary.phytogrow_fit"
  out
}

#' @export
print.summary.phytogrow_fit <- function(x, ...) {
  cat("phytogrow_fit summary\n")
  cat(" Method      :", x$method, "\n")
  cat(" Response    :", x$response, "\n")
  cat(" Groups      :", x$n_groups, "\n")
  cat(" Successful  :", x$n_success, "\n")
  cat(" Failed      :", x$n_failed, "\n")
  cat(" Time range  :", paste(signif(x$time_range, 5), collapse = " to "), "\n")
  invisible(x)
}

#' @export
plot.phytogrow_fit <- function(x, ...) {
  plot_growth_curve(x, ...)
}

#' @rdname tidy
#' @export
tidy.phytogrow_fit <- function(x, ...) {
  msgs <- x$messages
  gcols <- x$group_cols

  out <- lapply(seq_along(x$models), function(i) {
    mod <- x$models[[i]]
    grp <- if (nrow(msgs) >= i && length(gcols) > 0) msgs[i, gcols, drop = FALSE] else tibble::tibble(group = "all")

    # 'broom' lives in Suggests: use it when present, otherwise fall back to a
    # coefficient-only tidy built from stats::coef().
    td <- if (is.null(mod)) {
      .empty_tidy_row()
    } else if (.has_pkg("broom")) {
      tryCatch(broom::tidy(mod), error = function(e) .coef_tidy(mod))
    } else {
      .coef_tidy(mod)
    }

    dplyr::bind_cols(grp[rep(1, nrow(td)), , drop = FALSE], td)
  })

  dplyr::bind_rows(out) %>%
    dplyr::mutate(
      method = x$method,
      response = x$response
    )
}

#' @export
augment.phytogrow_fit <- function(x, data = x$data, ...) {
  gcols <- x$group_cols
  tcol <- x$time_col

  dat <- tibble::as_tibble(data)
  dat$.fitted <- NA_real_

  if (!tcol %in% names(dat)) {
    rlang::abort("`data` must contain the fit time column.")
  }

  for (i in seq_along(x$models)) {
    mod <- x$models[[i]]
    if (is.null(mod)) {
      next
    }

    grp <- if (length(gcols) > 0) x$messages[i, gcols, drop = FALSE] else tibble::tibble(group = "all")
    idx <- .group_match_rows(dat, grp, gcols)

    if (!any(idx)) {
      next
    }

    x_new <- data.frame(x = as.numeric(dat[[tcol]][idx]))
    pred <- tryCatch(
      as.numeric(stats::predict(mod, newdata = x_new)),
      error = function(e) {
        tryCatch(as.numeric(stats::predict(mod, newdata = data.frame(x = x_new$x))), error = function(e2) rep(NA_real_, sum(idx)))
      }
    )

    dat$.fitted[idx] <- pred
  }

  dat
}

#' @export
glance.phytogrow_fit <- function(x, ...) {
  gcols <- x$group_cols
  tcol <- x$time_col
  ycol <- x$response

  out <- lapply(seq_along(x$models), function(i) {
    mod <- x$models[[i]]
    grp <- if (length(gcols) > 0) x$messages[i, gcols, drop = FALSE] else tibble::tibble(group = "all")

    if (is.null(mod)) {
      return(dplyr::bind_cols(
        grp,
        tibble::tibble(
          n_obs = x$messages$n_obs[i],
          rmse = NA_real_,
          status = "failed",
          method = x$method,
          response = ycol
        )
      ))
    }

    dat <- x$data
    idx <- .group_match_rows(dat, grp, gcols)
    dat_g <- dat[idx, , drop = FALSE]

    pred <- tryCatch(
      as.numeric(stats::predict(mod, newdata = data.frame(x = dat_g[[tcol]]))),
      error = function(e) rep(NA_real_, nrow(dat_g))
    )

    rmse <- sqrt(mean((dat_g[[ycol]] - pred)^2, na.rm = TRUE))

    dplyr::bind_cols(
      grp,
      tibble::tibble(
        n_obs = nrow(dat_g),
        rmse = rmse,
        status = "ok",
        method = x$method,
        response = ycol
      )
    )
  })

  dplyr::bind_rows(out)
}
