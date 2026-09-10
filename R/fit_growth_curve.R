.fit_one_growth_curve <- function(df, method, n_grid = 100, span = 0.75, spline_spar = NULL, gam_k = 10) {
  df <- df %>%
    dplyr::filter(is.finite(.data$x), is.finite(.data$y)) %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(y = mean(.data$y, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(.data$x)

  if (nrow(df) < 4) {
    return(list(
      ok = FALSE,
      model = NULL,
      pred = NULL,
      message = "Not enough points to fit curve (minimum 4 unique times)."
    ))
  }

  x_grid <- seq(min(df$x), max(df$x), length.out = n_grid)

  safe_error <- function(e) {
    list(ok = FALSE, model = NULL, pred = NULL, message = conditionMessage(e))
  }

  if (method == "spline") {
    fit <- tryCatch(
      stats::smooth.spline(x = df$x, y = df$y, spar = spline_spar),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    p0 <- predict(fit, x = x_grid)
    p1 <- predict(fit, x = x_grid, deriv = 1)

    pred <- tibble::tibble(
      time = x_grid,
      fitted = as.numeric(p0$y),
      deriv = as.numeric(p1$y),
      se = NA_real_
    )

    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  if (method == "gam") {
    k_use <- max(4, min(gam_k, length(unique(df$x)) - 1))
    gam_formula <- stats::as.formula(paste0("y ~ s(x, k = ", k_use, ")"))

    fit <- tryCatch(
      mgcv::gam(gam_formula, data = df, method = "REML"),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    pred_obj <- tryCatch(
      stats::predict(fit, newdata = data.frame(x = x_grid), se.fit = TRUE),
      error = safe_error
    )
    if (is.list(pred_obj) && isFALSE(pred_obj$ok)) return(pred_obj)

    fitted <- as.numeric(pred_obj$fit)
    deriv <- .numeric_derivative(x_grid, fitted, central = TRUE)

    pred <- tibble::tibble(
      time = x_grid,
      fitted = fitted,
      deriv = deriv,
      se = as.numeric(pred_obj$se.fit)
    )

    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  if (method == "loess") {
    fit <- tryCatch(
      stats::loess(y ~ x, data = df, span = span, degree = 2, na.action = stats::na.exclude),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    fitted <- as.numeric(stats::predict(fit, newdata = data.frame(x = x_grid)))
    deriv <- .numeric_derivative(x_grid, fitted, central = TRUE)

    pred <- tibble::tibble(
      time = x_grid,
      fitted = fitted,
      deriv = deriv,
      se = NA_real_
    )

    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  if (method == "logistic") {
    start <- list(
      Asym = max(df$y, na.rm = TRUE) * 1.1,
      xmid = stats::median(df$x, na.rm = TRUE),
      scal = diff(range(df$x)) / 4
    )

    fit <- tryCatch(
      stats::nls(
        y ~ Asym / (1 + exp((xmid - x) / scal)),
        data = df,
        start = start,
        algorithm = "port",
        lower = c(Asym = 1e-8, xmid = min(df$x) - 100, scal = 1e-8)
      ),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    fitted <- as.numeric(stats::predict(fit, newdata = data.frame(x = x_grid)))
    deriv <- .numeric_derivative(x_grid, fitted, central = TRUE)

    pred <- tibble::tibble(time = x_grid, fitted = fitted, deriv = deriv, se = NA_real_)
    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  if (method == "gompertz") {
    start <- list(
      Asym = max(df$y, na.rm = TRUE) * 1.1,
      b = 5,
      c = 0.05
    )

    fit <- tryCatch(
      stats::nls(
        y ~ Asym * exp(-b * exp(-c * x)),
        data = df,
        start = start,
        algorithm = "port",
        lower = c(Asym = 1e-8, b = 1e-8, c = 1e-8)
      ),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    fitted <- as.numeric(stats::predict(fit, newdata = data.frame(x = x_grid)))
    deriv <- .numeric_derivative(x_grid, fitted, central = TRUE)

    pred <- tibble::tibble(time = x_grid, fitted = fitted, deriv = deriv, se = NA_real_)
    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  if (method == "richards") {
    start <- list(
      Asym = max(df$y, na.rm = TRUE) * 1.2,
      nu = 1,
      k = 0.05,
      xmid = stats::median(df$x, na.rm = TRUE)
    )

    fit <- tryCatch(
      stats::nls(
        y ~ Asym / ((1 + nu * exp(-k * (x - xmid)))^(1 / nu)),
        data = df,
        start = start,
        algorithm = "port",
        lower = c(Asym = 1e-8, nu = 1e-6, k = 1e-8, xmid = min(df$x) - 100)
      ),
      error = safe_error
    )
    if (is.list(fit) && isFALSE(fit$ok)) return(fit)

    fitted <- as.numeric(stats::predict(fit, newdata = data.frame(x = x_grid)))
    deriv <- .numeric_derivative(x_grid, fitted, central = TRUE)

    pred <- tibble::tibble(time = x_grid, fitted = fitted, deriv = deriv, se = NA_real_)
    return(list(ok = TRUE, model = fit, pred = pred, message = "ok"))
  }

  rlang::abort("Unsupported fitting method.")
}

#' Fit Growth Curves for Biomass or Leaf Area
#'
#' Fit smooth growth trajectories by treatment/genotype/group and derive
#' approximate instantaneous rates from fitted curves.
#'
#' @param data A data frame containing time and response columns.
#' @param response Response column to fit (for example `"total_biomass_g"`).
#' @param time_col Name of numeric time column.
#' @param group_cols Grouping columns for independent fits.
#' @param method Curve fitting method: `"spline"`, `"gam"`, `"loess"`,
#' `"logistic"`, `"gompertz"`, or `"richards"`.
#' @param n_grid Number of prediction points per group.
#' @param span `loess` span parameter.
#' @param spline_spar Optional `smooth.spline` smoothing parameter.
#' @param gam_k Basis size (`k`) used in `mgcv::s()`.
#'
#' @details
#' Returns an S3 object of class `phytogrow_fit` containing original data,
#' fitted model(s), predicted values, approximate derivatives, and fit messages.
#'
#' @return An object of class `phytogrow_fit`.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' fit <- fit_growth_curve(
#'   dat,
#'   response = "total_biomass_g",
#'   group_cols = c("treatment"),
#'   method = "gam"
#' )
#' print(fit)
#' @export
fit_growth_curve <- function(
    data,
    response = "total_biomass_g",
    time_col = "time",
    group_cols = c("treatment", "genotype", "block", "plot_id"),
    method = c("spline", "gam", "loess", "logistic", "gompertz", "richards"),
    n_grid = 120,
    span = 0.75,
    spline_spar = NULL,
    gam_k = 10
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  if (!response %in% names(data)) {
    rlang::abort(paste0("Response column `", response, "` was not found in `data`."))
  }

  if (!time_col %in% names(data)) {
    rlang::abort(paste0("Time column `", time_col, "` was not found in `data`."))
  }

  .assert_positive_integer(n_grid, arg = "n_grid")

  dat <- tibble::as_tibble(data) %>%
    dplyr::mutate(
      .time = as.numeric(.data[[time_col]]),
      .response = as.numeric(.data[[response]])
    ) %>%
    dplyr::filter(!is.na(.data$.time), !is.na(.data$.response))

  if (nrow(dat) == 0) {
    rlang::abort("No complete cases available for fitting.")
  }

  gcols <- intersect(group_cols, names(dat))
  dat$..group_key <- .make_group_key(dat, gcols)

  split_data <- split(dat, dat$..group_key)

  pred_list <- vector("list", length(split_data))
  model_list <- vector("list", length(split_data))
  msg_list <- vector("list", length(split_data))

  idx <- 1L
  for (key in names(split_data)) {
    gdat <- tibble::as_tibble(split_data[[key]])

    fit_res <- .fit_one_growth_curve(
      df = tibble::tibble(x = gdat$.time, y = gdat$.response),
      method = method,
      n_grid = n_grid,
      span = span,
      spline_spar = spline_spar,
      gam_k = gam_k
    )

    group_ref <- if (length(gcols) > 0) {
      dplyr::select(gdat, dplyr::all_of(gcols)) %>% dplyr::slice(1)
    } else {
      tibble::tibble(group = "all")
    }

    if (isTRUE(fit_res$ok)) {
      pred <- fit_res$pred %>%
        dplyr::mutate(
          conf.low = .data$fitted - 1.96 * .data$se,
          conf.high = .data$fitted + 1.96 * .data$se
        )

      pred <- dplyr::bind_cols(
        group_ref[rep(1, nrow(pred)), , drop = FALSE],
        pred
      )

      model_list[[idx]] <- fit_res$model
      pred_list[[idx]] <- pred
      msg_list[[idx]] <- dplyr::bind_cols(
        group_ref,
        tibble::tibble(
          status = "ok",
          message = fit_res$message,
          n_obs = nrow(gdat)
        )
      )
    } else {
      model_list[[idx]] <- NULL
      pred_list[[idx]] <- dplyr::bind_cols(
        group_ref,
        tibble::tibble(
          time = NA_real_,
          fitted = NA_real_,
          deriv = NA_real_,
          se = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_
        )
      )
      msg_list[[idx]] <- dplyr::bind_cols(
        group_ref,
        tibble::tibble(
          status = "failed",
          message = fit_res$message,
          n_obs = nrow(gdat)
        )
      )
    }

    idx <- idx + 1L
  }

  predictions <- dplyr::bind_rows(pred_list)
  messages <- dplyr::bind_rows(msg_list)

  out <- list(
    call = match.call(),
    method = method,
    response = response,
    time_col = time_col,
    group_cols = gcols,
    data = dat %>% dplyr::select(-..group_key, -.time, -.response),
    models = model_list,
    predictions = predictions,
    messages = messages
  )

  class(out) <- c("phytogrow_fit", class(out))
  out
}
