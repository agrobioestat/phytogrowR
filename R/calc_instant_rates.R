#' Calculate Instantaneous Growth Rates from Fitted Curves
#'
#' Derive instantaneous rates from smooth growth curves fitted with
#' [fit_growth_curve()].
#'
#' @param biomass_fit A `phytogrow_fit` object for total biomass.
#' @param leaf_area_fit Optional `phytogrow_fit` object for leaf area.
#' @param leaf_mass_fit Optional `phytogrow_fit` object for leaf biomass.
#' @param ground_area Either `NULL`, a single numeric value, or a column name
#' available in the original fit data.
#' @param central Logical; use central difference for numerical derivatives.
#' @param epsilon Optional positive offset for `log(W)` when values are near zero.
#'
#' @details
#' Computed rates:
#' - instantaneous `AGR = dW/dt`
#' - instantaneous `RGR = d(log W)/dt`
#' - instantaneous `NAR = (1/LA) * dW/dt`
#' - instantaneous `LAR = LA/W`
#' - instantaneous `SLA = LA/leaf_mass` (if leaf mass fit is provided)
#' - instantaneous `CGR = (1/ground_area) * dW/dt`
#' - instantaneous `ALGR = dLA/dt` and `RLGR = d(log LA)/dt` (if leaf area fit is provided)
#' - instantaneous `LAI = (LA/10000)/ground_area`
#'
#' @return A tibble with fitted values and instantaneous rates.
#'
#' @references
#' Hunt, R. (1990). *Basic Growth Analysis*. Unwin Hyman, London.
#'
#' Benincasa, M. M. P. (2003). *Plant Growth Analysis: Basic Concepts*.
#' FUNEP, Jaboticabal.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' fit_w <- fit_growth_curve(dat, response = "total_biomass_g", group_cols = "treatment")
#' fit_la <- fit_growth_curve(dat, response = "leaf_area_cm2", group_cols = "treatment")
#' inst <- calc_instant_rates(fit_w, leaf_area_fit = fit_la)
#' utils::head(inst)
#' @export
calc_instant_rates <- function(
    biomass_fit,
    leaf_area_fit = NULL,
    leaf_mass_fit = NULL,
    ground_area = NULL,
    central = TRUE,
    epsilon = NULL
) {
  if (!inherits(biomass_fit, "phytogrow_fit")) {
    rlang::abort("`biomass_fit` must be an object returned by `fit_growth_curve()`." )
  }

  if (!is.logical(central) || length(central) != 1 || is.na(central)) {
    rlang::abort("`central` must be TRUE or FALSE.")
  }

  gcols <- biomass_fit$group_cols

  bio <- biomass_fit$predictions %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$fitted)) %>%
    # Quoted tidyselect names: avoids relying on stats::deriv being imported
    # just to give the bare `deriv` symbol a binding.
    dplyr::rename(W = "fitted", dWdt = "deriv")

  if (nrow(bio) == 0) {
    return(tibble::tibble())
  }

  # Recompute derivative from fitted values when requested.
  if (central) {
    if (length(gcols) > 0) {
      bio <- bio %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
        dplyr::arrange(.data$time, .by_group = TRUE) %>%
        dplyr::mutate(dWdt = .numeric_derivative(.data$time, .data$W, central = TRUE)) %>%
        dplyr::ungroup()
    } else {
      bio <- bio %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(dWdt = .numeric_derivative(.data$time, .data$W, central = TRUE))
    }
  }

  if (is.null(epsilon) && any(bio$W <= 0, na.rm = TRUE)) {
    rlang::abort("Fitted biomass has non-positive values. Use positive fits or provide `epsilon`.")
  }

  merge_fit_at_time <- function(base_tbl, fit_obj, value_name, deriv_name = NULL) {
    if (is.null(fit_obj)) {
      base_tbl[[value_name]] <- NA_real_
      if (!is.null(deriv_name)) {
        base_tbl[[deriv_name]] <- NA_real_
      }
      return(base_tbl)
    }
    if (!inherits(fit_obj, "phytogrow_fit")) {
      rlang::abort("Optional fits must be objects of class `phytogrow_fit`.")
    }

    pred <- fit_obj$predictions %>%
      dplyr::filter(!is.na(.data$time), !is.na(.data$fitted))

    if (nrow(pred) == 0) {
      base_tbl[[value_name]] <- NA_real_
      if (!is.null(deriv_name)) {
        base_tbl[[deriv_name]] <- NA_real_
      }
      return(base_tbl)
    }

    out <- base_tbl
    out[[value_name]] <- NA_real_
    if (!is.null(deriv_name)) {
      out[[deriv_name]] <- NA_real_
    }

    if (length(gcols) == 0) {
      out[[value_name]] <- stats::approx(
        x = pred$time,
        y = pred$fitted,
        xout = out$time,
        rule = 2
      )$y

      if (!is.null(deriv_name)) {
        out[[deriv_name]] <- stats::approx(
          x = pred$time,
          y = pred$deriv,
          xout = out$time,
          rule = 2
        )$y
      }
      return(out)
    }

    split_base <- split(out, .make_group_key(out, gcols))
    split_pred <- split(pred, .make_group_key(pred, gcols))

    merged <- lapply(names(split_base), function(k) {
      b <- split_base[[k]]
      p <- split_pred[[k]]
      if (is.null(p) || nrow(p) < 2) {
        b[[value_name]] <- NA_real_
        if (!is.null(deriv_name)) {
          b[[deriv_name]] <- NA_real_
        }
      } else {
        b[[value_name]] <- stats::approx(
          x = p$time,
          y = p$fitted,
          xout = b$time,
          rule = 2
        )$y
        if (!is.null(deriv_name)) {
          b[[deriv_name]] <- stats::approx(
            x = p$time,
            y = p$deriv,
            xout = b$time,
            rule = 2
          )$y
        }
      }
      b
    })

    dplyr::bind_rows(merged)
  }

  out <- bio
  out <- merge_fit_at_time(out, leaf_area_fit, "LA", "dLAdt")
  out <- merge_fit_at_time(out, leaf_mass_fit, "leaf_mass")

  if (is.numeric(ground_area) && length(ground_area) == 1) {
    out$ground_area_use <- ground_area
  } else if (is.character(ground_area) && length(ground_area) == 1 &&
             ground_area %in% names(biomass_fit$data)) {
    join_cols <- intersect(gcols, names(biomass_fit$data))
    ref <- biomass_fit$data %>%
      dplyr::select(dplyr::all_of(c(join_cols, ground_area))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
      dplyr::summarise(ground_area_use = mean(.data[[ground_area]], na.rm = TRUE), .groups = "drop")

    out <- dplyr::left_join(out, ref, by = intersect(gcols, names(ref)))
  } else if ("ground_area_m2" %in% names(biomass_fit$data)) {
    join_cols <- intersect(gcols, names(biomass_fit$data))
    ref <- biomass_fit$data %>%
      dplyr::select(dplyr::all_of(c(join_cols, "ground_area_m2"))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
      dplyr::summarise(ground_area_use = mean(.data$ground_area_m2, na.rm = TRUE), .groups = "drop")

    out <- dplyr::left_join(out, ref, by = intersect(gcols, names(ref)))
  } else {
    out$ground_area_use <- NA_real_
  }

  if (length(gcols) > 0) {
    out <- out %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
      dplyr::arrange(.data$time, .by_group = TRUE) %>%
      dplyr::mutate(logW = .safe_log(.data$W, epsilon = epsilon, name = "fitted biomass")) %>%
      dplyr::mutate(rgr_inst = .numeric_derivative(.data$time, .data$logW, central = central)) %>%
      dplyr::ungroup()
  } else {
    out <- out %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(logW = .safe_log(.data$W, epsilon = epsilon, name = "fitted biomass")) %>%
      dplyr::mutate(rgr_inst = .numeric_derivative(.data$time, .data$logW, central = central))
  }

  out2 <- out %>%
    dplyr::mutate(
      agr_inst = .data$dWdt,
      algr_inst = .data$dLAdt,
      nar_inst = .safe_div(.data$agr_inst, .data$LA),
      lar_inst = .safe_div(.data$LA, .data$W),
      sla_inst = .safe_div(.data$LA, .data$leaf_mass),
      cgr_inst = .safe_div(.data$agr_inst, .data$ground_area_use),
      lai_inst = .safe_div((.data$LA / 10000), .data$ground_area_use),
      lmr_inst = .safe_div(.data$leaf_mass, .data$W)
    )

  if (length(gcols) > 0) {
    out2 <- out2 %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
      dplyr::arrange(.data$time, .by_group = TRUE) %>%
      dplyr::mutate(
        logLA = .safe_log(.data$LA, epsilon = epsilon, name = "fitted leaf area"),
        rlgr_inst = .numeric_derivative(.data$time, .data$logLA, central = central)
      ) %>%
      dplyr::ungroup()
  } else {
    out2 <- out2 %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(
        logLA = .safe_log(.data$LA, epsilon = epsilon, name = "fitted leaf area"),
        rlgr_inst = .numeric_derivative(.data$time, .data$logLA, central = central)
      )
  }

  keep_cols <- c(
    intersect(gcols, names(out2)),
    "time", "W", "LA", "leaf_mass",
    "agr_inst", "rgr_inst", "algr_inst", "rlgr_inst",
    "nar_inst", "lar_inst", "sla_inst", "lai_inst", "lmr_inst", "cgr_inst"
  )

  out2 %>% dplyr::select(dplyr::all_of(intersect(keep_cols, names(out2))))
}

