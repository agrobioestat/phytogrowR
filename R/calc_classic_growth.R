#' Calculate Classical Plant Growth Indices
#'
#' Computes interval-based growth metrics between consecutive time points.
#'
#' @param data A data frame in standardized format (see [prepare_growth_data()]).
#' @param group_cols Grouping columns used to define independent trajectories.
#' @param time_col Name of the numeric time column.
#' @param biomass_col Name of total biomass column.
#' @param leaf_area_col Name of leaf area column.
#' @param leaf_mass_col Name of leaf biomass column.
#' @param stem_mass_col Name of stem biomass column.
#' @param root_mass_col Name of root biomass column.
#' @param ground_area_col Name of ground area column.
#' @param epsilon Optional positive numeric offset used in log transforms.
#' @param keep_intermediate Logical; keep intermediate columns when `TRUE`.
#'
#' @details
#' Implemented formulas (between consecutive times `t1` and `t2`):
#'
#' - `RGR = (log(W2) - log(W1)) / (t2 - t1)`
#' - `AGR = (W2 - W1) / (t2 - t1)`
#' - `NAR = ((W2 - W1)/(t2 - t1)) * ((log(LA2) - log(LA1))/(LA2 - LA1))`
#' - `LAR = LA / W` (reported as interval mean of `t1` and `t2`)
#' - `SLA = LA / leaf_mass` (at `t1`)
#' - `LMR = leaf_mass / W`
#' - `SMR = stem_mass / W`
#' - `RMR = root_mass / W`
#' - `CGR = (W2 - W1) / ((t2 - t1) * ground_area)`
#' - `ALGR = (LA2 - LA1) / (t2 - t1)` (absolute leaf area growth rate)
#' - `RLGR = (log(LA2) - log(LA1)) / (t2 - t1)` (relative leaf area growth rate)
#' - `LAI = ((LA/10000) / ground_area)` (interval mean leaf area index)
#' - `LAD = ((LA1 + LA2)/2) * (t2 - t1)` (leaf area duration in `cm2 day`)
#'
#' Rates are interval averages, not instantaneous derivatives.
#' Classical concepts follow Hunt (1990), Benincasa (2003), and Hunt et al.
#' (2002) for interval-based growth analysis.
#'
#' @return A tibble with one row per interval and grouping unit.
#'
#' @references
#' Hunt, R. (1990). *Basic Growth Analysis*. Unwin Hyman, London.
#'
#' Benincasa, M. M. P. (2003). *Plant Growth Analysis: Basic Concepts*.
#' FUNEP, Jaboticabal.
#'
#' Hunt, R., Causton, D. R., Shipley, B., and Askew, A. P. (2002).
#' A modern tool for classical plant growth analysis.
#' *Annals of Botany*, 90(4), 485-488. doi:10.1093/aob/mcf214
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' classic <- calc_classic_growth(dat, group_cols = c("treatment", "plot_id", "plant_id"))
#' dplyr::glimpse(classic)
#' @export
calc_classic_growth <- function(
    data,
    group_cols = c("treatment", "genotype", "block", "plot_id", "plant_id"),
    time_col = "time",
    biomass_col = "total_biomass_g",
    leaf_area_col = "leaf_area_cm2",
    leaf_mass_col = "leaf_biomass_g",
    stem_mass_col = "stem_biomass_g",
    root_mass_col = "root_biomass_g",
    ground_area_col = "ground_area_m2",
    epsilon = NULL,
    keep_intermediate = FALSE
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  required <- c(time_col, biomass_col, leaf_area_col)
  .validate_cols(data, required)

  dat <- tibble::as_tibble(data) %>%
    dplyr::mutate(
      time = .data[[time_col]],
      W = .data[[biomass_col]],
      LA = .data[[leaf_area_col]]
    )

  dat$leaf_mass <- if (leaf_mass_col %in% names(dat)) dat[[leaf_mass_col]] else NA_real_
  dat$stem_mass <- if (stem_mass_col %in% names(dat)) dat[[stem_mass_col]] else NA_real_
  dat$root_mass <- if (root_mass_col %in% names(dat)) dat[[root_mass_col]] else NA_real_
  dat$ground_area <- if (ground_area_col %in% names(dat)) dat[[ground_area_col]] else NA_real_

  gcols <- intersect(group_cols, names(dat))

  # Prefer the dedicated two-harvest classical module when feasible.
  if (!keep_intermediate) {
    uniq_time <- sort(unique(dat$time[!is.na(dat$time)]))
    classical_groups <- setdiff(gcols, "plant_id")

    if (length(uniq_time) == 2L && length(classical_groups) > 0L) {
      counts <- dat %>%
        dplyr::filter(!is.na(.data$W)) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(classical_groups, "time")))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")

      group_ok <- counts %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(classical_groups))) %>%
        dplyr::summarise(
          n_time = dplyr::n(),
          min_n = min(.data$n, na.rm = TRUE),
          .groups = "drop"
        )

      if (nrow(group_ok) > 0 && all(group_ok$n_time == 2L & group_ok$min_n >= 2L)) {
        fit_classical <- tryCatch(
          classical_growth_interval(
            data = dat,
            time = time,
            total_biomass = W,
            leaf_biomass = leaf_mass,
            stem_biomass = stem_mass,
            root_biomass = root_mass,
            leaf_area = LA,
            group_by = classical_groups,
            ci_method = "classical",
            nar_label = "both",
            warn = FALSE
          ),
          error = function(e) NULL
        )

        if (!is.null(fit_classical) && nrow(fit_classical$results) > 0) {
          out <- fit_classical$results %>%
            dplyr::filter(.data$ci_source == "classical") %>%
            dplyr::select(dplyr::all_of(c(classical_groups, "index", "estimate"))) %>%
            tidyr::pivot_wider(names_from = .data$index, values_from = .data$estimate)

          info <- fit_classical$interval_info %>%
            dplyr::select(dplyr::all_of(c(classical_groups, "t1", "t2"))) %>%
            dplyr::rename(time_start = .data$t1, time_end = .data$t2)

          out <- dplyr::left_join(info, out, by = classical_groups)

          if (!"nar" %in% names(out) && "ulr" %in% names(out)) {
            out$nar <- out$ulr
          }
          if (!"lmr" %in% names(out) && "lwf" %in% names(out)) {
            out$lmr <- out$lwf
          }
          if (!"lwr" %in% names(out) && "lwf" %in% names(out)) {
            out$lwr <- out$lwf
          }

          for (nm in c("algr", "rlgr", "lai", "lad", "smr", "rmr", "swr", "cgr")) {
            if (!nm %in% names(out)) {
              out[[nm]] <- NA_real_
            }
          }

          keep_cols_classical <- c(
            classical_groups,
            "time_start", "time_end",
            "rgr", "agr", "nar",
            "algr", "rlgr",
            "lar", "lai", "lad",
            "sla", "lmr", "smr", "rmr", "lwr", "swr",
            "cgr"
          )

          return(out %>% dplyr::select(dplyr::all_of(intersect(keep_cols_classical, names(out)))))
        }
      }
    }
  }

  if (length(gcols) > 0) {
    dat <- dat %>% dplyr::group_by(dplyr::across(dplyr::all_of(gcols)))
  }

  dat <- dat %>%
    dplyr::arrange(.data$time, .by_group = length(gcols) > 0) %>%
    dplyr::mutate(
      time_next = dplyr::lead(.data$time),
      W_next = dplyr::lead(.data$W),
      LA_next = dplyr::lead(.data$LA),
      leaf_mass_next = dplyr::lead(.data$leaf_mass),
      ground_area_next = dplyr::lead(.data$ground_area),
      dt = .data$time_next - .data$time
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$dt), .data$dt > 0)

  if (nrow(dat) == 0) {
    return(tibble::tibble())
  }

  if (is.null(epsilon)) {
    bad_w <- sum(dat$W <= 0 | dat$W_next <= 0, na.rm = TRUE)
    bad_la <- sum(dat$LA <= 0 | dat$LA_next <= 0, na.rm = TRUE)

    if (bad_w > 0 || bad_la > 0) {
      rlang::abort(
        paste0(
          "Non-positive biomass or leaf area found in ", bad_w + bad_la,
          " interval(s). Use positive values or pass `epsilon` explicitly."
        )
      )
    }
  }

  log_w1 <- .safe_log(dat$W, epsilon = epsilon, name = "W1")
  log_w2 <- .safe_log(dat$W_next, epsilon = epsilon, name = "W2")
  log_la1 <- .safe_log(dat$LA, epsilon = epsilon, name = "LA1")
  log_la2 <- .safe_log(dat$LA_next, epsilon = epsilon, name = "LA2")

  delta_la <- dat$LA_next - dat$LA
  ga_use <- dplyr::coalesce(dat$ground_area, dat$ground_area_next)

  out <- dat %>%
    dplyr::mutate(
      time_start = .data$time,
      time_end = .data$time_next,
      rgr = (log_w2 - log_w1) / .data$dt,
      agr = (.data$W_next - .data$W) / .data$dt,
      algr = (.data$LA_next - .data$LA) / .data$dt,
      rlgr = (log_la2 - log_la1) / .data$dt,
      nar = dplyr::if_else(
        abs(delta_la) < 1e-12,
        NA_real_,
        .data$agr * ((log_la2 - log_la1) / delta_la)
      ),
      lar_t1 = .safe_div(.data$LA, .data$W),
      lar_t2 = .safe_div(.data$LA_next, .data$W_next),
      lar = (.data$lar_t1 + .data$lar_t2) / 2,
      sla = .safe_div(.data$LA, .data$leaf_mass),
      lmr = .safe_div(.data$leaf_mass, .data$W),
      smr = .safe_div(.data$stem_mass, .data$W),
      rmr = .safe_div(.data$root_mass, .data$W),
      cgr = .safe_div((.data$W_next - .data$W), (.data$dt * ga_use)),
      lai_t1 = .safe_div((.data$LA / 10000), ga_use),
      lai_t2 = .safe_div((.data$LA_next / 10000), ga_use),
      lai = (.data$lai_t1 + .data$lai_t2) / 2,
      lad = ((.data$LA + .data$LA_next) / 2) * .data$dt,
      lwr = .data$lmr,
      swr = .data$smr
    )

  keep_cols <- c(
    gcols,
    "time_start", "time_end",
    "rgr", "agr", "nar",
    "algr", "rlgr",
    "lar", "lai", "lad",
    "sla", "lmr", "smr", "rmr", "lwr", "swr",
    "cgr"
  )

  if (keep_intermediate) {
    keep_cols <- c(
      keep_cols,
      "W", "W_next", "LA", "LA_next", "dt", "lar_t1", "lar_t2", "lai_t1", "lai_t2", "ground_area"
    )
  }

  out %>% dplyr::select(dplyr::all_of(intersect(keep_cols, names(out))))
}

