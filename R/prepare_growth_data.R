#' Prepare and Standardize Plant Growth Data
#'
#' Standardize input datasets to the `phytogrowR` column convention, convert time
#' to numeric scale, build grouping identifiers, and optionally calculate total
#' biomass from biomass parts.
#'
#' @param data A data frame or tibble containing growth observations.
#' @param mapping Optional named list mapping original names to standard names.
#' @param time_origin Optional date used as day zero when `date` is available.
#' If `NULL`, the minimum date in the dataset is used.
#' @param group_cols Columns used to create `group_id`.
#' @param derive_total Logical; if `TRUE`, computes `total_biomass_g` from parts
#' when total biomass is missing.
#' @param quiet Logical; if `FALSE`, emits informative messages.
#'
#' @details
#' The standard output uses columns such as `time`, `total_biomass_g`,
#' `leaf_biomass_g`, `leaf_area_cm2`, and `ground_area_m2`. Missing standard
#' columns are created with `NA` so downstream functions remain predictable.
#'
#' @return A standardized tibble with a `group_id` column.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' names(dat)
#' @export
prepare_growth_data <- function(
    data,
    mapping = list(),
    time_origin = NULL,
    group_cols = c("treatment", "genotype", "block", "plot_id", "plant_id"),
    derive_total = TRUE,
    quiet = TRUE
) {
  map <- .resolve_mapping(data, mapping)
  dat <- .rename_by_mapping(data, map)

  standard_cols <- .default_growth_cols()

  if ("date" %in% names(dat) && !inherits(dat$date, "Date")) {
    parsed <- as.Date(dat$date)
    if (!all(is.na(parsed)) || all(is.na(dat$date))) {
      dat$date <- parsed
    }
  }

  numeric_growth_cols <- intersect(
    c(
      "time", "days_after_sowing",
      "total_biomass_g", "leaf_biomass_g", "stem_biomass_g",
      "root_biomass_g", "reproductive_biomass_g",
      "leaf_area_cm2", "ground_area_m2"
    ),
    names(dat)
  )

  for (nm in numeric_growth_cols) {
    if (!is.numeric(dat[[nm]])) {
      dat[[nm]] <- .coerce_numeric_column(dat[[nm]])
    }
  }

  if (!"time" %in% names(dat) || all(is.na(dat$time))) {
    if ("days_after_sowing" %in% names(dat)) {
      dat$time <- as.numeric(dat$days_after_sowing)
    } else if ("date" %in% names(dat) && inherits(dat$date, "Date")) {
      origin <- time_origin %||% min(dat$date, na.rm = TRUE)
      if (!inherits(origin, "Date")) {
        origin <- as.Date(origin)
      }
      dat$time <- as.numeric(dat$date - origin)
    }
  }

  if ("time" %in% names(dat) && !is.numeric(dat$time)) {
    dat$time <- suppressWarnings(as.numeric(dat$time))
  }

  part_cols <- intersect(
    c("leaf_biomass_g", "stem_biomass_g", "root_biomass_g", "reproductive_biomass_g"),
    names(dat)
  )

  if (derive_total) {
    if (!"total_biomass_g" %in% names(dat)) {
      if (length(part_cols) >= 2) {
        dat$total_biomass_g <- rowSums(dat[, part_cols, drop = FALSE], na.rm = TRUE)
      }
    } else {
      idx <- is.na(dat$total_biomass_g) | dat$total_biomass_g <= 0
      if (length(part_cols) >= 2 && any(idx, na.rm = TRUE)) {
        part_sum <- rowSums(dat[idx, part_cols, drop = FALSE], na.rm = TRUE)
        dat$total_biomass_g[idx] <- part_sum
      }
    }
  }

  missing_standard <- setdiff(standard_cols, names(dat))
  if (length(missing_standard) > 0) {
    for (nm in missing_standard) {
      dat[[nm]] <- NA
    }
  }

  gcols <- intersect(group_cols, names(dat))
  dat <- tibble::as_tibble(dat)
  dat$group_id <- .make_group_key(dat, gcols)

  sort_cols <- c(gcols, "time")
  sort_cols <- intersect(sort_cols, names(dat))
  if (length(sort_cols) > 0) {
    dat <- dplyr::arrange(dat, dplyr::across(dplyr::all_of(sort_cols)))
  }

  if (!quiet) {
    message("Data prepared with ", nrow(dat), " rows and ", ncol(dat), " columns.")
  }

  dat
}
