#' Calculate Biomass Partitioning and Allocation Ratios
#'
#' Compute biomass allocation fractions and biologically relevant ratios over time.
#'
#' @param data Standardized growth data.
#' @param group_cols Grouping columns for summary output.
#' @param summarise Logical; if `TRUE`, returns grouped means, otherwise returns
#' row-level partition values.
#' @param na_rm Logical passed to summary functions.
#'
#' @return A tibble containing biomass proportions and ratios.
#'
#' @examples
#' dat <- prepare_growth_data(growth_wide_example)
#' part <- biomass_partition(dat, group_cols = c("treatment", "time"))
#' utils::head(part)
#' @export
biomass_partition <- function(
    data,
    group_cols = c("treatment", "genotype", "time"),
    summarise = TRUE,
    na_rm = TRUE
) {
  dat <- prepare_growth_data(data, quiet = TRUE)

  part_cols <- intersect(
    c("leaf_biomass_g", "stem_biomass_g", "root_biomass_g", "reproductive_biomass_g"),
    names(dat)
  )

  if (length(part_cols) < 3) {
    rlang::abort("At least leaf, stem, and root biomass columns are required for partitioning.")
  }

  if (!"total_biomass_g" %in% names(dat)) {
    dat$total_biomass_g <- rowSums(dat[, part_cols, drop = FALSE], na.rm = TRUE)
  }

  out <- dat %>%
    dplyr::mutate(
      total_biomass_g = dplyr::if_else(
        .data$total_biomass_g <= 0,
        NA_real_,
        .data$total_biomass_g
      ),
      shoot_biomass_g = .data$leaf_biomass_g + .data$stem_biomass_g + dplyr::coalesce(.data$reproductive_biomass_g, 0),
      prop_leaf = .data$leaf_biomass_g / .data$total_biomass_g,
      prop_stem = .data$stem_biomass_g / .data$total_biomass_g,
      prop_root = .data$root_biomass_g / .data$total_biomass_g,
      prop_reproductive = dplyr::coalesce(.data$reproductive_biomass_g, 0) / .data$total_biomass_g,
      root_shoot_ratio = .data$root_biomass_g / .data$shoot_biomass_g,
      leaf_stem_ratio = .data$leaf_biomass_g / .data$stem_biomass_g
    )

  if (!summarise) {
    return(out)
  }

  gcols <- intersect(group_cols, names(out))
  if (length(gcols) == 0) {
    gcols <- "time"
  }

  out %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
    dplyr::summarise(
      prop_leaf = mean(.data$prop_leaf, na.rm = na_rm),
      prop_stem = mean(.data$prop_stem, na.rm = na_rm),
      prop_root = mean(.data$prop_root, na.rm = na_rm),
      prop_reproductive = mean(.data$prop_reproductive, na.rm = na_rm),
      root_shoot_ratio = mean(.data$root_shoot_ratio, na.rm = na_rm),
      leaf_stem_ratio = mean(.data$leaf_stem_ratio, na.rm = na_rm),
      n = dplyr::n(),
      .groups = "drop"
    )
}
