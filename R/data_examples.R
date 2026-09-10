# Simulated datasets --------------------------------------------------------

.simulate_phytogrow_data <- function() {
  blocks <- paste0("B", 1:3)
  treatments <- c("Control", "NitrogenPlus", "WaterDeficit")
  genotypes <- c("G1", "G2")
  times <- c(15, 30, 45, 60, 75)
  plants_per_plot <- 6

  base_design <- tidyr::crossing(
    block = blocks,
    treatment = treatments,
    genotype = genotypes
  ) %>%
    dplyr::mutate(
      plot_id = sprintf("P%02d", dplyr::row_number()),
      ground_area_m2 = 0.25
    )

  wide <- tidyr::crossing(
    base_design,
    plant_seq = seq_len(plants_per_plot),
    time = times
  ) %>%
    dplyr::mutate(
      plant_id = paste0(plot_id, "_", sprintf("%02d", plant_seq)),
      date = as.Date("2024-01-01") + time,
      days_after_sowing = time
    )

  block_effects <- c(B1 = -0.06, B2 = 0, B3 = 0.055)
  plant_levels <- dplyr::distinct(wide, .data$plant_id) %>%
    dplyr::mutate(
      plant_rank = dplyr::row_number(),
      plant_effect = ((.data$plant_rank %% 11) - 5) / 100
    )

  wide <- wide %>%
    dplyr::left_join(plant_levels, by = "plant_id") %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      noise_w = sin(.data$row_id * 1.71) * 0.45,
      noise_total = cos(.data$row_id * 2.11) * 0.02,
      noise_area = sin(.data$row_id * 0.97) * 9,
      k = dplyr::case_when(
        .data$treatment == "Control" ~ 22,
        .data$treatment == "NitrogenPlus" ~ 29,
        TRUE ~ 16
      ),
      r = dplyr::case_when(
        .data$treatment == "Control" ~ 0.085,
        .data$treatment == "NitrogenPlus" ~ 0.10,
        TRUE ~ 0.070
      ) + dplyr::if_else(.data$genotype == "G2", 0.007, 0),
      total_true = .data$k / (1 + exp(-.data$r * (.data$time - 45))),
      total_true = .data$total_true * exp(block_effects[.data$block] + .data$plant_effect),
      total_true = pmax(0.08, .data$total_true + .data$noise_w),
      p_leaf = pmax(0.16, 0.52 - 0.003 * .data$time + dplyr::if_else(.data$treatment == "NitrogenPlus", 0.03, 0)),
      p_stem = pmax(0.18, 0.19 + 0.0018 * .data$time),
      p_root = pmax(
        0.14,
        0.24 + dplyr::if_else(.data$treatment == "WaterDeficit", 0.05, 0) -
          dplyr::if_else(.data$treatment == "NitrogenPlus", 0.025, 0)
      ),
      p_reproductive = pmax(0.02, 1 - (.data$p_leaf + .data$p_stem + .data$p_root)),
      p_sum = .data$p_leaf + .data$p_stem + .data$p_root + .data$p_reproductive,
      p_leaf = .data$p_leaf / .data$p_sum,
      p_stem = .data$p_stem / .data$p_sum,
      p_root = .data$p_root / .data$p_sum,
      p_reproductive = .data$p_reproductive / .data$p_sum,
      leaf_biomass_g = .data$total_true * .data$p_leaf,
      stem_biomass_g = .data$total_true * .data$p_stem,
      root_biomass_g = .data$total_true * .data$p_root,
      reproductive_biomass_g = .data$total_true * .data$p_reproductive,
      total_biomass_g = pmax(0.05, .data$total_true * (1 + .data$noise_total)),
      sla_cm2_g = pmax(75, 220 - 1.15 * .data$time + dplyr::if_else(.data$treatment == "NitrogenPlus", 12, 0)),
      leaf_area_cm2 = pmax(6, .data$leaf_biomass_g * .data$sla_cm2_g + .data$noise_area)
    ) %>%
    dplyr::select(
      .data$plant_id,
      .data$plot_id,
      .data$block,
      .data$treatment,
      .data$genotype,
      .data$date,
      .data$days_after_sowing,
      .data$time,
      .data$total_biomass_g,
      .data$leaf_biomass_g,
      .data$stem_biomass_g,
      .data$root_biomass_g,
      .data$reproductive_biomass_g,
      .data$leaf_area_cm2,
      .data$ground_area_m2
    )

  leaf_miss <- which((seq_len(nrow(wide)) %% 23) == 0)
  root_miss <- which((seq_len(nrow(wide)) %% 31) == 0)
  wide$leaf_area_cm2[leaf_miss] <- NA_real_
  wide$root_biomass_g[root_miss] <- NA_real_

  long <- wide %>%
    tidyr::pivot_longer(
      cols = c(
        "total_biomass_g", "leaf_biomass_g", "stem_biomass_g", "root_biomass_g",
        "reproductive_biomass_g", "leaf_area_cm2", "ground_area_m2"
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      unit = dplyr::case_when(
        .data$variable == "leaf_area_cm2" ~ "cm2",
        .data$variable == "ground_area_m2" ~ "m2",
        TRUE ~ "g"
      )
    ) %>%
    dplyr::select(
      .data$plant_id,
      .data$plot_id,
      .data$block,
      .data$treatment,
      .data$genotype,
      .data$time,
      .data$variable,
      .data$value,
      .data$unit
    )

  harvest <- wide %>%
    dplyr::group_by(.data$plot_id, .data$block, .data$treatment, .data$genotype, .data$time, .data$ground_area_m2) %>%
    dplyr::summarise(
      n_plants = sum(!is.na(.data$total_biomass_g)),
      total_biomass_g = mean(.data$total_biomass_g, na.rm = TRUE),
      leaf_biomass_g = mean(.data$leaf_biomass_g, na.rm = TRUE),
      stem_biomass_g = mean(.data$stem_biomass_g, na.rm = TRUE),
      root_biomass_g = mean(.data$root_biomass_g, na.rm = TRUE),
      leaf_area_cm2 = mean(.data$leaf_area_cm2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$plot_id, .data$time) %>%
    dplyr::group_by(.data$plot_id) %>%
    dplyr::mutate(harvest = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$plot_id,
      .data$block,
      .data$treatment,
      .data$genotype,
      .data$harvest,
      .data$time,
      .data$n_plants,
      .data$total_biomass_g,
      .data$leaf_biomass_g,
      .data$stem_biomass_g,
      .data$root_biomass_g,
      .data$leaf_area_cm2,
      .data$ground_area_m2
    )

  list(wide = wide, long = long, harvest = harvest)
}

.sim_data <- .simulate_phytogrow_data()

#' Simulated Plant Growth Data (Wide Format)
#'
#' Realistic simulated dataset with repeated measurements, treatment contrasts,
#' genotype effects, block effects, deterministic noise, and missing observations.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{plant_id}{Plant identifier.}
#'   \item{plot_id}{Plot identifier.}
#'   \item{block}{Experimental block.}
#'   \item{treatment}{Treatment level.}
#'   \item{genotype}{Genotype level.}
#'   \item{date}{Evaluation date.}
#'   \item{days_after_sowing}{Days after sowing.}
#'   \item{time}{Numeric time variable (days).}
#'   \item{total_biomass_g}{Total dry biomass (g plant-1).}
#'   \item{leaf_biomass_g}{Leaf dry biomass (g plant-1).}
#'   \item{stem_biomass_g}{Stem dry biomass (g plant-1).}
#'   \item{root_biomass_g}{Root dry biomass (g plant-1).}
#'   \item{reproductive_biomass_g}{Reproductive dry biomass (g plant-1).}
#'   \item{leaf_area_cm2}{Leaf area (cm2 plant-1).}
#'   \item{ground_area_m2}{Ground area (m2).}
#' }
#' @source Simulated in-package data for examples and tests.
#' @examples
#' dplyr::glimpse(growth_wide_example)
#' @export
growth_wide_example <- .sim_data$wide

#' Simulated Plant Growth Data (Long Format)
#'
#' Long-format version of [growth_wide_example] with variable and unit columns.
#'
#' @format A tibble with columns `plant_id`, `plot_id`, `block`, `treatment`,
#' `genotype`, `time`, `variable`, `value`, and `unit`.
#' @source Simulated in-package data for examples and tests.
#' @examples
#' dplyr::count(growth_long_example, variable)
#' @export
growth_long_example <- .sim_data$long

#' Simulated Sequential Harvest Data
#'
#' Plot-level sequential destructive harvest means for growth analysis workflows.
#'
#' @format A tibble with columns:
#' `plot_id`, `block`, `treatment`, `genotype`, `harvest`, `time`, `n_plants`,
#' `total_biomass_g`, `leaf_biomass_g`, `stem_biomass_g`, `root_biomass_g`,
#' `leaf_area_cm2`, and `ground_area_m2`.
#' @source Simulated in-package data for examples and tests.
#' @examples
#' dplyr::arrange(harvest_example, plot_id, time) |> utils::head()
#' @export
harvest_example <- .sim_data$harvest

#' Simulated Two-Harvest Classical Dataset (Hunt-Style)
#'
#' Compact simulated dataset for classical interval growth analysis between two
#' successive harvests, including treatments, genotypes, blocks, and controlled
#' missing values.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{plant_id}{Plant identifier.}
#'   \item{harvest}{Harvest index (1 or 2).}
#'   \item{time}{Harvest time (days).}
#'   \item{treatment}{Treatment level.}
#'   \item{genotype}{Genotype level.}
#'   \item{block}{Block level.}
#'   \item{plot_id}{Plot identifier.}
#'   \item{root_biomass_g}{Root dry biomass (g plant-1).}
#'   \item{leaf_biomass_g}{Leaf dry biomass (g plant-1).}
#'   \item{stem_biomass_g}{Stem dry biomass (g plant-1).}
#'   \item{reproductive_biomass_g}{Reproductive dry biomass (g plant-1).}
#'   \item{shoot_biomass_g}{Shoot dry biomass (g plant-1).}
#'   \item{total_biomass_g}{Total dry biomass (g plant-1).}
#'   \item{leaf_area_cm2}{Leaf area (cm2 plant-1).}
#' }
#' @source Simulated in-package data for examples and tests.
#' @examples
#' dplyr::count(hunt_classical_example, treatment, harvest)
#' @export
hunt_classical_example <- local({
  set.seed(2026)

  blocks <- paste0("B", 1:3)
  treatments <- c("Control", "NPlus")
  genotypes <- c("G1", "G2")

  plots <- tidyr::crossing(
    block = blocks,
    treatment = treatments,
    genotype = genotypes
  ) %>%
    dplyr::mutate(plot_id = paste0("PL", sprintf("%02d", dplyr::row_number())))

  plants_per_plot <- 3
  harvest_map <- tibble::tibble(harvest = c(1L, 2L), time = c(30, 45))

  dat <- tidyr::crossing(
    plots,
    plant_seq = seq_len(plants_per_plot),
    harvest_map
  ) %>%
    dplyr::mutate(
      plant_id = paste0(plot_id, "_", sprintf("%02d", plant_seq), "_H", harvest),
      trt_eff = dplyr::if_else(treatment == "NPlus", 1.12, 1.00),
      geno_eff = dplyr::if_else(genotype == "G2", 1.06, 1.00),
      block_eff = dplyr::case_when(
        block == "B1" ~ 0.96,
        block == "B2" ~ 1.00,
        TRUE ~ 1.05
      ),
      base_total = dplyr::if_else(harvest == 1L, 2.8, 4.3),
      total_biomass_g = pmax(
        0.2,
        base_total * trt_eff * geno_eff * block_eff +
          rnorm(dplyr::n(), sd = 0.23)
      ),
      lwf = pmin(0.68, pmax(0.32, 0.48 - 0.02 * (harvest - 1) + rnorm(dplyr::n(), sd = 0.03))),
      swf = pmin(0.45, pmax(0.16, 0.29 + 0.02 * (harvest - 1) + rnorm(dplyr::n(), sd = 0.02))),
      rwf = pmin(0.42, pmax(0.16, 1 - lwf - swf - 0.04 + rnorm(dplyr::n(), sd = 0.015))),
      repf = pmax(0.01, 1 - lwf - swf - rwf),
      part_sum = lwf + swf + rwf + repf,
      lwf = lwf / part_sum,
      swf = swf / part_sum,
      rwf = rwf / part_sum,
      repf = repf / part_sum,
      leaf_biomass_g = total_biomass_g * lwf,
      stem_biomass_g = total_biomass_g * swf,
      root_biomass_g = total_biomass_g * rwf,
      reproductive_biomass_g = total_biomass_g * repf,
      shoot_biomass_g = leaf_biomass_g + stem_biomass_g + reproductive_biomass_g,
      sla = pmax(85, 220 - 1.6 * time + dplyr::if_else(treatment == "NPlus", 8, 0) + rnorm(dplyr::n(), sd = 6)),
      leaf_area_cm2 = pmax(10, leaf_biomass_g * sla + rnorm(dplyr::n(), sd = 8))
    ) %>%
    dplyr::select(
      .data$plant_id,
      .data$harvest,
      .data$time,
      .data$treatment,
      .data$genotype,
      .data$block,
      .data$plot_id,
      .data$root_biomass_g,
      .data$leaf_biomass_g,
      .data$stem_biomass_g,
      .data$reproductive_biomass_g,
      .data$shoot_biomass_g,
      .data$total_biomass_g,
      .data$leaf_area_cm2
    )

  dat$leaf_area_cm2[5] <- NA_real_
  dat$root_biomass_g[17] <- NA_real_
  dat$reproductive_biomass_g[9] <- NA_real_
  dat$shoot_biomass_g <- dat$leaf_biomass_g + dat$stem_biomass_g + dat$reproductive_biomass_g

  dat
})

rm(.sim_data)
