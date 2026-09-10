# Internal helpers ----------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.default_growth_cols <- function() {
  c(
    "plant_id", "plot_id", "block", "treatment", "genotype", "date", "time",
    "days_after_sowing", "total_biomass_g", "leaf_biomass_g", "stem_biomass_g",
    "root_biomass_g", "reproductive_biomass_g", "leaf_area_cm2", "ground_area_m2"
  )
}

.resolve_mapping <- function(data, mapping = list()) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  defaults <- list(
    plant_id = "plant_id",
    plot_id = "plot_id",
    block = "block",
    treatment = "treatment",
    genotype = "genotype",
    date = "date",
    time = "time",
    days_after_sowing = "days_after_sowing",
    total_biomass_g = "total_biomass_g",
    leaf_biomass_g = "leaf_biomass_g",
    stem_biomass_g = "stem_biomass_g",
    root_biomass_g = "root_biomass_g",
    reproductive_biomass_g = "reproductive_biomass_g",
    leaf_area_cm2 = "leaf_area_cm2",
    ground_area_m2 = "ground_area_m2"
  )

  for (nm in names(mapping)) {
    if (!nm %in% names(defaults)) {
      rlang::abort(paste0("Unknown mapping field: `", nm, "`."))
    }
    defaults[[nm]] <- mapping[[nm]]
  }

  defaults
}

.rename_by_mapping <- function(data, mapping) {
  inverse <- stats::setNames(names(mapping), unlist(mapping))
  present <- names(inverse)[names(inverse) %in% names(data)]

  if (length(present) == 0) {
    return(tibble::as_tibble(data))
  }

  dplyr::rename(tibble::as_tibble(data), !!!inverse[present])
}

.coerce_numeric_column <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.numeric(x))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }

  x_chr <- trimws(x)
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null", "N/A")] <- NA_character_

  normalize_token <- function(token) {
    if (is.na(token)) {
      return(NA_character_)
    }

    tok <- gsub("\\s+", "", token)
    if (!nzchar(tok)) {
      return(NA_character_)
    }

    has_comma <- grepl(",", tok, fixed = TRUE)
    has_dot <- grepl(".", tok, fixed = TRUE)

    if (has_comma && has_dot) {
      comma_pos <- max(gregexpr(",", tok, fixed = TRUE)[[1]])
      dot_pos <- max(gregexpr(".", tok, fixed = TRUE)[[1]])

      if (comma_pos > dot_pos) {
        tok <- gsub(".", "", tok, fixed = TRUE)
        tok <- sub(",", ".", tok, fixed = TRUE)
      } else {
        tok <- gsub(",", "", tok, fixed = TRUE)
      }
    } else if (has_comma && !has_dot) {
      comma_n <- lengths(regmatches(tok, gregexpr(",", tok, fixed = TRUE)))
      if (comma_n > 1) {
        tok <- gsub(",", "", tok, fixed = TRUE)
      } else {
        tok <- sub(",", ".", tok, fixed = TRUE)
      }
    }

    tok
  }

  normalized <- vapply(x_chr, normalize_token, FUN.VALUE = character(1))
  suppressWarnings(as.numeric(normalized))
}

.required_cols_min <- function() {
  c("time", "total_biomass_g", "leaf_area_cm2")
}

.validate_cols <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing, collapse = ", "),
        ". Use `prepare_growth_data()` or provide column mapping."
      )
    )
  }
}

.safe_log <- function(x, epsilon = NULL, name = "value") {
  if (is.null(epsilon)) {
    if (any(x <= 0, na.rm = TRUE)) {
      rlang::abort(
        paste0(
          "`", name, "` has values <= 0 and cannot be log-transformed.",
          " Pass a positive `epsilon` if you explicitly want an offset."
        )
      )
    }
    return(log(x))
  }

  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon <= 0) {
    rlang::abort("`epsilon` must be a single positive numeric value.")
  }

  log(x + epsilon)
}

.safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.default_group_cols <- function(data, candidates = c("treatment", "genotype", "block", "plot_id", "plant_id")) {
  intersect(candidates, names(data))
}

.make_group_key <- function(data, group_cols) {
  if (length(group_cols) == 0) {
    return(rep("all", nrow(data)))
  }

  apply(data[, group_cols, drop = FALSE], 1, function(x) {
    paste(x, collapse = "::")
  })
}

.summarise_scalar <- function(x, fun = mean, na_rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  fun(x, na.rm = na_rm)
}

.central_difference <- function(x, y) {
  n <- length(x)
  out <- rep(NA_real_, n)

  if (n < 2) {
    return(out)
  }

  if (n == 2) {
    slope <- (y[2] - y[1]) / (x[2] - x[1])
    return(c(slope, slope))
  }

  out[1] <- (y[2] - y[1]) / (x[2] - x[1])
  out[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])

  for (i in 2:(n - 1)) {
    out[i] <- (y[i + 1] - y[i - 1]) / (x[i + 1] - x[i - 1])
  }

  out
}

.forward_difference <- function(x, y) {
  n <- length(x)
  out <- rep(NA_real_, n)

  if (n < 2) {
    return(out)
  }

  for (i in 1:(n - 1)) {
    out[i] <- (y[i + 1] - y[i]) / (x[i + 1] - x[i])
  }

  out[n] <- out[n - 1]
  out
}

.numeric_derivative <- function(x, y, central = TRUE) {
  ord <- order(x)
  x_ord <- x[ord]
  y_ord <- y[ord]

  d_ord <- if (central) {
    .central_difference(x_ord, y_ord)
  } else {
    .forward_difference(x_ord, y_ord)
  }

  d <- rep(NA_real_, length(d_ord))
  d[ord] <- d_ord
  d
}

.boot_ci <- function(estimates, conf_level = 0.95) {
  alpha <- (1 - conf_level) / 2
  c(
    conf.low = stats::quantile(estimates, probs = alpha, na.rm = TRUE, names = FALSE),
    conf.high = stats::quantile(estimates, probs = 1 - alpha, na.rm = TRUE, names = FALSE)
  )
}

.assert_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1 || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    rlang::abort("`conf_level` must be a single numeric value between 0 and 1.")
  }
}

.assert_positive_integer <- function(x, arg = "times") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x %% 1 != 0) {
    rlang::abort(paste0("`", arg, "` must be a positive integer."))
  }
}
