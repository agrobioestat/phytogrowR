# Guards for packages listed in Suggests ------------------------------------
#
# CRAN policy: code must not fail if a package listed in `Suggests` is absent.
# Everything that touches 'shiny', 'DT', 'readr', 'rmarkdown', 'knitr' or
# 'broom' must therefore go through one of the two helpers below.

#' Is an optional package available?
#'
#' Thin wrapper around [requireNamespace()] used to branch on optional
#' functionality without ever attaching the package.
#'
#' @param pkg Character scalar with the package name.
#'
#' @return `TRUE` when the namespace can be loaded, `FALSE` otherwise.
#' @noRd
.has_pkg <- function(pkg) {
  isTRUE(requireNamespace(pkg, quietly = TRUE))
}

#' Require optional packages or abort with an actionable message
#'
#' Used at the top of every exported function whose work is impossible without
#' a suggested dependency. It fails early, names every missing package at once
#' (instead of one error per `library()` call), and prints the exact
#' `install.packages()` line the user needs.
#'
#' @param ... Character scalars with package names.
#' @param use Short description of what the packages are needed for.
#'
#' @return Invisibly `TRUE`; called for its side effect of aborting.
#' @noRd
.check_suggested <- function(..., use = NULL) {
  pkgs <- unique(c(...))
  missing <- pkgs[!vapply(pkgs, .has_pkg, logical(1))]

  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  rlang::abort(
    c(
      paste0(
        "The following optional package",
        if (length(missing) > 1) "s are" else " is",
        " required",
        if (is.null(use)) "" else paste0(" for ", use),
        ": ",
        paste0("'", missing, "'", collapse = ", "),
        "."
      ),
      i = paste0(
        "Install with: install.packages(c(",
        paste0('"', missing, '"', collapse = ", "),
        "))"
      )
    ),
    class = "phytogrowR_missing_suggests"
  )
}

#' Empty one-row tidy skeleton
#'
#' @return A one-row tibble with the standard `broom` tidy columns, all `NA`.
#' @noRd
.empty_tidy_row <- function() {
  tibble::tibble(
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_
  )
}

#' Coefficient-only tidy fallback
#'
#' Reproduces the shape of a `broom::tidy()` result using only base `stats`, so
#' that S3 `tidy()` methods keep a stable contract when 'broom' is not
#' installed or does not know how to tidy a given model object.
#'
#' @param mod A fitted model object.
#'
#' @return A tibble with `term`, `estimate`, `std.error`, `statistic` and
#' `p.value`.
#' @noRd
.coef_tidy <- function(mod) {
  cf <- tryCatch(stats::coef(mod), error = function(e) NULL)

  if (is.null(cf) || length(cf) == 0) {
    return(.empty_tidy_row())
  }

  nms <- names(cf)
  if (is.null(nms)) {
    nms <- paste0("term", seq_along(cf))
  }

  tibble::tibble(
    term = nms,
    estimate = as.numeric(cf),
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_
  )
}
