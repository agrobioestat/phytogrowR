#' Run the phytogrowR Shiny App
#'
#' Launches the interactive app shipped in `inst/shiny/phytogrowRApp`.
#'
#' @details
#' The app and its widgets rely on packages listed in `Suggests`
#' (`shiny`, `bslib`, `DT`, `readr`) rather than `Imports`, so the analytical
#' core of `phytogrowR` installs without them. The availability of every
#' optional package is verified here, before the app directory is handed to
#' [shiny::runApp()].
#'
#' The interface is organized as a Bootstrap 5 dashboard: a collapsible
#' sidebar for the controls, a row of summary boxes for the design, and one
#' card-based tab per analysis module. Five in-package example datasets are
#' available from the sidebar, so every module can be exercised without any
#' external file.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the app object. Called for its side effect of
#' starting the interactive session.
#'
#' @examples
#' \dontrun{
#' run_phytogrow_app()
#' }
#' @export
run_phytogrow_app <- function(...) {
  .check_suggested(
    "shiny", "bslib", "DT", "readr",
    use = "the interactive phytogrowR application"
  )

  app_dir <- system.file("shiny", "phytogrowRApp", package = "phytogrowR")

  if (!nzchar(app_dir)) {
    rlang::abort("Shiny app directory not found. Reinstall the package.")
  }

  shiny::runApp(appDir = app_dir, ...)
}
