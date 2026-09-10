#' Generate Automatic Growth Analysis Report
#'
#' Render an HTML, Word, or PDF report including data checks, fitted curves,
#' classical rates, instantaneous rates, treatment comparisons, and partitioning.
#'
#' @param data Standardized or raw growth data.
#' @param output_file Output file path.
#' @param output_format One of `"html_document"`, `"word_document"`,
#' or `"pdf_document"`.
#' @param title Report title.
#' @param group_var Grouping variable used in comparisons.
#' @param curve_method Method used in curve fitting.
#' @param quiet Logical passed to [rmarkdown::render()].
#'
#' @details
#' Report rendering depends on `rmarkdown` and `knitr`, which are listed in
#' `Suggests`. Their availability is checked before any file is written, so the
#' function fails early with an installation hint instead of half-way through
#' rendering.
#'
#' @return Path to generated report (invisibly).
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
#' \dontrun{
#' out <- growth_report(
#'   growth_wide_example,
#'   output_file = file.path(tempdir(), "phytogrowR_report.html")
#' )
#' }
#' @export
growth_report <- function(
    data,
    output_file = file.path(tempdir(), "phytogrowR_report.html"),
    output_format = c("html_document", "word_document", "pdf_document"),
    title = "phytogrowR - Plant Growth Analysis Report",
    group_var = "treatment",
    curve_method = "gam",
    quiet = TRUE
) {
  output_format <- match.arg(output_format)

  .check_suggested(
    "rmarkdown", "knitr",
    use = "rendering the automatic growth analysis report"
  )

  if (output_format == "pdf_document" && !.has_pkg("tinytex")) {
    if (!nzchar(Sys.which("pdflatex"))) {
      rlang::warn(
        "No LaTeX installation was detected; 'pdf_document' rendering may fail."
      )
    }
  }

  if (!is.character(output_file) || length(output_file) != 1) {
    rlang::abort("`output_file` must be a single file path.")
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  dat <- prepare_growth_data(data, quiet = TRUE)

  template <- c(
    "---",
    "title: \"`r params$title`\"",
    "output:",
    "  `r params$output_format`: default",
    "params:",
    "  data: !r NULL",
    "  group_var: \"treatment\"",
    "  curve_method: \"gam\"",
    "  title: \"phytogrowR Report\"",
    "  output_format: \"html_document\"",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(phytogrowR)",
    "library(dplyr)",
    "```",
    "",
    "## Data Summary",
    "",
    "```{r}",
    "dat <- prepare_growth_data(params$data, quiet = TRUE)",
    "dplyr::glimpse(dat)",
    "summary_tbl <- dat |> summarise(",
    "  n_rows = n(),",
    "  n_plants = dplyr::n_distinct(plant_id),",
    "  n_plots = dplyr::n_distinct(plot_id),",
    "  min_time = min(time, na.rm = TRUE),",
    "  max_time = max(time, na.rm = TRUE)",
    ")",
    "knitr::kable(summary_tbl)",
    "```",
    "",
    "## Data Quality Check",
    "",
    "```{r}",
    "qc <- check_growth_data(dat)",
    "knitr::kable(qc)",
    "```",
    "",
    "## Fitted Growth Curves",
    "",
    "```{r}",
    "fit_w <- fit_growth_curve(dat, response = 'total_biomass_g', method = params$curve_method, group_cols = c(params$group_var))",
    "plot_growth_curve(fit_w, facet_var = params$group_var)",
    "```",
    "",
    "## Classical Plant Growth Analysis",
    "",
    "```{r}",
    "tt <- sort(unique(dat$time))",
    "classic_tbl <- tibble::tibble(message = 'Classical analysis not available for current data.')",
    "if (length(tt) >= 2) {",
    "  fit_classic <- tryCatch(",
    "    classical_growth_interval(",
    "      data = dat,",
    "      time = time,",
    "      total_biomass = total_biomass_g,",
    "      leaf_biomass = leaf_biomass_g,",
    "      root_biomass = root_biomass_g,",
    "      stem_biomass = stem_biomass_g,",
    "      reproductive_biomass = reproductive_biomass_g,",
    "      leaf_area = leaf_area_cm2,",
    "      group_by = params$group_var,",
    "      interval = tt[1:2],",
    "      ci_method = 'classical',",
    "      nar_label = 'both',",
    "      warn = FALSE",
    "    ),",
    "    error = function(e) NULL",
    "  )",
    "  if (!is.null(fit_classic)) {",
    "    classic_tbl <- tidy(fit_classic)",
    "  }",
    "}",
    "knitr::kable(classic_tbl)",
    "```",
    "",
    "### Classical Plot",
    "",
    "```{r}",
    "if (exists('fit_classic') && !is.null(fit_classic)) {",
    "  plot(fit_classic, metric = 'rgr')",
    "}",
    "```",
    "",
    "### Interpretation",
    "",
    "Classical indices above are interval means between two harvests. They are not",
    "instantaneous rates. In this module, ULR and NAR are treated as operationally",
    "equivalent, and LWF/LWR are treated as synonyms.",
    "",
    "## Instantaneous Rates",
    "",
    "```{r}",
    "fit_la <- fit_growth_curve(dat, response = 'leaf_area_cm2', method = params$curve_method, group_cols = c(params$group_var))",
    "inst <- calc_instant_rates(fit_w, leaf_area_fit = fit_la)",
    "plot_growth_rates(inst, type = 'instantaneous', metrics = c('rgr_inst', 'agr_inst', 'nar_inst', 'rlgr_inst', 'algr_inst'))",
    "```",
    "",
    "## Biomass Partition",
    "",
    "```{r}",
    "part <- biomass_partition(dat, group_cols = c(params$group_var, 'time'))",
    "knitr::kable(head(part, 20))",
    "plot_partition(part, facet_var = params$group_var)",
    "```",
    "",
    "## Treatment Comparison",
    "",
    "```{r}",
    "cmp <- compare_growth(dat, metric = 'rgr', group_var = params$group_var)",
    "knitr::kable(cmp)",
    "```",
    "",
    "## Statistical Notes",
    "",
    "This report includes interval-based indices and curve-derived instantaneous rates.\n",
    "Classical indices are averages between consecutive times, while instantaneous\n",
    "metrics are derivatives from smooth fits. Interpretation should consider\n",
    "experimental design, missingness, and unit consistency.\n\n",
    "Classical references include Hunt (1990), Benincasa (2003), and Hunt et al. (2002)."
  )

  template_file <- tempfile(fileext = ".Rmd")
  writeLines(template, con = template_file, useBytes = TRUE)

  rendered <- rmarkdown::render(
    input = template_file,
    output_format = output_format,
    output_file = basename(output_file),
    output_dir = out_dir,
    params = list(
      data = dat,
      group_var = group_var,
      curve_method = curve_method,
      title = title,
      output_format = output_format
    ),
    quiet = quiet,
    envir = new.env(parent = globalenv())
  )

  invisible(rendered)
}

