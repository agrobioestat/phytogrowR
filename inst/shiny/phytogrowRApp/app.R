# phytogrowR - interactive plant growth analysis ----------------------------
#
# This app is optional: it depends on packages listed in Suggests
# (shiny, bslib, DT, readr). `phytogrowR::run_phytogrow_app()` verifies them
# before this file is sourced.

`%||%` <- function(x, y) if (is.null(x)) y else x

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(tibble)
library(phytogrowR)

# --- Theme -----------------------------------------------------------------

BRAND <- list(
  green = "#1F7A5A",
  green_dark = "#155C43",
  green_soft = "#E8F3EE",
  amber = "#B7791F",
  ink = "#1B2430",
  muted = "#5B6875"
)

app_theme <- bslib::bs_theme(
  version = 5,
  primary = BRAND$green,
  secondary = BRAND$muted,
  success = BRAND$green,
  info = "#2C6E8F",
  warning = BRAND$amber,
  "body-color" = BRAND$ink,
  "body-bg" = "#F6F8F7",
  "border-radius" = "0.6rem",
  "card-border-color" = "rgba(27, 36, 48, 0.08)",
  "card-cap-bg" = "#FFFFFF"
)

# A system font stack keeps the app self-contained: no webfont is fetched at
# runtime, so it renders identically offline and inside RStudio's viewer.
app_css <- "
:root {
  --pg-green: #1F7A5A;
  --pg-green-soft: #E8F3EE;
  --pg-ink: #1B2430;
  --pg-muted: #5B6875;
}
body, .card, .btn, .form-control, .form-select {
  font-family: system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
}
.navbar {
  box-shadow: 0 1px 0 rgba(27,36,48,.08);
}
.pg-brand { display: flex; align-items: baseline; gap: .55rem; }
.pg-brand-name { font-weight: 700; letter-spacing: -.02em; font-size: 1.18rem; }
.pg-brand-name .pg-accent { color: var(--pg-green); }
.pg-brand-tag {
  font-size: .78rem; color: var(--pg-muted); font-weight: 500;
  border-left: 1px solid rgba(27,36,48,.18); padding-left: .55rem;
}
.card { box-shadow: 0 1px 2px rgba(27,36,48,.04), 0 1px 10px rgba(27,36,48,.04); }
.card-header { font-weight: 600; font-size: .92rem; }
.bslib-value-box .value-box-title { font-size: .74rem; text-transform: uppercase; letter-spacing: .06em; opacity: .85; }
.bslib-value-box .value-box-value { font-size: 1.55rem; font-weight: 700; letter-spacing: -.02em; }
.bslib-sidebar-layout > .sidebar { background: #FFFFFF; }
.accordion-button { font-weight: 600; font-size: .9rem; }
.accordion-button:not(.collapsed) { background: var(--pg-green-soft); color: var(--pg-green); }
.form-label, .control-label { font-weight: 500; font-size: .85rem; margin-bottom: .2rem; }
.shiny-input-container { margin-bottom: .7rem; }
.pg-run { font-weight: 600; letter-spacing: .01em; }
.pg-hint { font-size: .8rem; color: var(--pg-muted); margin-top: -.35rem; }
.pg-step { display: flex; gap: .8rem; margin-bottom: .9rem; }
.pg-step-n {
  flex: 0 0 1.7rem; height: 1.7rem; border-radius: 50%;
  background: var(--pg-green); color: #fff; font-weight: 700; font-size: .85rem;
  display: flex; align-items: center; justify-content: center;
}
.pg-step-b { font-size: .9rem; }
.pg-step-b strong { display: block; color: var(--pg-ink); }
.pg-step-b span { color: var(--pg-muted); }
.pg-eq {
  background: #FBFCFB; border: 1px solid rgba(27,36,48,.08); border-radius: .5rem;
  padding: .7rem .9rem; font-size: .86rem; color: var(--pg-ink);
}
.pg-tag {
  display: inline-block; font-size: .72rem; font-weight: 600; padding: .12rem .45rem;
  border-radius: .3rem; background: var(--pg-green-soft); color: var(--pg-green);
  margin-right: .3rem;
}
table.dataTable { font-size: .86rem; }
table.dataTable thead th { font-weight: 600; }
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate { font-size: .82rem; }
.nav-tabs .nav-link { font-size: .9rem; font-weight: 500; }
.alert { font-size: .9rem; border: 0; }
.pg-download { margin-bottom: .6rem; }
.pg-vb-chip {
  width: 2.9rem; height: 2.9rem; border-radius: .7rem; padding: .62rem;
  display: flex; align-items: center; justify-content: center;
}
.bslib-value-box .value-box-area { padding: .55rem .2rem; }
.shiny-notification { border-radius: .6rem; font-size: .88rem; }
"

# Small hand-drawn icons: avoids an icon-font dependency for the value boxes.
pg_icon <- function(kind) {
  paths <- switch(
    kind,
    rows = '<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M3 10h18M9 10v10"/>',
    groups = '<circle cx="7" cy="8" r="3"/><circle cx="17" cy="8" r="3"/><path d="M2 20c0-2.8 2.2-5 5-5s5 2.2 5 5M12 20c0-2.8 2.2-5 5-5s5 2.2 5 5"/>',
    clock = '<circle cx="12" cy="12" r="9"/><path d="M12 7v5l3.5 2"/>',
    leaf = '<path d="M4 20c0-8 6-14 16-15 1 10-5 16-13 16H4z"/><path d="M4 20c4-4 7-6 12-8"/>'
  )

  htmltools::HTML(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" ',
    'stroke="currentColor" stroke-width="1.7" stroke-linecap="round" ',
    'stroke-linejoin="round" style="height:100%;">', paths, "</svg>"
  ))
}

# White cards with a tinted icon chip read as a report, not a dashboard toy,
# and keep every box visually distinct without four saturated fills.
pg_value_box <- function(title, output_id, icon, colour) {
  bslib::value_box(
    title = title,
    value = textOutput(output_id),
    showcase = div(
      class = "pg-vb-chip",
      style = paste0("color:", colour, "; background:", colour, "14;"),
      pg_icon(icon)
    ),
    showcase_layout = bslib::showcase_left_center(width = "4.2rem"),
    theme = bslib::value_box_theme(bg = "#FFFFFF", fg = BRAND$ink),
    height = "104px"
  )
}

# --- Example data ----------------------------------------------------------

EXAMPLE_CHOICES <- c(
  "Repeated measurements (full workflow)" = "wide",
  "Destructive sequential harvests" = "harvest",
  "Two-harvest classical design (Hunt et al. 2002)" = "hunt",
  "Long tidy table (auto-converted to wide)" = "long",
  "Quick small subset (fast to run)" = "small"
)

EXAMPLE_NOTES <- list(
  wide = paste(
    "540 rows. Three treatments (Control, NitrogenPlus, WaterDeficit) crossed",
    "with two genotypes, four blocks, and five harvest times (15-75 days).",
    "Exercises every module, including the formal curve comparison."
  ),
  harvest = paste(
    "90 rows of destructive harvest means with a plant count per plot",
    "(n_plants). Use it for crop growth rate and for interval indices based on",
    "plot means rather than individual plants."
  ),
  hunt = paste(
    "72 rows, two treatments and exactly two harvests (30 and 45 days). This is",
    "the design the classical two-harvest formulae of Hunt et al. (2002) were",
    "written for; open the Classical analysis tab with this dataset."
  ),
  long = paste(
    "3780 rows in tidy long format (one row per plant / time / variable). The",
    "app pivots it to wide automatically, which is a good test of the column",
    "mapping step."
  ),
  small = paste(
    "First 180 rows of the repeated-measurements dataset. Same structure, fewer",
    "plants: useful for a fast first run."
  )
)

STANDARD_WIDE_COLS <- c(
  "plant_id", "plot_id", "block", "treatment", "genotype", "date",
  "days_after_sowing", "time", "total_biomass_g", "leaf_biomass_g",
  "stem_biomass_g", "root_biomass_g", "reproductive_biomass_g",
  "leaf_area_cm2", "ground_area_m2"
)

.make_example_dataset <- function(example_id = "wide") {
  if (example_id == "wide") {
    return(phytogrowR::growth_wide_example)
  }

  if (example_id == "small") {
    return(dplyr::slice(phytogrowR::growth_wide_example, 1:180))
  }

  if (example_id == "harvest") {
    return(
      phytogrowR::harvest_example %>%
        dplyr::mutate(
          plant_id = paste0(.data$plot_id, "_H", .data$harvest),
          date = as.Date("2024-01-01") + .data$time,
          days_after_sowing = .data$time,
          reproductive_biomass_g = NA_real_
        ) %>%
        dplyr::select(dplyr::all_of(c(STANDARD_WIDE_COLS, "n_plants")))
    )
  }

  if (example_id == "hunt") {
    return(
      phytogrowR::hunt_classical_example %>%
        dplyr::mutate(
          date = as.Date("2024-01-01") + .data$time,
          days_after_sowing = .data$time,
          ground_area_m2 = NA_real_
        ) %>%
        dplyr::select(dplyr::all_of(c(STANDARD_WIDE_COLS, "harvest")))
    )
  }

  if (example_id == "long") {
    return(
      phytogrowR::growth_long_example %>%
        dplyr::select(-dplyr::any_of("unit")) %>%
        tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
        dplyr::mutate(
          date = as.Date("2024-01-01") + .data$time,
          days_after_sowing = .data$time
        ) %>%
        dplyr::select(dplyr::any_of(STANDARD_WIDE_COLS))
    )
  }

  phytogrowR::growth_wide_example
}

# --- CSV upload ------------------------------------------------------------

.detect_csv_delimiter <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(header) == 0 || is.na(header[1])) {
    return(",")
  }

  first <- header[1]
  counts <- c(
    ";" = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
    "," = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
    "\t" = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE)))
  )

  delim <- names(counts)[which.max(counts)]
  if (max(counts, na.rm = TRUE) == 0) {
    delim <- ","
  }

  delim
}

.read_uploaded_dataset <- function(path) {
  delim <- .detect_csv_delimiter(path)

  # A semicolon file almost always comes from a comma-decimal locale.
  loc <- if (identical(delim, ";")) {
    readr::locale(decimal_mark = ",", grouping_mark = ".")
  } else {
    readr::locale(decimal_mark = ".", grouping_mark = ",")
  }

  readr::read_delim(
    file = path,
    delim = delim,
    locale = loc,
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
}

# --- UI --------------------------------------------------------------------

brand <- div(
  class = "pg-brand",
  span(class = "pg-brand-name", HTML("phytogrow<span class='pg-accent'>R</span>")),
  span(class = "pg-brand-tag", "Plant growth analysis")
)

step <- function(n, title, text) {
  div(
    class = "pg-step",
    div(class = "pg-step-n", n),
    div(class = "pg-step-b", tags$strong(title), tags$span(text))
  )
}

sidebar_ui <- bslib::sidebar(
  width = 350,
  bslib::accordion(
    id = "controls",
    open = c("Data source", "Curve fitting"),
    multiple = TRUE,

    bslib::accordion_panel(
      "Data source",
      radioButtons(
        "source_kind",
        NULL,
        choices = c("Example data" = "example", "Upload a CSV" = "upload"),
        selected = "example"
      ),
      conditionalPanel(
        "input.source_kind == 'example'",
        selectInput("example_data", "Dataset", choices = EXAMPLE_CHOICES, selected = "wide"),
        div(class = "pg-hint", textOutput("example_note", inline = TRUE))
      ),
      conditionalPanel(
        "input.source_kind == 'upload'",
        fileInput("file", "CSV file", accept = c(".csv", ".txt", ".tsv"), width = "100%"),
        div(
          class = "pg-hint",
          "Comma, semicolon and tab delimiters are detected automatically,",
          "including comma-decimal files."
        )
      )
    ),

    bslib::accordion_panel(
      "Column mapping",
      div(class = "pg-hint", "Standard names are matched automatically. Adjust only if needed."),
      br(),
      selectInput("time_col", "Time", choices = NULL),
      selectInput("date_col", "Date (optional)", choices = NULL),
      selectInput("biomass_col", "Total biomass", choices = NULL),
      selectInput("leaf_area_col", "Leaf area", choices = NULL),
      selectInput("leaf_col", "Leaf biomass", choices = NULL),
      selectInput("stem_col", "Stem biomass", choices = NULL),
      selectInput("root_col", "Root biomass", choices = NULL),
      selectInput("repro_col", "Reproductive biomass", choices = NULL),
      selectInput("treat_col", "Treatment", choices = NULL),
      selectInput("geno_col", "Genotype", choices = NULL),
      selectInput("block_col", "Block", choices = NULL),
      selectInput("plot_col", "Plot ID", choices = NULL),
      selectInput("plant_col", "Plant ID", choices = NULL),
      selectInput("ground_col", "Ground area", choices = NULL)
    ),

    bslib::accordion_panel(
      "Curve fitting",
      selectInput(
        "fit_method", "Smoother / model",
        choices = c(
          "GAM (penalised spline)" = "gam",
          "Smoothing spline" = "spline",
          "LOESS" = "loess",
          "Logistic" = "logistic",
          "Gompertz" = "gompertz",
          "Richards" = "richards"
        ),
        selected = "gam"
      ),
      selectInput("response_var", "Response for the curve", choices = NULL),
      selectInput(
        "cmp_metric", "Index compared between treatments",
        choices = c("rgr", "nar", "lar", "lai", "sla", "cgr", "algr", "rlgr", "final_biomass"),
        selected = "rgr"
      )
    ),

    bslib::accordion_panel(
      "Classical interval",
      div(class = "pg-hint", "Interval indices between two harvests (Hunt et al. 2002)."),
      br(),
      selectInput("classic_group", "Grouping", choices = NULL),
      selectInput("classic_h1", "Harvest 1", choices = NULL),
      selectInput("classic_h2", "Harvest 2", choices = NULL),
      selectInput(
        "classic_ci", "Confidence interval",
        choices = c("Classical (fast)" = "classical", "Bootstrap" = "bootstrap", "Both" = "both"),
        selected = "classical"
      ),
      conditionalPanel(
        "input.classic_ci != 'classical'",
        selectInput(
          "classic_boot_unit", "Bootstrap unit",
          choices = c("row", "plant", "plot"), selected = "row"
        ),
        numericInput("classic_n_boot", "Bootstrap resamples", value = 199, min = 50, max = 5000, step = 50),
        div(
          class = "pg-hint",
          "Resampling is the slowest step of the app: expect tens of seconds."
        )
      )
    ),

    bslib::accordion_panel(
      "Curve comparison",
      div(
        class = "pg-hint",
        "H0: one single curve describes every treatment. H1: one curve per treatment."
      ),
      br(),
      selectInput("cc_group", "Grouping (treatment)", choices = NULL),
      selectInput("cc_response", "Response", choices = NULL),
      selectInput(
        "cc_model", "Functional form",
        choices = c(
          "Polynomial in time" = "poly",
          "Exponential" = "exponential",
          "Logistic" = "logistic",
          "Gompertz" = "gompertz",
          "Richards" = "richards"
        ),
        selected = "poly"
      ),
      conditionalPanel(
        "input.cc_model == 'poly'",
        numericInput("cc_degree", "Polynomial degree", value = 2, min = 1, max = 5, step = 1)
      ),
      checkboxInput("cc_log", "Fit on ln(response)", value = TRUE),
      selectInput(
        "cc_test", "Test statistic",
        choices = c("Both" = "both", "F test (nested models)" = "F", "Likelihood ratio" = "LRT"),
        selected = "both"
      ),
      checkboxInput("cc_pairwise", "Pairwise comparisons", value = TRUE),
      selectInput(
        "cc_padjust", "p-value adjustment",
        choices = c("BH", "holm", "bonferroni", "none"), selected = "BH"
      )
    )
  ),
  actionButton("run", "Run analysis", class = "btn btn-primary btn-lg w-100 pg-run mt-2"),
  div(class = "pg-hint mt-2", "Re-run after changing any option above.")
)

overview_panel <- bslib::nav_panel(
  "Overview",
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("What this app does"),
      bslib::card_body(
        p(
          "phytogrowR covers the two established routes of plant growth analysis, ",
          "from sequential or destructive harvests of biomass and leaf area."
        ),
        p(
          span(class = "pg-tag", "Classical"),
          "Interval indices between two harvests, with the formulae and the ",
          "assumptions of Radford (1967), Poorter & Garnier (1996) and ",
          "Hunt, Causton, Shipley & Askew (2002): RGR, AGR, ULR (= NAR), LAR, ",
          "SLA, LWF, CGR, biomass partitioning and root-shoot allometry."
        ),
        p(
          span(class = "pg-tag", "Functional"),
          "A smooth curve is fitted through time and the rates are read off its ",
          "derivative, with bootstrap confidence intervals and a formal test of ",
          "whether the treatments share one single curve."
        ),
        hr(),
        h6("Workflow"),
        step(1, "Pick a data source", "An example dataset, or your own CSV."),
        step(2, "Check the column mapping", "Standard names are detected automatically."),
        step(3, "Run the analysis", "One button; every module reports its own status."),
        step(4, "Read and export", "Tables, figures, CSV files and a full report.")
      )
    ),
    div(
      bslib::card(
        bslib::card_header("The statistical question behind Curve comparison"),
        bslib::card_body(
          p(
            "Comparing one index at a time answers a narrow question. Comparing ",
            "whole curves is a nested-model problem:"
          ),
          div(
            class = "pg-eq",
            tags$b("H0 (reduced)"), " one curve for all treatments, ",
            tags$i("p"), tags$sub("0"), " = ", tags$i("p"), " parameters", br(),
            tags$b("H1 (full)"), " one curve per treatment, ",
            tags$i("p"), tags$sub("1"), " = ", tags$i("k p"), " parameters", br(), br(),
            tags$i("F"), " = [(RSS", tags$sub("0"), " − RSS", tags$sub("1"),
            ") / (", tags$i("p"), tags$sub("1"), " − ", tags$i("p"), tags$sub("0"),
            ")] / [RSS", tags$sub("1"), " / (", tags$i("n"), " − ",
            tags$i("p"), tags$sub("1"), ")]", br(),
            tags$i("G"), " = ", tags$i("n"), " log(RSS", tags$sub("0"), " / RSS",
            tags$sub("1"), ") ~ χ²"
          ),
          p(
            class = "mt-3 mb-0",
            "The F test is exact under normality for the polynomial family and ",
            "approximate for the non-linear ones; the likelihood ratio test is ",
            "asymptotic. Both come from the same pair of fits."
          )
        )
      ),
      bslib::card(
        bslib::card_header("Templates and citation"),
        bslib::card_body(
          p(class = "mb-2", "Start from a spreadsheet with the standard column names:"),
          div(
            class = "pg-download",
            downloadButton("dl_template_growth", "Repeated-measures template",
                           class = "btn btn-outline-primary btn-sm w-100")
          ),
          div(
            class = "pg-download",
            downloadButton("dl_template_harvest", "Harvest-means template",
                           class = "btn btn-outline-primary btn-sm w-100")
          ),
          hr(),
          p(class = "pg-hint mb-0", HTML(
            "Cite the package with <code>citation(\"phytogrowR\")</code>."
          ))
        )
      )
    )
  )
)

ui <- bslib::page_sidebar(
  title = brand,
  theme = app_theme,
  window_title = "phytogrowR",
  sidebar = sidebar_ui,
  tags$head(tags$style(HTML(app_css))),

  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    fill = FALSE,
    pg_value_box("Observations", "vb_rows", "rows", BRAND$green),
    pg_value_box("Treatments", "vb_groups", "groups", "#2C6E8F"),
    pg_value_box("Harvest times", "vb_times", "clock", "#7A4E9E"),
    pg_value_box("Time span (days)", "vb_span", "leaf", BRAND$amber)
  ),

  bslib::navset_card_tab(
    id = "main_tabs",

    overview_panel,

    bslib::nav_panel(
      "Data",
      bslib::card(
        bslib::card_header("As uploaded / as shipped"),
        DTOutput("raw_preview")
      ),
      bslib::card(
        bslib::card_header("After prepare_growth_data()"),
        DTOutput("std_preview")
      )
    ),

    bslib::nav_panel(
      "Diagnostics",
      bslib::card(
        bslib::card_header("Module status"),
        DTOutput("status_tbl")
      ),
      bslib::card(
        bslib::card_header("Data quality checks"),
        DTOutput("check_tbl")
      )
    ),

    bslib::nav_panel(
      "Curves",
      bslib::card(
        height = "540px",
        full_screen = TRUE,
        bslib::card_header("Fitted growth trajectories"),
        bslib::card_body(plotOutput("curve_plot", height = "100%"))
      ),
      bslib::card(
        bslib::card_header("Curve landmarks"),
        bslib::card_body(
          div(
            class = "pg-hint",
            "When growth peaked, how fast it was then, and how long the active",
            "phase lasted. Read the asymptote as a lower bound whenever",
            "plateau_reached is FALSE: the curve was still rising at the last harvest."
          ),
          DTOutput("curve_params_tbl")
        )
      ),
      bslib::card(
        bslib::card_header("Fit messages"),
        DTOutput("curve_messages")
      )
    ),

    bslib::nav_panel(
      "Rates",
      bslib::card(
        height = "540px",
        full_screen = TRUE,
        bslib::card_header("Growth rates through time"),
        bslib::card_body(plotOutput("rates_plot", height = "100%"))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Interval (classic) rates"), DTOutput("classic_tbl")),
        bslib::card(bslib::card_header("Instantaneous rates"), DTOutput("instant_tbl"))
      )
    ),

    bslib::nav_panel(
      "Classical analysis",
      bslib::card(
        height = "520px",
        full_screen = TRUE,
        bslib::card_header("Interval indices between two harvests"),
        bslib::card_body(plotOutput("classic_interval_plot", height = "100%"))
      ),
      bslib::card(
        bslib::card_header("Estimates and confidence intervals"),
        DTOutput("classic_interval_tbl")
      ),
      bslib::card(
        bslib::card_header("Assumption checks"),
        DTOutput("classic_checks_tbl")
      )
    ),

    bslib::nav_panel(
      "Partition",
      bslib::card(
        height = "540px",
        full_screen = TRUE,
        bslib::card_header("Biomass allocation"),
        bslib::card_body(plotOutput("partition_plot", height = "100%"))
      ),
      bslib::card(
        bslib::card_header("Partition table"),
        DTOutput("partition_tbl")
      )
    ),

    bslib::nav_panel(
      "Comparisons",
      bslib::card(
        bslib::card_header("Pairwise contrasts for a single index"),
        DTOutput("compare_tbl")
      )
    ),

    bslib::nav_panel(
      "Curve comparison",
      uiOutput("cc_verdict"),
      bslib::card(
        height = "560px",
        full_screen = TRUE,
        bslib::card_header("Treatment curves against the common curve under H0"),
        bslib::card_body(plotOutput("cc_plot", height = "100%"))
      ),
      bslib::card(
        bslib::card_header("Which functional form fits best?"),
        bslib::card_body(
          div(
            class = "pg-hint",
            "Candidates ranked by AIC within each treatment. A delta below about 2",
            "means no meaningful distinction, so prefer the simpler or more",
            "interpretable family. Use this to choose the form tested below."
          ),
          DTOutput("model_sel_tbl")
        )
      ),
      bslib::card(
        bslib::card_header("Nested-model tests"),
        DTOutput("cc_tests")
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header("Pairwise coincidence tests"),
          DTOutput("cc_pairwise_tbl")
        ),
        bslib::card(
          bslib::card_header("Per-treatment parameters"),
          DTOutput("cc_params")
        )
      )
    ),

    bslib::nav_panel(
      "Export",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Tables (CSV)"),
          bslib::card_body(
            div(class = "pg-download", downloadButton("download_classic", "Interval (classic) rates", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_classical_interval", "Classical interval analysis", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_instant", "Instantaneous rates", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_partition", "Biomass partition", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_compare", "Index comparisons", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_curve_test", "Curve comparison tests", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_curve_params", "Curve landmarks", class = "btn btn-outline-primary w-100")),
            div(class = "pg-download", downloadButton("download_model_sel", "Model selection", class = "btn btn-outline-primary w-100"))
          )
        ),
        bslib::card(
          bslib::card_header("Full report"),
          bslib::card_body(
            p(
              class = "pg-hint",
              "One document with the data summary, quality checks, fitted curves,",
              "classical and instantaneous rates, partitioning and comparisons."
            ),
            selectInput(
              "report_format", "Format",
              choices = c("HTML" = "html_document", "Word" = "word_document", "PDF" = "pdf_document"),
              selected = "html_document"
            ),
            downloadButton("download_report", "Download report", class = "btn btn-primary w-100"),
            div(
              class = "pg-hint mt-3",
              HTML(paste(
                "Needs <code>rmarkdown</code> and <code>knitr</code>;",
                "PDF output additionally needs a LaTeX installation."
              ))
            )
          )
        )
      )
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  safe_exec <- function(expr) {
    tryCatch(
      list(result = force(expr), error = NULL),
      error = function(e) list(result = NULL, error = conditionMessage(e))
    )
  }

  dt_table <- function(data, page_length = 10, scroll_x = TRUE) {
    data <- tibble::as_tibble(data)

    tbl <- DT::datatable(
      data,
      rownames = FALSE,
      class = "stripe hover row-border",
      options = list(
        scrollX = scroll_x,
        pageLength = page_length,
        lengthChange = FALSE,
        autoWidth = FALSE,
        dom = "tip"
      )
    )

    num_cols <- names(data)[vapply(data, is.numeric, logical(1))]

    if (length(num_cols) > 0) {
      # Counts and degrees of freedom must not be shown as "6.000".
      whole <- vapply(
        data[num_cols],
        function(z) all(is.na(z) | (is.finite(z) & z == round(z))),
        logical(1)
      )

      int_cols <- num_cols[whole]
      p_cols <- intersect(num_cols[!whole], c("p_value", "p.value", "p_adj", "padj"))
      real_cols <- setdiff(num_cols[!whole], p_cols)

      if (length(int_cols) > 0) tbl <- DT::formatRound(tbl, int_cols, digits = 0)
      if (length(real_cols) > 0) tbl <- DT::formatSignif(tbl, real_cols, digits = 4)
      if (length(p_cols) > 0) tbl <- DT::formatSignif(tbl, p_cols, digits = 3)
    }

    tbl
  }

  info_table <- function(module, status, message) {
    dt_table(
      tibble::tibble(module = module, status = status, message = message),
      page_length = 5,
      scroll_x = FALSE
    )
  }

  empty_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate(
        "text", x = 0, y = 0, label = msg,
        size = 4, colour = "#5B6875", hjust = 0.5, vjust = 0.5
      ) +
      ggplot2::theme_void()
  }

  # A fit object can exist while every group failed to converge (a two-harvest
  # design cannot support a smooth curve, for instance). The plotting functions
  # abort in that case, so every plot goes through this guard and the reason is
  # shown in the panel instead of breaking the session.
  safe_plot <- function(expr, fallback) {
    res <- tryCatch(force(expr), error = function(e) conditionMessage(e))
    if (inherits(res, "ggplot")) print(res) else print(empty_plot(res %||% fallback))
  }

  output$example_note <- renderText({
    EXAMPLE_NOTES[[input$example_data %||% "wide"]] %||% ""
  })

  # --- Data ----------------------------------------------------------------

  raw_data_res <- reactive({
    if (identical(input$source_kind %||% "example", "example")) {
      return(list(result = .make_example_dataset(input$example_data %||% "wide"), error = NULL))
    }

    req(input$file)
    safe_exec(.read_uploaded_dataset(input$file$datapath))
  })

  observeEvent(raw_data_res(), {
    res <- raw_data_res()
    if (!is.null(res$error) || is.null(res$result)) {
      return()
    }

    nm <- names(res$result)
    if (length(nm) == 0) {
      return()
    }

    optional <- c("None", nm)
    auto_pick <- function(target, fallback = "None") if (target %in% nm) target else fallback

    updateSelectInput(session, "time_col", choices = nm, selected = auto_pick("time", nm[1]))
    updateSelectInput(session, "date_col", choices = optional, selected = auto_pick("date"))
    updateSelectInput(session, "biomass_col", choices = nm, selected = auto_pick("total_biomass_g", nm[1]))
    updateSelectInput(session, "leaf_area_col", choices = nm, selected = auto_pick("leaf_area_cm2", nm[1]))
    updateSelectInput(session, "leaf_col", choices = optional, selected = auto_pick("leaf_biomass_g"))
    updateSelectInput(session, "stem_col", choices = optional, selected = auto_pick("stem_biomass_g"))
    updateSelectInput(session, "root_col", choices = optional, selected = auto_pick("root_biomass_g"))
    updateSelectInput(session, "repro_col", choices = optional, selected = auto_pick("reproductive_biomass_g"))
    updateSelectInput(session, "treat_col", choices = optional, selected = auto_pick("treatment"))
    updateSelectInput(session, "geno_col", choices = optional, selected = auto_pick("genotype"))
    updateSelectInput(session, "block_col", choices = optional, selected = auto_pick("block"))
    updateSelectInput(session, "plot_col", choices = optional, selected = auto_pick("plot_id"))
    updateSelectInput(session, "plant_col", choices = optional, selected = auto_pick("plant_id"))
    updateSelectInput(session, "ground_col", choices = optional, selected = auto_pick("ground_area_m2"))
  }, ignoreInit = FALSE)

  mapping <- reactive({
    pick <- function(x) if (!is.null(x) && !identical(x, "None")) x else NULL

    map <- list(
      time = input$time_col,
      date = pick(input$date_col),
      total_biomass_g = input$biomass_col,
      leaf_area_cm2 = input$leaf_area_col,
      leaf_biomass_g = pick(input$leaf_col),
      stem_biomass_g = pick(input$stem_col),
      root_biomass_g = pick(input$root_col),
      reproductive_biomass_g = pick(input$repro_col),
      treatment = pick(input$treat_col),
      genotype = pick(input$geno_col),
      block = pick(input$block_col),
      plot_id = pick(input$plot_col),
      plant_id = pick(input$plant_col),
      ground_area_m2 = pick(input$ground_col)
    )

    map[!vapply(map, is.null, logical(1))]
  })

  std_data_res <- reactive({
    res <- raw_data_res()
    if (!is.null(res$error) || is.null(res$result)) {
      return(list(result = NULL, error = res$error %||% "Data input unavailable."))
    }

    safe_exec(prepare_growth_data(res$result, mapping = mapping(), quiet = TRUE))
  })

  observeEvent(std_data_res(), {
    res <- std_data_res()
    if (!is.null(res$error) || is.null(res$result)) {
      return()
    }

    dat <- res$result

    choices <- intersect(
      c("total_biomass_g", "leaf_area_cm2", "leaf_biomass_g", "stem_biomass_g", "root_biomass_g"),
      names(dat)
    )
    if (length(choices) == 0) {
      choices <- names(dat)
    }

    sel <- if ("total_biomass_g" %in% choices) "total_biomass_g" else choices[1]
    updateSelectInput(session, "response_var", choices = choices, selected = sel)
    updateSelectInput(session, "cc_response", choices = choices, selected = sel)

    g_choices <- intersect(c("treatment", "genotype", "block", "plot_id"), names(dat))
    if (length(g_choices) == 0) {
      g_choices <- "group_id"
    }

    g_sel <- if ("treatment" %in% g_choices) "treatment" else g_choices[1]
    updateSelectInput(session, "classic_group", choices = g_choices, selected = g_sel)
    updateSelectInput(session, "cc_group", choices = g_choices, selected = g_sel)

    tt <- sort(unique(dat$time))
    tt <- tt[!is.na(tt)]
    if (length(tt) >= 2) {
      updateSelectInput(session, "classic_h1", choices = tt, selected = tt[1])
      updateSelectInput(session, "classic_h2", choices = tt, selected = tt[min(2, length(tt))])
    } else {
      updateSelectInput(session, "classic_h1", choices = tt, selected = tt[1] %||% NULL)
      updateSelectInput(session, "classic_h2", choices = tt, selected = tt[1] %||% NULL)
    }

    # A degree-d polynomial needs d+1 distinct harvest dates in every group, so
    # the control is capped by the design rather than letting the user pick an
    # unidentifiable model and meet an error afterwards.
    grp_for_deg <- if ("treatment" %in% names(dat)) {
      dat$treatment
    } else if ("genotype" %in% names(dat)) {
      dat$genotype
    } else {
      rep("all", nrow(dat))
    }

    per_group_times <- tapply(dat$time, grp_for_deg, function(z) length(unique(z[!is.na(z)])))
    max_degree <- max(1L, min(as.integer(per_group_times), na.rm = TRUE) - 1L)
    max_degree <- min(max_degree, 5L)

    updateNumericInput(
      session, "cc_degree",
      max = max_degree,
      value = min(as.integer(input$cc_degree %||% 2L), max_degree)
    )
  }, ignoreInit = FALSE)

  # --- Value boxes ---------------------------------------------------------

  design <- reactive({
    res <- std_data_res()
    if (!is.null(res$error) || is.null(res$result)) {
      return(NULL)
    }

    dat <- res$result
    tt <- sort(unique(dat$time[!is.na(dat$time)]))

    grp <- if ("treatment" %in% names(dat) && !all(is.na(dat$treatment))) {
      dat$treatment
    } else if ("genotype" %in% names(dat) && !all(is.na(dat$genotype))) {
      dat$genotype
    } else {
      NULL
    }

    list(
      n_rows = nrow(dat),
      n_groups = if (is.null(grp)) NA_integer_ else length(unique(stats::na.omit(grp))),
      n_times = length(tt),
      span = if (length(tt) >= 2) max(tt) - min(tt) else NA_real_
    )
  })

  output$vb_rows <- renderText({
    d <- design()
    if (is.null(d)) "—" else format(d$n_rows, big.mark = ",")
  })

  output$vb_groups <- renderText({
    d <- design()
    if (is.null(d) || is.na(d$n_groups)) "—" else as.character(d$n_groups)
  })

  output$vb_times <- renderText({
    d <- design()
    if (is.null(d)) "—" else as.character(d$n_times)
  })

  output$vb_span <- renderText({
    d <- design()
    if (is.null(d) || is.na(d$span)) "—" else format(d$span)
  })

  # --- Previews ------------------------------------------------------------

  output$raw_preview <- renderDT({
    res <- raw_data_res()
    if (!is.null(res$error)) {
      return(info_table("Data source", "error", res$error))
    }

    dt_table(head(res$result, 50), page_length = 10)
  })

  output$std_preview <- renderDT({
    res <- std_data_res()
    if (!is.null(res$error)) {
      return(info_table("Data preparation", "error", res$error))
    }

    dt_table(head(res$result, 50), page_length = 10)
  })

  output$check_tbl <- renderDT({
    res <- std_data_res()
    if (!is.null(res$error)) {
      return(info_table("Checks", "error", res$error))
    }

    dt_table(check_growth_data(res$result), page_length = 10)
  })

  # --- Analysis ------------------------------------------------------------

  analysis <- eventReactive(input$run, {
    std_res <- std_data_res()

    status_tbl <- tibble::tibble(
      module = character(), status = character(), message = character()
    )

    add_status <- function(module, status, message) {
      status_tbl <<- dplyr::bind_rows(
        status_tbl,
        tibble::tibble(module = module, status = status, message = message)
      )
    }

    # Ten steps; the bootstrap branch of the classical module is by far the
    # slowest, so the user is told which one is running.
    progress <- shiny::Progress$new(session, min = 0, max = 10)
    progress$set(message = "Running analysis", value = 0)
    on.exit(progress$close(), add = TRUE)

    stepno <- 0
    step <- function(label) {
      stepno <<- stepno + 1
      progress$set(value = stepno, detail = label)
    }

    step("preparing data")

    if (!is.null(std_res$error) || is.null(std_res$result)) {
      err <- std_res$error %||% "Standardized data unavailable."
      add_status("Data preparation", "error", err)
      return(list(
        dat = tibble::tibble(),
        fit_main = NULL, fit_error = err, fit_la = NULL,
        classic = tibble::tibble(), classic_error = err,
        classic_interval = NULL, classic_interval_error = err,
        instant = tibble::tibble(), instant_error = err,
        partition = tibble::tibble(), partition_error = err,
        compare = tibble::tibble(), compare_error = err,
        curve_test = NULL, curve_test_error = err,
        curve_params = NULL, curve_params_error = err,
        model_sel = NULL, model_sel_error = err,
        status = status_tbl
      ))
    }

    dat <- std_res$result
    add_status("Data preparation", "ok", paste0(nrow(dat), " rows ready for analysis."))

    gcols <- intersect(c("treatment", "genotype"), names(dat))
    if (length(gcols) == 0) {
      gcols <- intersect(c("plot_id"), names(dat))
    }

    step("fitting growth curves")
    fit_main_res <- safe_exec(
      fit_growth_curve(
        data = dat,
        response = input$response_var,
        method = input$fit_method,
        group_cols = gcols
      )
    )
    fit_main <- fit_main_res$result

    if (is.null(fit_main)) {
      add_status("Curve fitting", "error", fit_main_res$error %||% "Curve fit unavailable for this setup.")
    } else {
      # The object can come back with every group failed (a smooth curve needs
      # at least four distinct harvest dates), so report what really happened
      # instead of a blanket "ok".
      msgs <- fit_main$messages
      n_ok <- if (is.null(msgs)) 0L else sum(msgs$status == "ok", na.rm = TRUE)
      n_all <- if (is.null(msgs)) 0L else nrow(msgs)

      if (n_ok == 0L) {
        add_status(
          "Curve fitting", "warning",
          paste0(
            "No group could be fitted for ", input$response_var,
            ". Reason: ", (msgs$message[1] %||% "unknown"),
            " Curve-based modules were skipped; the classical interval module still applies."
          )
        )
      } else if (n_ok < n_all) {
        add_status(
          "Curve fitting", "warning",
          paste0(n_ok, " of ", n_all, " groups fitted for ", input$response_var, "; see Fit messages.")
        )
      } else {
        add_status(
          "Curve fitting", "ok",
          paste0(n_all, " group(s) fitted for ", input$response_var, ".")
        )
      }
    }

    fit_la <- NULL
    if ("leaf_area_cm2" %in% names(dat)) {
      fit_la <- safe_exec(
        fit_growth_curve(dat, response = "leaf_area_cm2", method = input$fit_method, group_cols = gcols)
      )$result
    }

    fit_lm <- NULL
    if ("leaf_biomass_g" %in% names(dat)) {
      fit_lm <- safe_exec(
        fit_growth_curve(dat, response = "leaf_biomass_g", method = input$fit_method, group_cols = gcols)
      )$result
    }

    step("interval (classic) rates")
    classic_res <- safe_exec(
      calc_classic_growth(
        dat,
        group_cols = intersect(c("treatment", "genotype", "plot_id", "plant_id"), names(dat)),
        epsilon = 1e-6
      )
    )
    classic <- classic_res$result %||% tibble::tibble()

    if (!is.null(classic_res$error)) {
      add_status("Classic rates", "error", classic_res$error)
    } else {
      add_status("Classic rates", "ok", paste0(nrow(classic), " rows calculated."))
    }

    c_group <- if (!is.null(input$classic_group) && input$classic_group %in% names(dat)) {
      input$classic_group
    } else if ("treatment" %in% names(dat)) {
      "treatment"
    } else if ("genotype" %in% names(dat)) {
      "genotype"
    } else {
      NULL
    }

    c_times <- sort(unique(dat$time))
    c_times <- c_times[!is.na(c_times)]
    c_interval <- if (!is.null(input$classic_h1) && !is.null(input$classic_h2)) {
      suppressWarnings(as.numeric(c(input$classic_h1, input$classic_h2)))
    } else {
      c_times[1:min(2, length(c_times))]
    }

    if (length(c_interval) < 2 || any(is.na(c_interval)) || c_interval[1] == c_interval[2]) {
      c_interval <- c_times[1:min(2, length(c_times))]
    }

    step(if (identical(input$classic_ci, "classical")) {
      "classical interval"
    } else {
      "classical interval (bootstrap, this is the slow one)"
    })

    classic_interval <- NULL
    classic_interval_error <- NULL

    if (length(c_interval) == 2 && !is.null(c_group)) {
      dat_interval <- dat[dat$time %in% c_interval, , drop = FALSE]

      non_positive <- rep(FALSE, nrow(dat_interval))
      if ("total_biomass_g" %in% names(dat_interval)) {
        non_positive <- non_positive |
          (!is.na(dat_interval$total_biomass_g) & dat_interval$total_biomass_g <= 0)
      }
      if ("leaf_area_cm2" %in% names(dat_interval)) {
        non_positive <- non_positive |
          (!is.na(dat_interval$leaf_area_cm2) & dat_interval$leaf_area_cm2 <= 0)
      }

      removed_non_positive <- sum(non_positive, na.rm = TRUE)
      if (removed_non_positive > 0) {
        dat_interval <- dat_interval[!non_positive, , drop = FALSE]
        add_status(
          "Classical interval", "warning",
          paste0(
            "Removed ", removed_non_positive,
            " rows with non-positive biomass/leaf area for log-based indices."
          )
        )
      }

      if (nrow(dat_interval) > 0) {
        c_res <- safe_exec(
          classical_growth_interval(
            data = dat_interval,
            time = time,
            total_biomass = total_biomass_g,
            leaf_biomass = leaf_biomass_g,
            root_biomass = root_biomass_g,
            stem_biomass = stem_biomass_g,
            reproductive_biomass = reproductive_biomass_g,
            leaf_area = leaf_area_cm2,
            group_by = c_group,
            interval = c_interval,
            ci_method = input$classic_ci,
            bootstrap_unit = input$classic_boot_unit,
            n_boot = as.integer(input$classic_n_boot),
            nar_label = "both",
            seed = 123,
            warn = FALSE
          )
        )

        classic_interval <- c_res$result
        classic_interval_error <- c_res$error
      } else {
        classic_interval_error <- "No rows left in the selected interval after filtering non-positive values."
      }

      if (!is.null(classic_interval) && inherits(classic_interval, "phytogrow_classical")) {
        add_status("Classical interval", "ok", "Classical interval analysis completed.")
      } else {
        add_status(
          "Classical interval", "error",
          classic_interval_error %||% "Classical interval analysis unavailable."
        )
      }
    } else {
      classic_interval_error <- "Select two valid harvests and one grouping variable for classical interval analysis."
      add_status("Classical interval", "warning", classic_interval_error)
    }

    step("instantaneous rates")
    inst <- tibble::tibble()
    instant_error <- NULL
    if (!is.null(fit_main)) {
      inst_res <- safe_exec(
        calc_instant_rates(fit_main, leaf_area_fit = fit_la, leaf_mass_fit = fit_lm, epsilon = 1e-6)
      )
      inst <- inst_res$result %||% tibble::tibble()
      instant_error <- inst_res$error

      if (!is.null(instant_error)) {
        add_status("Instant rates", "error", instant_error)
      } else {
        add_status("Instant rates", "ok", paste0(nrow(inst), " rows calculated."))
      }
    } else {
      instant_error <- "Main curve fit unavailable; instant rates were skipped."
      add_status("Instant rates", "warning", instant_error)
    }

    step("biomass partition")
    part_res <- safe_exec(
      biomass_partition(dat, group_cols = intersect(c("treatment", "genotype", "time"), names(dat)))
    )
    part <- part_res$result %||% tibble::tibble()

    if (!is.null(part_res$error)) {
      add_status("Partition", "error", part_res$error)
    } else {
      add_status("Partition", "ok", paste0(nrow(part), " rows calculated."))
    }

    cmp_group <- if ("treatment" %in% names(dat)) {
      "treatment"
    } else if ("genotype" %in% names(dat)) {
      "genotype"
    } else {
      NULL
    }

    step("index comparisons")
    cmp <- tibble::tibble()
    cmp_error <- NULL

    if (!is.null(cmp_group)) {
      cmp_res <- safe_exec(
        compare_growth(dat, metric = input$cmp_metric, group_var = cmp_group, epsilon = 1e-6)
      )
      cmp <- cmp_res$result %||% tibble::tibble()
      cmp_error <- cmp_res$error

      if (!is.null(cmp_error)) {
        add_status("Comparisons", "error", cmp_error)
      } else {
        add_status("Comparisons", "ok", paste0(nrow(cmp), " contrasts computed."))
      }
    } else {
      cmp_error <- "No treatment or genotype column available for comparisons."
      add_status("Comparisons", "warning", cmp_error)
    }

    # --- Formal test of coincidence of curves between treatments -------------
    # H0 (reduced model): one single curve describes every treatment.
    # H1 (full model)   : one curve per treatment.
    # The F test / likelihood ratio test compares these two NESTED models.
    cc_group <- if (!is.null(input$cc_group) && input$cc_group %in% names(dat)) {
      input$cc_group
    } else if ("treatment" %in% names(dat)) {
      "treatment"
    } else if ("genotype" %in% names(dat)) {
      "genotype"
    } else {
      NULL
    }

    cc_response <- if (!is.null(input$cc_response) && input$cc_response %in% names(dat)) {
      input$cc_response
    } else {
      "total_biomass_g"
    }

    step("curve landmarks and model selection")
    curve_params <- safe_exec(
      if (is.null(fit_main)) NULL else growth_curve_params(fit_main)
    )

    model_sel <- safe_exec(
      compare_growth_models(
        data = dat,
        response = input$response_var %||% "total_biomass_g",
        time_col = "time",
        group_var = if ("treatment" %in% names(dat)) "treatment" else NULL,
        models = c("exponential", "logistic", "gompertz", "richards"),
        poly_degrees = 1:3
      )
    )

    if (!is.null(model_sel$result) && nrow(model_sel$result) > 0) {
      best <- model_sel$result[model_sel$result$best, , drop = FALSE]
      add_status(
        "Model selection", "ok",
        paste0("Best by AIC: ", paste(unique(best$model), collapse = ", "), ".")
      )
    } else {
      add_status("Model selection", "warning",
                 model_sel$error %||% "Model selection unavailable.")
    }

    step("curve coincidence test")
    curve_test <- NULL
    curve_test_error <- NULL

    if (is.null(cc_group)) {
      curve_test_error <- "No grouping column available for the curve comparison."
      add_status("Curve comparison", "warning", curve_test_error)
    } else if (length(unique(stats::na.omit(dat[[cc_group]]))) < 2) {
      curve_test_error <- paste0(
        "Grouping variable ", cc_group,
        " has fewer than two levels; there is nothing to compare."
      )
      add_status("Curve comparison", "warning", curve_test_error)
    } else {
      cc_res <- safe_exec(
        compare_growth_curves(
          data = dat,
          response = cc_response,
          time_col = "time",
          group_var = cc_group,
          model = input$cc_model %||% "poly",
          degree = as.integer(input$cc_degree %||% 2L),
          log_response = isTRUE(input$cc_log),
          # A small offset keeps ln() defined when a harvest recorded a zero.
          epsilon = if (isTRUE(input$cc_log)) 1e-6 else NULL,
          test = input$cc_test %||% "both",
          pairwise = isTRUE(input$cc_pairwise),
          p_adjust_method = input$cc_padjust %||% "BH"
        )
      )

      curve_test <- cc_res$result
      curve_test_error <- cc_res$error

      if (!is.null(curve_test)) {
        add_status(
          "Curve comparison", "ok",
          paste0(
            "Coincidence test computed for ", cc_response, " by ", cc_group,
            " (model: ", input$cc_model %||% "poly", ")."
          )
        )
      } else {
        add_status(
          "Curve comparison", "error",
          curve_test_error %||% "Curve comparison unavailable."
        )
      }
    }

    step("done")

    list(
      dat = dat,
      fit_main = fit_main,
      fit_error = fit_main_res$error,
      fit_la = fit_la,
      classic = classic,
      classic_error = classic_res$error,
      classic_interval = classic_interval,
      classic_interval_error = classic_interval_error,
      instant = inst,
      instant_error = instant_error,
      partition = part,
      partition_error = part_res$error,
      compare = cmp,
      compare_error = cmp_error,
      curve_test = curve_test,
      curve_test_error = curve_test_error,
      curve_params = curve_params$result,
      curve_params_error = curve_params$error,
      model_sel = model_sel$result,
      model_sel_error = model_sel$error,
      status = status_tbl
    )
  }, ignoreNULL = FALSE)

  output$status_tbl <- renderDT({
    req(analysis())
    dt_table(analysis()$status, page_length = 12, scroll_x = FALSE)
  })

  # --- Curves and rates ----------------------------------------------------

  output$curve_plot <- renderPlot(res = 96, {
    req(analysis())
    if (is.null(analysis()$fit_main)) {
      print(empty_plot(analysis()$fit_error %||% "Curve fit unavailable for this setup."))
    } else {
      safe_plot(plot_growth_curve(analysis()$fit_main), "Curve plot unavailable.")
    }
  })

  output$curve_messages <- renderDT({
    req(analysis())
    fit <- analysis()$fit_main

    if (!is.null(fit) && !is.null(fit$messages) && nrow(fit$messages) > 0) {
      dt_table(fit$messages, page_length = 10)
    } else if (!is.null(analysis()$fit_error)) {
      info_table("Curve fitting", "error", analysis()$fit_error)
    } else {
      info_table("Curve fitting", "info", "No additional curve messages.")
    }
  })

  output$curve_params_tbl <- renderDT({
    req(analysis())
    p <- analysis()$curve_params

    if (is.null(p) || nrow(p) == 0) {
      return(info_table(
        "Curve landmarks", "warning",
        analysis()$curve_params_error %||% "Landmarks need a curve fit that converged."
      ))
    }

    dt_table(p, page_length = 10)
  })

  output$model_sel_tbl <- renderDT({
    req(analysis())
    m <- analysis()$model_sel

    if (is.null(m) || nrow(m) == 0) {
      return(info_table(
        "Model selection", "warning",
        analysis()$model_sel_error %||% "Model selection unavailable."
      ))
    }

    dt_table(m, page_length = 12)
  })

  output$rates_plot <- renderPlot(res = 96, {
    req(analysis())
    if (nrow(analysis()$instant) > 0) {
      safe_plot(plot_growth_rates(
        analysis()$instant, type = "instantaneous",
        metrics = c("agr_inst", "rgr_inst", "nar_inst", "algr_inst", "rlgr_inst")
      ), "Rates plot unavailable.")
    } else if (nrow(analysis()$classic) > 0) {
      safe_plot(plot_growth_rates(
        analysis()$classic, type = "classic",
        metrics = c("agr", "rgr", "nar", "algr", "rlgr")
      ), "Rates plot unavailable.")
    } else {
      print(empty_plot("Rates unavailable."))
    }
  })

  output$classic_tbl <- renderDT({
    req(analysis())
    if (nrow(analysis()$classic) > 0) {
      dt_table(head(analysis()$classic, 200), page_length = 10)
    } else {
      info_table("Classic rates", "warning", analysis()$classic_error %||% "Classic rates unavailable for this setup.")
    }
  })

  output$instant_tbl <- renderDT({
    req(analysis())
    if (nrow(analysis()$instant) > 0) {
      dt_table(head(analysis()$instant, 200), page_length = 10)
    } else {
      info_table("Instant rates", "warning", analysis()$instant_error %||% "Instant rates unavailable for current setup.")
    }
  })

  # --- Classical interval --------------------------------------------------

  output$classic_interval_plot <- renderPlot(res = 96, {
    req(analysis())
    fit_c <- analysis()$classic_interval

    if (!is.null(fit_c) && inherits(fit_c, "phytogrow_classical") && nrow(fit_c$results) > 0) {
      safe_plot(plot(fit_c, metric = "rgr"), "Classical interval plot unavailable.")
    } else {
      print(empty_plot(analysis()$classic_interval_error %||% "Classical interval analysis unavailable."))
    }
  })

  output$classic_interval_tbl <- renderDT({
    req(analysis())
    fit_c <- analysis()$classic_interval

    if (!is.null(fit_c) && inherits(fit_c, "phytogrow_classical")) {
      dt_table(head(generics::tidy(fit_c), 400), page_length = 12)
    } else {
      info_table(
        "Classical interval", "warning",
        analysis()$classic_interval_error %||% "Classical interval analysis unavailable."
      )
    }
  })

  output$classic_checks_tbl <- renderDT({
    req(analysis())
    fit_c <- analysis()$classic_interval

    if (!is.null(fit_c) && inherits(fit_c, "phytogrow_classical")) {
      dt_table(fit_c$checks, page_length = 10)
    } else {
      info_table("Classical checks", "info", "No classical checks available.")
    }
  })

  # --- Partition and comparisons -------------------------------------------

  output$partition_plot <- renderPlot(res = 96, {
    req(analysis())
    part <- analysis()$partition

    if (nrow(part) > 0) {
      safe_plot(plot_partition(
        part,
        facet_var = if ("treatment" %in% names(part)) "treatment" else NULL
      ), "Partition plot unavailable.")
    } else {
      print(empty_plot(analysis()$partition_error %||% "Partition output unavailable."))
    }
  })

  output$partition_tbl <- renderDT({
    req(analysis())
    if (nrow(analysis()$partition) > 0) {
      dt_table(analysis()$partition, page_length = 10)
    } else {
      info_table("Partition", "warning", analysis()$partition_error %||% "Partition output unavailable.")
    }
  })

  output$compare_tbl <- renderDT({
    req(analysis())
    if (nrow(analysis()$compare) > 0) {
      dt_table(analysis()$compare, page_length = 10)
    } else {
      info_table("Comparisons", "warning", analysis()$compare_error %||% "Comparisons unavailable for current setup.")
    }
  })

  # --- Formal curve comparison module --------------------------------------

  cc_obj <- reactive({
    req(analysis())
    ct <- analysis()$curve_test
    if (is.null(ct) || !inherits(ct, "phytogrow_curve_test")) NULL else ct
  })

  output$cc_verdict <- renderUI({
    req(analysis())
    ct <- cc_obj()

    if (is.null(ct)) {
      return(div(
        class = "alert alert-warning",
        analysis()$curve_test_error %||% "Curve comparison unavailable for this setup."
      ))
    }

    coincidence <- ct$tests[
      ct$tests$hypothesis == "coincidence" & !is.na(ct$tests$p_value), ,
      drop = FALSE
    ]

    if (nrow(coincidence) == 0) {
      return(div(class = "alert alert-warning", "The coincidence test could not be computed."))
    }

    stats_txt <- paste(
      sprintf(
        "%s = %.4f (df = %s, p = %s)",
        coincidence$test,
        coincidence$statistic,
        ifelse(
          is.na(coincidence$df_den),
          format(coincidence$df_num),
          paste0(format(coincidence$df_num), ", ", format(coincidence$df_den))
        ),
        format.pval(coincidence$p_value, digits = 4, eps = 1e-12)
      ),
      collapse = " &nbsp;&middot;&nbsp; "
    )

    rejected <- coincidence$p_value[1] < 0.05

    div(
      class = if (rejected) "alert alert-danger" else "alert alert-success",
      tags$strong(if (rejected) {
        "H0 of coincident curves is REJECTED at 5%: each treatment needs its own curve."
      } else {
        "H0 of coincident curves is NOT rejected at 5%: a single curve describes all treatments."
      }),
      tags$br(),
      HTML(stats_txt),
      tags$br(),
      tags$small(paste0(
        ct$response,
        if (isTRUE(ct$log_response)) " (natural log scale)" else "",
        " ~ ", ct$time_col, " by ", ct$group_var,
        " | model: ", ct$model,
        if (identical(ct$model, "poly")) paste0(" (degree ", ct$degree, ")") else "",
        " | n = ", ct$n_obs,
        " | parameters: reduced = ", ct$n_par[["reduced"]],
        ", full = ", ct$n_par[["full"]]
      ))
    )
  })

  output$cc_plot <- renderPlot(res = 96, {
    req(analysis())
    ct <- cc_obj()

    if (is.null(ct)) {
      print(empty_plot(analysis()$curve_test_error %||% "Curve comparison unavailable."))
    } else {
      safe_plot(plot(ct), "Curve comparison plot unavailable.")
    }
  })

  output$cc_tests <- renderDT({
    req(analysis())
    ct <- cc_obj()

    if (is.null(ct)) {
      return(info_table(
        "Curve comparison", "warning",
        analysis()$curve_test_error %||% "Curve comparison unavailable."
      ))
    }

    dt_table(ct$tests, page_length = 12)
  })

  output$cc_pairwise_tbl <- renderDT({
    req(analysis())
    ct <- cc_obj()

    if (is.null(ct)) {
      return(info_table("Pairwise coincidence", "info", "Not available."))
    }

    if (is.null(ct$pairwise) || nrow(ct$pairwise) == 0) {
      return(info_table(
        "Pairwise coincidence", "info",
        "Enable the pairwise comparisons checkbox in the sidebar to obtain contrasts between pairs of treatments."
      ))
    }

    dt_table(ct$pairwise, page_length = 12)
  })

  output$cc_params <- renderDT({
    req(analysis())
    ct <- cc_obj()

    if (is.null(ct) || nrow(ct$parameters) == 0) {
      return(info_table("Curve parameters", "info", "Not available."))
    }

    dt_table(ct$parameters, page_length = 12, scroll_x = FALSE)
  })

  # --- Templates -----------------------------------------------------------

  template_handler <- function(basename_csv) {
    downloadHandler(
      filename = function() basename_csv,
      content = function(file) {
        src <- system.file("extdata", basename_csv, package = "phytogrowR")
        if (nzchar(src)) {
          file.copy(src, file, overwrite = TRUE)
        } else {
          readr::write_csv(tibble::tibble(message = "Template not found."), file)
        }
      }
    )
  }

  output$dl_template_growth <- template_handler("growth_template.csv")
  output$dl_template_harvest <- template_handler("harvest_template.csv")

  # --- Exports -------------------------------------------------------------

  csv_handler <- function(basename_csv, get_data, get_error) {
    downloadHandler(
      filename = function() basename_csv,
      content = function(file) {
        req(analysis())
        dat <- get_data()

        if (is.null(dat) || nrow(dat) == 0) {
          readr::write_csv(
            tibble::tibble(message = get_error() %||% "Output unavailable."),
            file
          )
        } else {
          readr::write_csv(dat, file)
        }
      }
    )
  }

  output$download_classic <- csv_handler(
    "phytogrowR_classic_rates.csv",
    function() analysis()$classic,
    function() analysis()$classic_error
  )

  output$download_instant <- csv_handler(
    "phytogrowR_instant_rates.csv",
    function() analysis()$instant,
    function() analysis()$instant_error
  )

  output$download_partition <- csv_handler(
    "phytogrowR_partition.csv",
    function() analysis()$partition,
    function() analysis()$partition_error
  )

  output$download_compare <- csv_handler(
    "phytogrowR_comparisons.csv",
    function() analysis()$compare,
    function() analysis()$compare_error
  )

  output$download_classical_interval <- csv_handler(
    "phytogrowR_classical_interval.csv",
    function() {
      fit_c <- analysis()$classic_interval
      if (!is.null(fit_c) && inherits(fit_c, "phytogrow_classical")) {
        generics::tidy(fit_c)
      } else {
        NULL
      }
    },
    function() analysis()$classic_interval_error
  )

  output$download_curve_params <- csv_handler(
    "phytogrowR_curve_landmarks.csv",
    function() analysis()$curve_params,
    function() analysis()$curve_params_error
  )

  output$download_model_sel <- csv_handler(
    "phytogrowR_model_selection.csv",
    function() analysis()$model_sel,
    function() analysis()$model_sel_error
  )

  output$download_curve_test <- downloadHandler(
    filename = function() "phytogrowR_curve_comparison.csv",
    content = function(file) {
      req(analysis())
      ct <- cc_obj()

      if (is.null(ct)) {
        readr::write_csv(
          tibble::tibble(
            message = analysis()$curve_test_error %||% "Curve comparison unavailable."
          ),
          file
        )
        return(invisible(NULL))
      }

      out <- generics::tidy(ct)

      if (!is.null(ct$pairwise) && nrow(ct$pairwise) > 0) {
        # Columns are taken with `$` rather than the `.data` pronoun: this
        # script attaches only shiny, bslib, DT, dplyr, ggplot2, tidyr, readr
        # and tibble, none of which put `.data` in scope.
        pw <- ct$pairwise
        pw <- tibble::tibble(
          hypothesis = "pairwise coincidence",
          term = paste(pw$group1, "vs", pw$group2),
          test = pw$test,
          df_num = pw$df_num,
          df_den = pw$df_den,
          rss_reduced = NA_real_,
          rss_full = NA_real_,
          statistic = pw$statistic,
          p_value = pw$p_value,
          p_adj = pw$p_adj,
          model = ct$model,
          response = ct$response,
          group_var = ct$group_var,
          log_response = ct$log_response
        )

        out <- dplyr::bind_rows(out, pw)
      }

      readr::write_csv(out, file)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() {
      ext <- switch(
        input$report_format,
        html_document = "html",
        word_document = "docx",
        pdf_document = "pdf",
        "html"
      )
      paste0("phytogrowR_report.", ext)
    },
    content = function(file) {
      req(analysis())
      dat <- analysis()$dat

      report_group <- if ("treatment" %in% names(dat)) {
        "treatment"
      } else if ("genotype" %in% names(dat)) {
        "genotype"
      } else {
        "group_id"
      }

      growth_report(
        data = dat,
        output_file = file,
        output_format = input$report_format,
        group_var = report_group,
        curve_method = input$fit_method,
        quiet = TRUE
      )
    }
  )
}

shinyApp(ui, server)
