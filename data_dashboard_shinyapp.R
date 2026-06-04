# ============================================================
# DSPG Summer 2026
# Workshop: Organizing and Documenting Data Availability
# Optional Extension — Shiny App Version
# ============================================================
# This is the same dashboard built as a Shiny app.
# It requires audit_final.rds to be in your working directory.
#
# To run: click "Run App" in RStudio, or run:
#   shiny::runApp("app.R")
#
# Note: Unlike the QMD version, this app requires a running
# R session. It cannot be shared as a standalone HTML file.
# ============================================================

library(shiny)
library(tidyverse)
library(reactable)

# ── Load data ─────────────────────────────────────────────
# Make sure you have run workshop_exercise.R first to
# create audit_final.rds in your working directory.

audit_final <- readRDS("audit_final.rds")

# ── Prepare display data ──────────────────────────────────
cat_colors <- c(
  "Demographics"      = "#861F41",
  "Socioeconomic"     = "#E5751F",
  "Healthcare Access" = "#2E7D8C",
  "Infrastructure"    = "#5B7A3A"
)

table_data <- audit_final |>
  mutate(
    coverage = paste0(n_years, "/14"),
    missing_text = map_chr(missing_years, function(x) {
      if (length(x) == 0)       "\u2014"
      else if (length(x) <= 3)  paste(x, collapse = ", ")
      else                      paste0(min(x), "\u2013", max(x))
    }),
    status = if_else(n_years == 14, "Full", "Partial")
  ) |>
  select(label, category, coverage, missing_text, status, n_years)

# ── UI ────────────────────────────────────────────────────
# The UI defines what the user sees:
#   - titlePanel: the page title
#   - sidebarLayout: splits the page into sidebar + main
#   - sidebarPanel: contains the filter controls
#   - mainPanel: contains the table output

ui <- fluidPage(

  # Page title and subtitle
  titlePanel(
    div(
      h2("ACS Variable Availability",
         style = "color: #861F41; font-family: Georgia, serif;
                  margin-bottom: 4px;"),
      p("Virginia Counties · 2010–2023",
        style = "color: #8a7070; font-size: 14px; margin-top: 0;")
    )
  ),

  # Orange rule under title — matches VT slide style
  tags$hr(style = "border-top: 3px solid #E5751F; margin-bottom: 20px;"),

  # Page-level styling
  tags$head(tags$style(HTML("
    body { font-family: Calibri, Arial, sans-serif; background: #faf8f5; }
    .well { background: white; border: 1px solid #e0d6cf; border-radius: 10px; }
    h4 { color: #861F41; font-family: Georgia, serif; }
  "))),

  sidebarLayout(

    # ── Sidebar: filter controls ────────────────────────
    sidebarPanel(
      width = 3,

      h4("Filters"),

      # Coverage filter
      # radioButtons lets the user pick one option at a time
      radioButtons(
        inputId  = "coverage_filter",
        label    = "Coverage",
        choices  = c("All", "Full (14/14)", "Partial"),
        selected = "All"
      ),

      hr(style = "border-color: #e0d6cf;"),

      # Category filter
      # checkboxGroupInput lets the user pick multiple categories
      checkboxGroupInput(
        inputId  = "cat_filter",
        label    = "Category",
        choices  = unique(table_data$category),
        selected = unique(table_data$category)
      ),

      hr(style = "border-color: #e0d6cf;"),

      # Summary stats — these update reactively when filters change
      h4("Summary"),
      uiOutput("summary_stats")
    ),

    # ── Main panel: the table ───────────────────────────
    mainPanel(
      width = 9,
      reactableOutput("availability_table")
    )
  )
)

# ── Server ────────────────────────────────────────────────
# The server defines the logic:
#   - input$... reads values from the UI controls
#   - output$... sends results back to the UI
#   - reactive() creates a value that updates when inputs change

server <- function(input, output, session) {

  # ── Filtered data ──────────────────────────────────────
  # This reactive expression filters table_data based on
  # whatever the user has selected in the sidebar.
  # It re-runs automatically whenever the inputs change.

  filtered_data <- reactive({
    data <- table_data

    # Apply coverage filter
    if (input$coverage_filter == "Full (14/14)") {
      data <- data |> filter(coverage == "14/14")
    } else if (input$coverage_filter == "Partial") {
      data <- data |> filter(coverage != "14/14")
    }

    # Apply category filter
    data <- data |> filter(category %in% input$cat_filter)

    data
  })

  # ── Summary stats ──────────────────────────────────────
  # These update reactively based on the filtered data.

  output$summary_stats <- renderUI({
    data <- filtered_data()
    n_full    <- sum(data$coverage == "14/14")
    n_partial <- sum(data$coverage != "14/14")

    tagList(
      div(style = "margin-bottom: 8px;",
        span(style = "font-weight: bold; color: #861F41;",
             nrow(data)),
        span(" variables shown", style = "color: #8a7070; font-size: 13px;")
      ),
      div(style = "margin-bottom: 8px;",
        span(style = "font-weight: bold; color: #2E7D8C;", n_full),
        span(" full coverage", style = "color: #8a7070; font-size: 13px;")
      ),
      div(
        span(style = "font-weight: bold; color: #E5751F;", n_partial),
        span(" partial coverage", style = "color: #8a7070; font-size: 13px;")
      )
    )
  })

  # ── Table ──────────────────────────────────────────────
  # renderReactable() builds the styled table from the
  # filtered data. It re-renders whenever filtered_data()
  # changes.

  output$availability_table <- renderReactable({
    reactable(
      filtered_data(),
      highlight       = TRUE,
      defaultPageSize = 10,
      style = list(fontFamily = "Calibri, Arial, sans-serif",
                   fontSize   = "15px"),
      theme = reactableTheme(
        headerStyle    = list(
          background = "#861F41",
          color      = "white",
          fontWeight = "bold",
          fontSize   = "13px"
        ),
        borderColor    = "#e0d6cf",
        stripedColor   = "#faf5f6",
        highlightColor = "#f3eeea",
        cellPadding    = "10px 16px"
      ),
      columns = list(
        label = colDef(
          name  = "Variable",
          width = 240,
          style = list(fontWeight = "500", color = "#1a0a0f")
        ),
        category = colDef(
          name  = "Category",
          width = 180,
          style = function(value) {
            list(color = cat_colors[value], fontWeight = "bold")
          }
        ),
        coverage = colDef(
          name  = "Coverage",
          width = 100,
          align = "center",
          style = function(value) {
            if (value == "14/14") list(color = "#2E7D8C", fontWeight = "bold")
            else                  list(color = "#E5751F", fontWeight = "bold")
          }
        ),
        missing_text = colDef(
          name  = "Missing Years",
          width = 160,
          align = "center",
          style = function(value) {
            if (value == "\u2014") list(color = "#8a7070")
            else                   list(color = "#861F41", fontWeight = "500")
          }
        ),
        status = colDef(
          name  = "Status",
          width = 110,
          align = "center",
          cell  = function(value) {
            color <- if (value == "Full") "#2E7D8C" else "#E5751F"
            bg    <- if (value == "Full") "#e8f4f6" else "#fdf0e8"
            div(style = paste0(
              "display:inline-block; padding:3px 10px; border-radius:100px;",
              "font-size:11px; font-weight:600;",
              "color:", color, "; background:", bg, ";"
            ), value)
          }
        ),
        n_years = colDef(show = FALSE)
      )
    )
  })
}

# ── Run the app ───────────────────────────────────────────
shinyApp(ui = ui, server = server)
