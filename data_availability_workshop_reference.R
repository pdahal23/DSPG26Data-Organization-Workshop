# ============================================================
# DSPG Summer 2026
# Workshop: Organizing and Documenting Data Availability
# Reference Script — Full Pipeline with Explanations
# ============================================================
# This script walks through the complete pipeline from
# downloading ACS data to building an interactive dashboard.
# You can run it section by section, or come back to it
# later as a reference when working on your own project.
#
# Sections:
#   1. Setting up packages
#   2. Understanding tidycensus and ACS variable codes
#   3. Downloading ACS data via the Census API
#   4. Loading and exploring the pre-downloaded data
#   5. Auditing variable availability across years
#   6. Adding human-readable labels and categories
#   7. Building the dashboard — four options
# ============================================================


# ── Section 1: Load packages ──────────────────────────────
#
# tidycensus: connects R to the Census Bureau API so you can
#   download ACS data directly without visiting data.census.gov
#
# tidyverse: a collection of R packages for data wrangling
#   and visualization. Includes dplyr, tidyr, readr, purrr,
#   ggplot2, and more.
#
# jsonlite: converts R objects to JSON format, which is
#   used to pass data to Claude for dashboard generation.
#
# reactable: creates interactive HTML tables
#
# htmlwidgets: saves R widgets as standalone HTML files
#
# shiny: builds interactive web apps with R

install.packages("tidycensus")
install.packages("tidyverse")
install.packages("jsonlite")
install.packages("reactable")
install.packages("htmlwidgets")
install.packages("shiny")

library(tidycensus)
library(tidyverse)
library(jsonlite)
library(reactable)
library(htmlwidgets)
library(shiny)


# ── Section 2: Understanding ACS Variable Codes ───────────
#
# The ACS organizes data into tables. Each table covers one
# topic (e.g. "Median Household Income"). Each table has
# many variables (cells) inside it, each measuring a
# different slice of that topic.
#
# Variable codes follow this pattern:
#   B19013_001
#   │      │
#   │      └─ cell number (001 = the main estimate)
#   └──────── table number (B19013 = Median Household Income)
#
# The "B" prefix means it's a base table with full detail.
# Tables starting with "S" are subject tables (pre-summarized).
# Tables ending in "PR" are Puerto Rico specific — avoid these
# if you are working with US state data.
#
# Use load_variables() to search for the codes you need.
# This downloads a full list of all ACS variables for a
# given year and survey type.

vars_lookup <- load_variables(2022, "acs5", cache = TRUE)
# cache = TRUE saves the result locally so you don't have
# to re-download it every time you run this line.

# View the full list as a spreadsheet in RStudio
View(vars_lookup)

# Search for income-related variables
vars_lookup |>
  filter(str_detect(label, "income|Income")) |>
  select(name, label, concept) |>
  head(10) |>
  View()
# name    = the variable code to use in get_acs()
# label   = what the variable measures (!!! separates hierarchy levels)
# concept = the name of the table this variable belongs to

# Search for broadband variables
vars_lookup |>
  filter(str_detect(concept, "INTERNET|internet|broadband|Broadband")) |>
  select(name, label, concept) |>
  View()


# ── Section 3: Downloading ACS Data via the API ───────────
#
# get_acs() is the main function in tidycensus.
# It sends a request to the Census API and returns a
# clean dataframe with one row per geographic unit.
#
# Key arguments:
#   geography = the unit of analysis
#               options: "county", "state", "tract", "zcta"
#   variables = one or more ACS variable codes
#   state     = filter to a specific state (optional)
#   year      = the end year of the 5-year estimate period
#               e.g. year=2022 → 2018-2022 5-year ACS
#   survey    = "acs5" for 5-year, "acs1" for 1-year estimates
#               Note: 1-year estimates are only available for
#               areas with population > 65,000

# Pull a single variable for a single year
va_income_2022 <- get_acs(
  geography = "county",      # county-level data
  variables = "B19013_001",  # median household income
  state     = "VA",          # Virginia counties only
  year      = 2022,          # 2018-2022 5-year ACS
  survey    = "acs5"         # 5-year estimates
)

# Look at what came back
head(va_income_2022)
# GEOID    = unique county identifier (FIPS code)
# NAME     = county name
# variable = the variable code you requested
# estimate = the ACS estimate (e.g. median income in dollars)
# moe      = margin of error (measure of statistical uncertainty)

# Pull multiple variables at once
va_multi_2022 <- get_acs(
  geography = "county",
  variables = c(
    median_income = "B19013_001",   # you can rename variables
    poverty       = "B17001_002",   # using name = "code" syntax
    population    = "B01003_001"
  ),
  state  = "VA",
  year   = 2022,
  survey = "acs5"
)

glimpse(va_multi_2022)
# The data comes back in long format — one row per
# county × variable combination.

# ── Downloading multiple variables across multiple years ──
#
# To build a panel dataset (variables × counties × years),
# you need to loop over years.
#
# We pull each variable separately across all years.
# This is important because not all variables exist in all
# years — pulling them separately means a failure for one
# variable in one year doesn't block everything else.
#
# map_dfr() runs a function for each element in a list and
# stacks (row-binds) the results into one dataframe.
#
# tryCatch() handles errors gracefully — if a variable
# doesn't exist for a given year, it returns NULL instead
# of crashing the whole loop.

vars  <- c(
  "B19013_001",  # Median Household Income
  "B17001_002",  # Population Below Poverty
  "B15003_022",  # Bachelor's Degree
  "B27001_001",  # Health Insurance Universe
  "B01003_001",  # Total Population
  "B28002_004"   # Broadband Access (only available from 2017)
)

years <- 2010:2023

# NOTE: This loop makes ~84 API calls and takes several minutes.
# We have already run this and saved the result as
# acs_virginia_raw.csv. You do NOT need to run this section
# during the workshop — it is here for your reference.

raw_data <- map_dfr(vars, function(var) {
  map_dfr(years, function(yr) {
    tryCatch({
      get_acs(
        geography = "county",
        variables = var,
        state     = "VA",
        year      = yr,
        survey    = "acs5"
      ) |>
        mutate(year = yr)    # add a year column to track which year
    }, error = function(e) NULL)   # skip silently if variable doesn't exist
  })
})

# Save the result immediately so you never have to re-run the loop
write_csv(raw_data, "acs_virginia_raw.csv")


# ── Section 4: Load and Explore the Pre-Downloaded Data ───
#
# For the workshop exercise, we load the pre-downloaded CSV
# instead of running the API loop above.
# The CSV contains the same data that the loop would produce.

raw <- read_csv("acs_virginia_raw.csv")

# Quick overview of the data structure
glimpse(raw)
# 9,725 rows — one per county × variable × year combination
# 6 columns: GEOID, NAME, variable, estimate, moe, year

# How many rows per variable per year?
raw |> count(year, variable)

# Which variables are in the data?
raw |> distinct(variable)

# Check a specific variable and year
raw |>
  filter(variable == "B28002_004", year == 2016) |>
  nrow()
# Returns 0 — broadband was not tracked by ACS before 2017


# ── Section 5: Audit Variable Availability ────────────────
#
# Before cleaning or analyzing data, you need to know which
# variables actually exist across which years.
#
# The approach:
#   1. Get all variable-year combinations that exist in the CSV
#   2. Create a grid of ALL possible combinations (6 vars × 14 years)
#   3. Join them together — missing combinations become FALSE
#
# This is much faster than making API calls because the data
# is already downloaded. We just check what's in the file.

# Step 5a: What actually exists in the CSV
years_in_data <- raw |>
  distinct(variable, year)
# This gives us one row per variable-year combo that has data

# Step 5b: All possible combinations
all_combos <- expand_grid(
  variable = unique(raw$variable),    # all 6 variables
  year     = 2010:2023                # all 14 years
)
# expand_grid() creates every possible combination —
# 6 variables × 14 years = 84 rows

# Step 5c: Join and flag availability
audit_results <- all_combos |>
  left_join(
    years_in_data |> mutate(available = TRUE),
    by = c("variable", "year")
  ) |>
  mutate(available = replace_na(available, FALSE))
# left_join keeps all 84 rows from all_combos.
# Rows that matched years_in_data get available = TRUE.
# Rows with no match get available = NA, replaced with FALSE.

# Check the results
audit_results |>
  filter(available == FALSE) |>
  arrange(variable, year)
# You should see:
#   B15003_022 missing in 2010-2011
#   B27001_001 missing in 2010-2011
#   B28002_004 missing in 2010-2016


# ── Section 6: Add Labels and Categories ──────────────────
#
# Variable codes like "B19013_001" are not human-readable.
# We create a metadata lookup table with:
#   label    → plain English name for the variable
#   category → thematic grouping for color-coding in the dashboard
#
# Categories used here match the DSPG Infrastructure project:
#   Demographics      → who the population is
#   Socioeconomic     → income, poverty, education
#   Healthcare Access → insurance, health services
#   Infrastructure    → broadband, transportation

var_metadata <- tibble(
  variable = c("B19013_001", "B17001_002", "B15003_022",
               "B27001_001", "B01003_001", "B28002_004"),
  label    = c("Median Household Income", "Population Below Poverty",
               "Bachelor's Degree", "Health Insurance Universe",
               "Total Population", "Broadband Access"),
  category = c("Socioeconomic", "Socioeconomic", "Socioeconomic",
               "Healthcare Access", "Demographics", "Infrastructure")
)

# Join metadata and summarise into one row per variable
audit_final <- audit_results |>
  left_join(var_metadata, by = "variable") |>
  group_by(variable, label, category) |>
  summarise(
    n_years       = sum(available),
    years_present = list(year[available == TRUE]),
    missing_years = list(year[available == FALSE]),
    .groups = "drop"
  )

# Print a summary
audit_final |>
  select(label, category, n_years) |>
  print()

# Save for use in the dashboard QMD and Shiny app
saveRDS(audit_final, "audit_final.rds")


# ── Section 7: Build the Dashboard ────────────────────────
# Four options — from simplest to most polished.
# All four require audit_final to exist in your environment.
# Options 2, 3, and 4 also require audit_final.rds on disk.


# ── Option 1: reactable widget ────────────────────────────
#
# The quickest way to get a shareable HTML file.
# Produces a basic interactive table with search and filter.
# Works offline as a completely standalone HTML file.

widget <- reactable(
  audit_final,
  filterable = TRUE,
  searchable = TRUE
)

saveWidget(widget, "data_availability.html", selfcontained = TRUE)
# Open data_availability.html in any browser.


# ── Option 2: Quarto Dashboard ────────────────────────────
#
# Renders data_availability_dashboard.qmd which loads
# audit_final.rds and produces a styled, color-coded table.
# More polished than Option 1 — uses VT colors and custom styling.
# Still works offline as a standalone HTML file.
#
# Requires: audit_final.rds in your working directory.

quarto::quarto_render("data_availability_dashboard.qmd")
# Opens data_availability_dashboard.html in your browser.


# ── Option 3: Shiny App ───────────────────────────────────
#
# Runs app.R as a live interactive Shiny application.
# Filters update instantly without re-rendering.
# Good for exploring the data interactively in RStudio.
#
# Note: Shiny requires a running R session. Unlike Options
# 1 and 2, it cannot be saved as a standalone HTML file
# and shared directly.
#
# Requires: audit_final.rds in your working directory.

shiny::runApp("app.R")


# ── Option 4: R + Claude ──────────────────────────────────
#
# Export audit_final as JSON, paste into Claude with the
# prompt below to generate a polished color-coded
# interactive HTML dashboard with hover tooltips and filters.
#
# Step 1: Export the JSON

cat(toJSON(
  audit_final |>
    mutate(
      years_present = map(years_present, as.integer),
      missing_years = map(missing_years, as.integer)
    ),
  pretty = TRUE
))

# Step 2: Copy the JSON output from the console
# Step 3: Open claude.ai in your browser
# Step 4: Paste the prompt below, then paste the JSON at the bottom

# ── Claude Prompt ─────────────────────────────────────────
# You are a data visualization expert. I have audited variable
# availability in ACS 5-year estimates for Virginia counties
# from 2010-2023. Below is a JSON file with the results.
#
# Please create a single self-contained interactive HTML dashboard
# that shows:
# - A grid where rows = variables and columns = years
# - Color-coded cells by category (filled = available, grey = missing)
# - Hover tooltips showing variable name, category, years present,
#   and missing years
# - Filter buttons by coverage (full/partial) and by category
# - A summary stats bar at the top
#
# Use Virginia Tech colors: maroon (#861F41), orange (#E5751F),
# white background. Make it clean, professional, and suitable
# for a research presentation.
#
# Here is the JSON data: [PASTE YOUR JSON HERE]
# ─────────────────────────────────────────────────────────

# Step 5: Claude generates an HTML file
# Step 6: Download it and open in your browser


# ── For Your Own Project ──────────────────────────────────
#
# To apply this pipeline to your own project:
#
# 1. Identify which variables you need from your project proposal
# 2. Find the codes using load_variables() — search by keyword
# 3. Run the download loop (Section 3) with your variables
# 4. Run the audit (Section 5) to check year coverage
# 5. Add your own labels and categories (Section 6)
# 6. Generate the dashboard using whichever option fits your needs
# 7. Save the script — this becomes part of your project documentation
#
# Remember: always check coverage of all variables you need
# before building a panel that includes it. 
