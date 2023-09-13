# Prepare screening functions ---------------------------------------------

# Apply screening criteria
# Returns list of inclusion counts and filtered dataframe
count_filter <- function(data, vars) {

  counts <-
    tibble(name = as.character(),
           value = as.logical(),
           n = as.integer()
    )

  for (var in vars) {
    counts <-
      data %>%
      count(.data[[var]]) %>%
      tidyr::pivot_longer(-n) %>%
      add_row(counts, .)

    data <- filter(data, .data[[var]])
  }

  list(data = data, counts = counts)

}

# Report screening summary counts
report_n <- function(counts, var, condition) {
  n <-
    counts %>%
    filter(name == var & value == condition) %>%
    pull(n)

  # If empty, count is 0
  if (rlang::is_empty(n)){n <- 0}

  n
}

# Screen trials -----------------------------------------------------------

screening_criteria <- c(
  "in_ictrp",
  "is_reg_2020",
  "is_intervention",
  "is_not_withdrawn_auto",
  "is_not_phase_1_manually_excluded",
  "is_any_rcd_cutoff_3",
  "is_clinical_trial_manual",
  "is_covid_manual",
  "is_not_withdrawn_manual",
  "is_cd_cutoff_3"
)

trials_screened <-  count_filter(trials_screening_main, screening_criteria)

trials <- trials_screened$data

# Tabularize trial screening counts ---------------------------------------

screening_counts <-
  trials_screened$counts |>
  mutate(value = tidyr::replace_na(value, FALSE)) |>
  group_by(name, value) |>
  summarize(n = sum(n), .groups = "drop") |>

  # Organize by screening order
  left_join(
    tibble(name = screening_criteria) |> mutate(step = row_number()),
    by = "name"
  ) |>
  arrange(step, desc(value)) |>

  # Rename "in_ictrp" to clarify as unique trials and change FALSE to reflect duplicates
  mutate(
    name = if_else(name == "in_ictrp", "unique_trial", name),
    n = if_else(name == "unique_trial" & value == FALSE, nrow(ictrp) - nrow(filter(trials_screening, in_ictrp)), n)
  ) |>

  # Add base ictrp
  add_row(name = "ictrp", value = TRUE, n = nrow(ictrp), step = 0, .before = 0)


# Report trial screening counts -------------------------------------------

n_ictrp <- report_n(screening_counts, "ictrp", TRUE)
n_auto_pass <- report_n(screening_counts, "is_any_rcd_cutoff_3", TRUE)
n_manual_pass <- report_n(screening_counts, "is_not_withdrawn_manual", TRUE)
n_analysis <- report_n(screening_counts, "is_cd_cutoff_3", TRUE)
n_crossreg <- report_n(screening_counts, "unique_trial", FALSE)
n_pre2020 <- report_n(screening_counts, "is_reg_2020", FALSE)
n_nonintervention <- report_n(screening_counts, "is_intervention", FALSE)
n_withdrawn_auto <- report_n(screening_counts, "is_not_withdrawn_auto", FALSE)
n_exphase1 <- report_n(screening_counts, "is_not_phase_1_manually_excluded", FALSE)
n_incomplete <- report_n(screening_counts, "is_any_rcd_cutoff_3", FALSE)
n_nonct <- report_n(screening_counts, "is_clinical_trial_manual", FALSE)
n_noncovid <- report_n(screening_counts, "is_covid_manual", FALSE)
n_withdrawn_manual <- report_n(screening_counts, "is_not_withdrawn_manual", FALSE)
n_cd_analysis_exclude <- report_n(screening_counts, "is_cd_cutoff_3", FALSE)


# Prepare flowchart -------------------------------------------------------

library(glue)

# Prepare labels
label_ictrp <- glue('Registered COVID-19 Studies\non ICTRP in July 2021\n(n = {n_ictrp})')
label_auto_pass <- glue('Passed Automated Inclusion\n(n = {n_auto_pass})')
label_manual_pass <- glue('Passed Manual Inclusion\n(n = {n_manual_pass})')
label_analysis <- glue('Final Dataset\n(n = {n_analysis})')
label_auto_exclude <- glue(
  'Cross-Registrations\t(n = {n_crossreg})
  Registered Prior to 2020\t(n = {n_pre2020})
  Not interventional\t(n = {n_nonintervention})
  Withdrawn on ICTRP/registry\t(n = {n_withdrawn_auto})
  Excluded in Phase 1\t(n = {n_exphase1})
  Any completion > 30 June 2021\t(n = {n_incomplete})'
)
label_manual_exclude <- glue(
  'Not a Clinical Trial\t(n = {n_nonct})
  Not on Treatment/Prevention\t(n = {n_noncovid})
  Withdrawn on Manual Review\t(n = {n_withdrawn_manual})'
)
label_cd_analysis_exclude <- glue('Completion Date > 30 June 2021\non Another Registration\n(n = {n_cd_analysis_exclude})')

flow_trials <- DiagrammeR::grViz("digraph trials {

# GRAPH
graph [layout = dot, rankdir = LR, splines = false]
node [shape = rectangle, width = 3, height = 1, fixedsize = true, penwidth = 1, fontname = Arial, fontsize = 12]
edge [penwidth = 1]

# INCLUSION SUBGRAPH
subgraph included {

# NODES INCLUSION
ictrp [label = '@@1']
auto_pass [label = '@@2']
manual_pass [label = '@@3']
analysis [label = '@@4']

# NODES BLANK
node [label = '', width = 0.01, height = 0.01, style = invis]

rank = same

# EDGES INCLUSION
edge [minlen = 2.5]
ictrp -> auto_pass -> manual_pass -> analysis

# EDGES BLANK
edge [dir = none, style = invis]
ictrp -> blank_1
blank_1 -> auto_pass
auto_pass -> blank_2
blank_2 -> manual_pass
manual_pass -> blank_3
blank_3 -> analysis
}

# EXCLUSION SUBGRAPH
subgraph excluded {

node [width = 3.5]

# NODES EXCLUSION
auto_exclude [label = '@@5', height = 1.5]
manual_exclude [label = '@@6']
cd_analysis_exclude [label = '@@7']

}

# EDGES EXCLUSION
blank_1 -> auto_exclude
blank_2 -> manual_exclude
blank_3 -> cd_analysis_exclude
}

# LABELS
[1]: label_ictrp
[2]: label_auto_pass
[3]: label_manual_pass
[4]: label_analysis
[5]: label_auto_exclude
[6]: label_manual_exclude
[7]: label_cd_analysis_exclude
")

# Export image
dir_reporting_metadata <- fs::dir_create(here::here("data", "reporting", "metadata"))

flow_trials |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_pdf(fs::path(dir_reporting_metadata, "flow-trials.pdf"),
                 width = 150,
                 height = 150
  )

flow_trials |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_png(fs::path(dir_reporting_metadata, "flow-trials.png"),
                 width = 150*10,
                 height = 150*10
  )

# Clean up
rm(list=ls(pattern = "label_"))
rm(trials_screened)
