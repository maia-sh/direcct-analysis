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

# NOTE: deprecated since added additional cd screening
# Check that analysis pop screening matches
# if (!nrow(filter(trials_screening, is_analysis_pop)) == nrow(trials)){
#   stop("There is a discrepancy in the analysis pop screening!")
# }

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
label_crossreg <- glue('Cross-Registrations\n(n = {n_crossreg})')
label_pre2020 <- glue('Registered Prior to 2020\n(n = {n_pre2020})')
label_nonintervention <- glue('Not interventional\n(n = {n_nonintervention})')
label_withdrawn_auto <- glue('Withdrawn on ICTRP/registry\n(n = {n_withdrawn_auto})')
label_exphase1 <- glue('Manually excluded in Phase 1\n(n = {n_exphase1})')
label_incomplete <- glue('Any completion > 30 June 2021\n(n = {n_incomplete})')
label_nonct <- glue('Not a Clinical Trial\n(n = {n_nonct})')
label_noncovid <- glue('Not on Treatment/Prevention\n(n = {n_noncovid})')
label_withdrawn_manual <- glue('Withdrawn on Manual Review\n(n = {n_withdrawn_manual})')
label_cd_analysis_exclude <- glue('Last updated completion\n> 30 June 2021\n(n = {n_cd_analysis_exclude})')


# Prepare flowchart
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
edge [minlen = 5]
ictrp -> auto_pass -> manual_pass -> analysis

# EDGES BLANK
edge [dir = none, style = invis]
ictrp -> blank_1
blank_1 -> blank_2
blank_2 -> blank_3
blank_3 -> blank_4
blank_4 -> blank_5
blank_5 -> blank_6
blank_6 -> auto_pass
auto_pass -> blank_7
blank_7 -> blank_8
blank_8 -> blank_9
blank_9 -> manual_pass
manual_pass -> blank_10
blank_10 -> analysis
}

# EXCLUSION SUBGRAPH
subgraph excluded {

node [width = 2.4]

# NODES EXCLUSION
crossreg [label = '@@5']
pre2020 [label = '@@6']
nonintervention [label = '@@7']
withdrawn_auto [label = '@@8']
exphase1 [label = '@@9']
incomplete [label = '@@10']
nonct [label = '@@11']
noncovid [label = '@@12']
withdrawn_manual [label = '@@13']
cd_analysis_exclude [label = '@@14']
}

# EDGES EXCLUSION
blank_1 -> crossreg
blank_2 -> pre2020
blank_3 -> nonintervention
blank_4 -> withdrawn_auto
blank_5 -> exphase1
blank_6 -> incomplete
blank_7 -> nonct
blank_8 -> noncovid
blank_9 -> withdrawn_manual
blank_10 -> cd_analysis_exclude
}

# LABELS
[1]: label_ictrp
[2]: label_auto_pass
[3]: label_manual_pass
[4]: label_analysis
[5]: label_crossreg
[6]: label_pre2020
[7]: label_nonintervention
[8]: label_withdrawn_auto
[9]: label_exphase1
[10]: label_incomplete
[11]: label_nonct
[12]: label_noncovid
[13]: label_withdrawn_manual
[14]: label_cd_analysis_exclude
")

# Export image
dir_reporting_metadata <- fs::dir_create(here::here("data", "reporting", "metadata"))

flow_trials |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_pdf(fs::path(dir_reporting_metadata, "flow-trials.pdf"),
                 width = 150,
                 height = 500
  )

flow_trials |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_png(fs::path(dir_reporting_metadata, "flow-trials.png"),
                 width = 150*8,
                 height = 500*8
  )

# Clean up
rm(list=ls(pattern = "label_"))
rm(trials_screened)
