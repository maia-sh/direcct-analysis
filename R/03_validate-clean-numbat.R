library(dplyr)
library(pointblank)


dir <- fs::dir_create(here::here("data", "raw", "numbat"))
dir_cleaned <- fs::dir_create(here::here("data", "cleaned"))

# Get latest export based on logs
loggit::set_logfile(fs::path(dir, "numbat-downloads.log"))

numbat_export_logs <- loggit::read_logs()

latest_export_date <-
  numbat_export_logs |>
  arrange(desc(timestamp)) |>
  slice_head(n = 1) |>
  pull(timestamp) |>
  as.Date.character()

dir_raw <- fs::path(dir, latest_export_date)

# Get trials with issues to be discussed by study team
# problem_trials <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1mvBJjK2wQIPixK2oorizs-fFGSp4YwZ3uil_FsXSzwM/edit#gid=0")


# Validate trials ---------------------------------------------------------

select_trial_vars <- function(tbl){
  tbl |>
  select(
    referenceid, db_id, trialid,
    username,
    timestamp_finished,
    is_clinical_trial_manual,
    is_covid_manual,
    is_not_withdrawn_manual,
    pcd_reg_manual,
    scd_reg_manual,
    incidental_screening,
    incidental_screening_changes,
    incidental_screening_comment,
    non_english,
    comments,
    unclear_screening,
    team_review,
    is_reconciled
  )
}

# Check for valid UTF8 comments, since pointblank can't handle non-UTF8
valiate_utf8 <- function(tbl) {

  if (nrow(filter(tbl, !is.na(incidental_screening_comment))) > 0) {
    filter(tbl, !validUTF8(tbl$incidental_screening_comment))
  }

  if (nrow(filter(tbl, !is.na(comments))) > 0) {
    filter(tbl, !validUTF8(tbl$comments))
  }
}

validate_trials <- function(tbl, tbl_name = NULL){

  tbl |>

    create_agent(
      tbl_name = tbl_name, label = "trials",
      actions = action_levels(warn_at = 0.001)
    ) |>

    # IDs should be non-na
    col_vals_not_null(
      vars(referenceid, db_id, trialid),
      label = "IDs exist"
    ) |>

    # Check that screening criteria are booleans (or na)
    col_is_logical(
      vars(is_clinical_trial_manual, is_covid_manual, is_not_withdrawn_manual),
      label = "Screening is T/F"
    ) |>

    # Check that non-english is true (or na)
    col_vals_equal(
      non_english, TRUE, na_pass = TRUE,
      label = "Non-english is T or NA"
    ) |>

    # If trial fails screening criterion, subsequent criteria should be NA
    col_vals_null(
      vars(is_covid_manual, is_not_withdrawn_manual),
      preconditions = ~ . %>% filter(!is_clinical_trial_manual),
      label = "Non-clinical-trials screened out"
    ) |>
    col_vals_null(
      vars(is_not_withdrawn_manual),
      preconditions = ~ . %>% filter(!is_covid_manual),
      label = "Non-COVID trials screened out"
    ) |>

    # Trial screening criteria should not be NA following TRUE (following only FALSE or NA)
    col_vals_not_null(
      vars(is_covid_manual),
      preconditions = ~ . %>% filter(is_clinical_trial_manual),
      label = "Clinical trials missing COVID criterion"
    ) |>
    col_vals_not_null(
      vars(is_not_withdrawn_manual),
      preconditions = ~ . %>% filter(is_clinical_trial_manual, is_covid_manual),
      label = "COVID trials missing withdrawn criterion"
    ) |>

    #TODO! https://stackoverflow.com/questions/70114227/check-rowwise-assertion-in-r-pointblank
    # Not all screening criteria should be NA
    # rows_complete(
    #   vars(is_clinical_trial_manual, is_covid_manual, is_not_withdrawn_manual),
    #   actions = action_levels()
    #   ) |>
    # col_vals_not_null(c(is_clinical_trial_manual, is_covid_manual, is_not_withdrawn_manual)) |>
    # conjointly(
    #   ~ col_vals_not_null(., is_clinical_trial_manual),
    #   ~ col_vals_not_null(., is_covid_manual),
  #   ~ col_vals_not_null(., is_not_withdrawn_manual)
  # ) |>
  #
  # assert_rows(num_row_NAs, within_bounds(0, 2), is_clinical_trial_manual, is_covid_manual, is_not_withdrawn_manual)

  # Review flags should be clear after reconciliation (but not after extraction)
  col_vals_null(
    vars(unclear_screening, team_review),
    label = "Not flagged for review"
  ) |>

    # Comments should not be [Left blank]
    col_vals_not_equal(
      comments, "[Left blank]", na_pass = TRUE,
      label = "Blank comments are not [Left blank]"
    ) |>

    # If incidental screening, all associated variables should be non-na
    col_vals_not_null(
      vars(incidental_screening_changes, incidental_screening_comment),
      preconditions = ~ . %>% filter(!is.na(incidental_screening)),
      label = "Incidental screening detailed"
    ) |>

    interrogate()
}

# scan_data(tbl)


# Trials 2 (reconciled) ---------------------------------------------------
phase <- 2
filename <- glue::glue("reconciliation-trials-{phase}")

reconciliation_trials_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(
    username = NA_character_,
    is_reconciled = TRUE
  ) |>
  select_trial_vars()

valiate_utf8(reconciliation_trials_2)

agent_reconciliation_trials_2 <- validate_trials(reconciliation_trials_2, tbl_name = filename)
# agent_reconciliation_trials_2

# reconciliation_trials_2 |>
#   filter(is.na(is_clinical_trial_manual) & is.na(is_covid_manual) & is.na(is_not_withdrawn_manual))

# get_data_extracts(agent_reconciliation_trials_2, i = 14)

# Trials 2 (extractions) --------------------------------------------------
phase <- 2
filename <- glue::glue("extraction-trials-{phase}")

extraction_trials_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # For trials not reconciled, we use extraction data
  anti_join(reconciliation_trials_2, by = "db_id") |>

  # For dual coded trials, use most recent extraction
  group_by(db_id) |>
  arrange(desc(timestamp_finished)) |>
  slice_head(n = 1) |>
  ungroup() |>

  mutate(is_reconciled = FALSE) |>
  select_trial_vars()

valiate_utf8(extraction_trials_2)

agent_extraction_trials_2 <- validate_trials(extraction_trials_2, tbl_name = filename)
# agent_extraction_trials_2

# flagged_trials <- get_data_extracts(agent_extraction_trials_2, i = 13)
# anti_join(flagged_trials, problem_trials, by = "db_id")

# Trials 3 (reconciled) ---------------------------------------------------
phase <- 3
filename <- glue::glue("reconciliation-trials-{phase}")

reconciliation_trials_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(
    username = NA_character_,
    is_reconciled = TRUE
  ) |>
  select_trial_vars()

valiate_utf8(reconciliation_trials_3)

agent_reconciliation_trials_3 <- validate_trials(reconciliation_trials_3, tbl_name = filename)
# agent_reconciliation_trials_3

# get_data_extracts(agent_reconciliation_trials_3, i = 13) |> mutate(issue = glue::glue("[3] {db_id}/{trialid}")) |> pull(issue)

# Trials 3 (extractions) --------------------------------------------------
phase <- 3
filename <- glue::glue("extraction-trials-{phase}")

extraction_trials_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # For trials not reconciled, we use extraction data
  anti_join(reconciliation_trials_3, by = "db_id") |>

  # For dual coded trials, use most recent extraction
  group_by(db_id) |>
  arrange(desc(timestamp_finished)) |>
  slice_head(n = 1) |>
  ungroup() |>

  mutate(is_reconciled = FALSE) |>
  select_trial_vars()

valiate_utf8(extraction_trials_3)

agent_extraction_trials_3 <- validate_trials(extraction_trials_3, tbl_name = filename)
# agent_extraction_trials_3

# get_data_extracts(agent_extraction_trials_3, i = 14)
# anti_join(flagged_trials, problem_trials, by = "db_id")

# Combine trials ----------------------------------------------------------

multiagent_trials <-
  create_multiagent(
    agent_reconciliation_trials_2, agent_extraction_trials_2, agent_extraction_trials_3
  )
# report_trials <-
#   get_multiagent_report(
#     multiagent_trials,
#     title = ":tbl_name:"
#   )

trials <-
  bind_rows(reconciliation_trials_2, extraction_trials_2, reconciliation_trials_3, extraction_trials_3) |>

  # Check that one row per trial
  assertr::assert(assertr::is_uniq, db_id) |>

  rename(id = db_id) |>

  # Trials failing any manual screening criteria are excluded
  mutate(is_manual_excluded = if_else(!is_clinical_trial_manual | !is_covid_manual | !is_not_withdrawn_manual, TRUE, FALSE))

trials |>
filter(is.na(is_clinical_trial_manual) & is.na(is_covid_manual) & is.na(is_not_withdrawn_manual))

readr::write_csv(trials, here::here(dir_cleaned, "trials.csv"))


# Validate results --------------------------------------------------------

# Prepare expected preprint domains
preprint_domains <- c(
  "medrxiv", "papers.ssrn", "researchsquare", "osf",
  "authorea", "biorxiv", "preprints", "scientificarchives"#, "ncbi.nlm.nih"
)

select_result_vars <- function(tbl){
  tbl |>
  select(
    referenceid, db_id, trialid,
    username,
    search_type,
    pub_type,
    search_engine,
    doi,
    pmid,
    url,
    date_publication,
    date_completion_reported,
    date_completion,
    date_final_enrollment,
    followup_primary,
    followup_secondary,
    estimated_cd_enrollment
  )
}

validate_results <- function(tbl, tbl_name = NULL){

  tbl |>

    # Extract domain from url
    mutate(
      domain =
        stringr::str_remove(url, "^https?://") %>%
        stringr::str_remove(., "^www.") %>%
        stringr::str_remove(., "\\.(com|org|io|gov|net).*")
    ) |>

    create_agent(
      tbl_name = tbl_name, label = "results",
      actions = action_levels(warn_at = 0.001)
    ) |>

    # IDs should be non-na
    col_vals_not_null(
      vars(referenceid, db_id, trialid),
      label = "IDs exist"
    ) |>

    # Search type, pub type, and url should be non-na
    col_vals_not_null(
      vars(search_type, pub_type, url),
      label = "Publication data exists"
    ) |>

    # Search engine should be non-na when search type is trn or keyword
    col_vals_not_null(
      search_engine,
      preconditions = ~ . %>% filter(search_type %in% c("trn", "keywords")),
      label = "Search engine exists when trn or keyword search"
    ) |>

    # Search engine should be na when search type is auto, registry, or cochrane
    col_vals_null(
      search_engine,
      preconditions = ~ . %>% filter(search_type %in% c("auto", "registry", "cochrane")),
      label = "No search engine when auto, registry, or cochrane search"
    ) |>

    # Dates should not have extra "-01"
    col_is_date(
      starts_with("date") & !ends_with("reported"),
      label = "Dates are valid"
    ) |>

    # Articles, preprints, and summary results should have a publication date
    col_vals_not_null(
      date_publication,
      preconditions = ~ . %>% filter(stringr::str_detect(pub_type, "article|preprint|summary")),
      label = "Articles, preprints, and summary results have publication date"
    ) |>

    # In general, results with pmid should have doi
    # Some publications known to not have doi so excepted
    col_vals_not_null(
      doi,
      preconditions = ~ . %>% filter(!is.na(pmid) & !db_id %in% c("tri03558", "tri03853", "tri00776", "tri04760")),
      label = "Results with pmid have doi (in general)"
    ) |>

    # Results with no completion date should not have completion date reported
    col_vals_null(
      date_completion_reported,
      preconditions = ~ . %>% filter(is.na(date_completion)),
      label = "No completion date reported when no completion date"
    ) |>

    # Results with no completion date reported should not have completion date
    col_vals_null(
      date_completion,
      preconditions = ~ . %>% filter(is.na(date_completion_reported)),
      label = "No completion date when no completion date reported"
    ) |>

    # Medrxiv dois should not have version number
    col_vals_regex(
      doi,
      regex = "[^v]\\d$",
      preconditions = ~ . %>% filter(stringr::str_detect(url, "medrxiv")),
      label = "Medrxiv dois don't include version number"
    ) |>

    # Check preprint domains, in preprints and not in articles
    col_vals_in_set(
      domain,
      set = preprint_domains,
      preconditions = ~ . %>% filter(stringr::str_detect(pub_type, "preprint")),
      label = "Preprints have expected domains"
    ) |>
    col_vals_not_in_set(
      domain,
      set = preprint_domains,
      preconditions = ~ . %>% filter(stringr::str_detect(pub_type, "article")),
      label = "Articles do not have expected preprint domains"
    ) |>

    interrogate()
}

# Results 2 (reconciled) --------------------------------------------------

phase <- 2
filename <- glue::glue("reconciliation-results-{phase}")

reconciliation_results_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(username = NA_character_) |>
  select_result_vars()

agent_reconciliation_results_2 <- validate_results(reconciliation_results_2, tbl_name = filename)
# agent_reconciliation_results_2

# get_data_extracts(agent_reconciliation_results_2, i = 17)# |> mutate(issue = glue::glue("[2] {db_id}/{trialid}")) |> pull(issue)

# Results 2 (extractions) -------------------------------------------------

phase <- 2
filename <- glue::glue("extraction-results-{phase}")

extraction_results_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # Limit to non-reconciled trials and most recent extraction
  semi_join(extraction_trials_2, by = c("db_id", "username")) |>
  select_result_vars()

agent_extraction_results_2 <- validate_results(extraction_results_2, tbl_name = filename)
# agent_extraction_results_2
# get_data_extracts(agent_extraction_results_2, i = 5) #|> anti_join(problem_trials, by = "db_id") |> mutate(issue = glue::glue("[2] {db_id}/{trialid}")) |> pull(issue)
# expect issues with 5 for tri02586 (maia-sh)

# Results 3 (reconciled) --------------------------------------------------

phase <- 3
filename <- glue::glue("reconciliation-results-{phase}")

reconciliation_results_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(username = NA_character_) |>
  select_result_vars()

agent_reconciliation_results_3 <- validate_results(reconciliation_results_3, tbl_name = filename)
# agent_reconciliation_results_3
# get_data_extracts(agent_reconciliation_results_3, i = 15) #|> mutate(issue = glue::glue("[2] {db_id}/{trialid}")) |> pull(issue)


# Results 3 (extractions) -------------------------------------------------

phase <- 3
filename <- glue::glue("extraction-results-{phase}")

extraction_results_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # Limit to non-reconciled trials and most recent extraction
  # Note: This also removes ghost extractions for unassigned coders (tri00319 and tri00324). Numbat deletes main but not sub-extractions.
  semi_join(extraction_trials_3, by = c("db_id", "username")) |>

  select_result_vars()

agent_extraction_results_3 <- validate_results(extraction_results_3, tbl_name = filename)
# agent_extraction_results_3
# get_data_extracts(agent_extraction_results_3, i = 15) |>
#   # anti_join(problem_trials, by = "db_id") |>
#   mutate(issue = glue::glue("[phase 3] {username}: {db_id}/{trialid}")) |>
#   pull(issue)
# results issues for tri00319 and tri00324 for many coders...but not showing up in extractions.. wrote to murph 2022-01-28 right now fixing by joining with trials TODO
# ex_tr_3 %>% filter(db_id %in% c("tri00319", "tri00324"))
# [phase 3] MollyPugh-Jones: tri01208/NCT04362111

# Combine results ---------------------------------------------------------

multiagent_results <-
  create_multiagent(
    agent_reconciliation_results_2, agent_extraction_results_2, agent_reconciliation_results_3, agent_extraction_results_3
  )

results <-
  bind_rows(reconciliation_results_2, extraction_results_2, reconciliation_results_3, extraction_results_3) |>

  # Standardize dois to lowercase
  mutate(doi = tolower(doi)) |>

  rename(id = db_id)

readr::write_csv(results, here::here(dir_cleaned, "results.csv"))

# Validate registrations --------------------------------------------------

prepare_registration_vars <- function(tbl){
  tbl |>
    select(
      referenceid, db_id, trialid,
      username,
      source_register,
      source,
      trn_numbat = trn
    ) |>

    # Remove any duplicates (i.e., same trial id included twice in same extraction)
    distinct(db_id, username, trn_numbat, source, .keep_all = TRUE) |>

    # Clean trns and get registry
    ctregistries::mutate_trn_registry(trn_numbat) |>


    # Add text to trns
    mutate(
      trn = if_else(registry == "EudraCT", stringr::str_c("EUCTR", trn), trn),
      trn = if_else(registry %in% c("JapicCTI", "jRCT", "UMIN-CTR"), stringr::str_c("JPRN-", trn), trn)
    )
}

validate_registrations <- function(tbl, tbl_name = NULL){

  tbl |>

    create_agent(
      tbl_name = tbl_name, label = "registrations",
      actions = action_levels(warn_at = 0.001)
    ) |>

    # TRN and source should be non-na
    col_vals_not_null(
      vars(source, trn_numbat),
      label = "TRN and source exist"
    ) |>

    # Check that TRNs are valid
    col_vals_equal(
      trn_numbat, vars(trn),
      label = "TRNs are valid"
    ) |>

    interrogate()
}


# Registrations 2 (reconciled) --------------------------------------------

phase <- 2
filename <- glue::glue("reconciliation-registrations-{phase}")

reconciliation_registrations_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(username = NA_character_) |>
  prepare_registration_vars()

agent_reconciliation_registrations_2 <- validate_registrations(reconciliation_registrations_2, tbl_name = filename)
# agent_reconciliation_registrations_2

# get_data_extracts(agent_reconciliation_registrations_2, i = 1)

# Registrations 2 (extractions) -------------------------------------------

phase <- 2
filename <- glue::glue("extraction-registrations-{phase}")

extraction_registrations_2 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # Limit to non-reconciled trials and most recent extraction
  # TODO: alternatively could include all extractions...
  semi_join(extraction_trials_2, by = c("db_id", "username")) #|>

  # NOTE: Limiting to non-reconciled means no extracted only
  # prepare_registration_vars()

# agent_extraction_registrations_2 <- validate_registrations(extraction_registrations_2, tbl_name = filename)
# agent_extraction_registrations_2

# get_data_extracts(agent_extraction_registrations_2, i = 1)

# Registrations 3 (reconciliation) -------------------------------------------

phase <- 3
filename <- glue::glue("reconciliation-registrations-{phase}")

reconciliation_registrations_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>
  mutate(username = NA_character_) |>
  prepare_registration_vars()

agent_reconciliation_registrations_3 <- validate_registrations(reconciliation_registrations_3, tbl_name = filename)
# agent_reconciliation_registrations_3

# get_data_extracts(agent_reconciliation_registrations_3, i = 3)

# Registrations 3 (extractions) -------------------------------------------

phase <- 3
filename <- glue::glue("extraction-registrations-{phase}")

extraction_registrations_3 <-
  readr::read_csv(fs::dir_ls(dir_raw, regexp = filename)) |>

  # For trials not reconciled, we use extraction data
  semi_join(extraction_trials_3, by = c("db_id", "username")) |>

  prepare_registration_vars()

agent_extraction_registrations_3 <- validate_registrations(extraction_registrations_3, tbl_name = filename)
# agent_extraction_registrations_3

# get_data_extracts(agent_extraction_registrations_3, i = 3) #|>
#   anti_join(problem_trials, by = "db_id") |>
#   mutate(issue = glue::glue("[phase 3] {username}: {db_id}/{trialid}")) |>
#   pull(issue)


# Combine registrations ---------------------------------------------------

multiagent_registrations <-
  create_multiagent(
    agent_reconciliation_registrations_2,
    # agent_extraction_registrations_2,
    agent_reconciliation_registrations_3,
    agent_extraction_registrations_3
  )


registrations <-
  bind_rows(reconciliation_registrations_2,
            extraction_registrations_2,
          reconciliation_registrations_3,
          extraction_registrations_3) |>

  select(id = db_id, trn, registry, username, source)

readr::write_csv(registrations, here::here(dir_cleaned, "registrations.csv"))
