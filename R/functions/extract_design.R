# This function takes a dataframe with a registry and study design from ICTRP and extracts certain characteristics
# It returns dataframe with same number of rows as input, all columns + design columns (with NA for unknown).
# It is based on code from James Smith (R) and Nicholas DeVito (Python)
# https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R
# https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py

extract_design <- function(data, registry = registry, study_design = study_design){

  # Add checks for registry names, and registry and study_design variables

  # Function extracts designs for following registries
  reg_ctgov <- c("ClinicalTrials.gov")
  reg_drks <- c("DRKS")
  reg_euctr <- c("EudraCT", "EUCTR")
  reg_irct <- c("IRCT")
  reg_chictr <- c("ChiCTR")
  reg_ctri <- c("CTRI")
  reg_lbctr <- c("LBCTR")
  reg_ntr <- c("NTR")
  reg_pactr <- c("PACTR")
  reg_repec <- c("REPEC")
  reg_rpcec <- c("RPCEC")
  reg_slctr <- c("SLCTR")
  reg_tctr <- c("TCTR")
  reg_anzctr <- c("ANZCTR")
  reg_isrctn <- c("ISRCTN")
  reg_cris <- c("CRiS", "KCT")
  reg_jprn <- c("JapicCTI", "jRCT", "UMIN-CTR")

  available_registries <-
    # ls(pattern = "^reg_") # doesn't work because gives variable names instead of strings
    c(
      reg_ctgov,
      reg_drks,
      reg_euctr,
      reg_irct,
      reg_chictr,
      reg_ctri,
      reg_lbctr,
      reg_ntr,
      reg_pactr,
      reg_repec,
      reg_rpcec,
      reg_slctr,
      reg_tctr,
      reg_anzctr,
      reg_isrctn,
      reg_cris,
      reg_jprn
    )

  # Registries in data but not covered by function
  missing_registries <- sort(setdiff(unique(data$registry), available_registries))

  message(glue::glue("The following registries in your dataset will not be processed by this function: ", glue::glue_collapse(missing_registries, ", ", last = ", and ")))

  # clinicaltrials.gov ------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L86-L126

  ct <-
    data |>
    filter(registry %in% reg_ctgov) |>
    mutate(
      # "Sequential Assignment" is variable in meaning so we don't assign that
      # here
      control_arm = case_when(
        grepl("Allocation: Randomized|Parallel|Crossover|Factorial", study_design) ~ "Yes",
        grepl("Single Group", study_design) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("Allocation: Non-Randomized", study_design) ~ "No",
        grepl("Allocation: Randomized", study_design) ~ "Yes",
        grepl("Allocation: N/A", study_design) ~ NA_character_,
        # manually reviewed some of these and they are non randomised
        grepl("Sequential Assignment", study_design) ~ "No"),
      # if randomisation missing, state unreported
      randomisation = case_when(
        is.na(randomisation) ~ NA_character_,
        TRUE ~ randomisation),
      subject_blind = ifelse(grepl("Participant", study_design),
                             "Yes", "No"),
      caregiver_blind = ifelse(grepl("Care Provider", study_design),
                               "Yes", "No"),
      investigator_blind = ifelse(grepl("Investigator", study_design),
                                  "Yes", "No"),
      outcome_blind = ifelse(grepl("Outcomes Assessor", study_design),
                             "Yes", "No"),
      # blinding is reported for all of the trials so according to our definition
      # we can assume analyst not blinded
      analyst_blind = "No",
      # all have blinding reported so we know blinded if not stated that open label
      blinding = ifelse(grepl("None \\(Open Label\\)|Masking: Open Label", study_design),
                        "No", "Yes"),
      primary_purpose = case_when(
        grepl("Primary purpose: Treatment", study_design,
              ignore.case = T) ~ "Treatment",
        grepl("Primary purpose: Prevention", study_design,
              ignore.case = T) ~ "Prevention",
        # if it says primary purpose but isn't one of above it is other
        grepl("Primary purpose", study_design,
              ignore.case = T) ~ "Other")
    )


  # drks --------------------------------------------------------------------
  # https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py#L114-L115
drks <-
    data |>
    filter(registry %in% reg_drks) |>
    mutate(
      randomisation = case_when(
        grepl("Allocation: Randomized", study_design) ~ "Yes",
        TRUE ~ "No" #TODO: check with ND
      )
    )


  # euctr -------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L133-L158

  eu <-
    data |>
    filter(registry %in% reg_euctr) |>
    mutate(
      # all meet these conditions
      control_arm = case_when(
        grepl("Controlled: yes|Parallel group: yes", study_design) ~ "Yes",
        grepl("Controlled: no", study_design) ~ "No"),
      # all meet these
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("Randomised: no", study_design) ~ "No",
        grepl("Randomised: yes", study_design) ~ "Yes",
        grepl("Randomised:<br>", study_design) ~ NA_character_),
      # all meet these
      blinding = case_when(
        grepl("blind: yes", study_design) ~ "Yes",
        grepl("Open: yes", study_design) ~ "No",
        grepl("blind:<br>", study_design) ~ NA_character_),
      # who specifically is blinded is not stated
      # many say double or single but not who
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = NA,
      analyst_blind = NA,
      primary_purpose = NA
    )


  # irct --------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L165-L188

  irct <-
    data |>
    filter(registry %in% reg_irct) |>
    mutate(
      randomisation = case_when(
        grepl("Randomization: Not randomized", study_design) ~ "No",
        grepl("Randomization: Randomized", study_design) ~ "Yes",
        grepl("Randomization: N/A", study_design) ~ NA_character_),
      control_arm = case_when(
        grepl("Assignment: Parallel|Assignment: Crossover",
              study_design) ~ "Yes",
        grepl("Assignment: Single", study_design) ~ "No"),
      blinding = case_when(
        grepl("Blinding: Single blinded|Blinding: Double blinded|Blinding: Triple blinded",
              study_design) ~ "Yes",
        grepl("Blinding: Not blinded", study_design) ~ "No"),
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = NA,
      analyst_blind = NA,
      primary_purpose = case_when(
        grepl("Purpose: Treatment", study_design) ~ "Treatment",
        grepl("Purpose: Prevention", study_design) ~ "Prevention",
        grepl("Purpose: Health service research|Purpose: Supportive|Purpose: Other",
              study_design) ~ "Other"))


  # chictr ------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L232-L250

  chictr_control <- "Parallel|Cross-over|Quasi-randomized controlled|Randomized parallel controlled trial|Factorial|Non randomized control|Dose comparison|Case-Control study"

  chictr <-
    data |>
    filter(registry %in% reg_chictr) |>
    mutate(
      control_arm = case_when(
        grepl(chictr_control, study_design) ~ "Yes",
        grepl("Single arm|Sequential", study_design) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("Non randomized|Quasi-randomized", study_design) ~ "No",
        grepl("Randomized",
              study_design, ignore.case = T) ~ "Yes"),
      blinding = NA,
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = NA,
      analyst_blind = NA,
      primary_purpose = NA)


  # ctri --------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L260-L298

  ctri_control <- "parallel|Crossover|Factorial Trial|Cluster Randomized Trial|Multiple Arm Trial|Controlled Trial"
  ctri_random <- "randomized|Computer generated randomization|Method of generating randomization sequence:Other|Stratified randomization|Stratified block randomization|Random Number Table"
  # we consider not applicable stated to mean no blinding
  ctri_no_blind <- "Blinding and masking:Open Label|Blinding and masking:Not Applicable"


  ctri <-
    data |>
    filter(registry %in% reg_ctri) |>
    mutate(
      control_arm = case_when(
        grepl(ctri_control, study_design, ignore.case = T) ~ "Yes",
        grepl("Single Arm Trial", study_design,
              ignore.case = T) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("Non-randomized", study_design,
              ignore.case = T) ~ "No",
        grepl(ctri_random, study_design,
              ignore.case = T) ~ "Yes"),
      # they all have a blinding field, and if not open label or
      # not applicable, they have some blinding
      blinding = case_when(
        grepl(ctri_no_blind, study_design,
              ignore.case = T) ~ "No",
        TRUE ~ "Yes"),
      subject_blind = ifelse(
        grepl("masking:Participant", study_design, ignore.case = T),
        "Yes", "No"),
      # we assume caregiver is no because at least one other
      # party is specified for all trials with blinding
      caregiver_blind = "No",
      investigator_blind = ifelse(
        grepl("Investigator", study_design, ignore.case = T),
        "Yes", "No"),
      outcome_blind = ifelse(
        grepl("Outcome Assessor", study_design, ignore.case = T),
        "Yes", "No"),
      # we consider the data entry operator to be the analyst
      analyst_blind = ifelse(
        grepl("Date-entry Operator", study_design, ignore.case = T),
        "Yes", "No"))


  # lbctr -------------------------------------------------------------------
  # https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py#L133

  lbctr <-
    data |>
    filter(registry %in% reg_lbctr) |>
    mutate(
      randomisation = case_when(
        grepl("Allocation: Randomized controlled trial",
              study_design) ~ "Yes",
      )
    )


# ntr ---------------------------------------------------------------------
 # https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py#L135-L136

  ntr <-
    data |>
    filter(registry %in% reg_ntr) |>
    mutate(
      randomisation = case_when(
        grepl("Randomized: Yes",
              study_design) ~ "Yes",
        grepl("<br>", study_design) ~ NA_character_ #TODO: ND check
      )
    )

  # pactr -------------------------------------------------------------------
  # https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py#L138-L140

  pactr <-
    data |>
    filter(registry %in% reg_pactr) |>
    mutate(
      randomisation = case_when(
        grepl("[rR]andomi[sz]ed",
              study_design) ~ "Yes",
        TRUE ~ NA_character_ #TODO: ND check
      )
    )


  # repec -------------------------------------------------------------------
  # Not in JS or ND code

  repec <-
    data |>
    filter(registry %in% reg_repec) |>
    mutate(
      randomisation = case_when(
        grepl("no[nt][ -]*[rR]andomi[sz]ed",
              study_design) ~ "No",
        grepl("[rR]andomi[sz]ed",
              study_design) ~ "Yes",
        TRUE ~ NA_character_ #TODO: ND check
      )
    )


# rpcec -------------------------------------------------------------------
  # https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/notebooks/diffable_python/Randomisation.py#L152-L153

  rpcec <-
    data |>
    filter(registry %in% reg_rpcec) |>
    mutate(
      randomisation = case_when(
        grepl("Allocation: Randomized trial|Allocation: Randomized controlled trial",
              study_design) ~ "Yes",
        grepl("Allocation: N/A: single arm study|Allocation: Non-randomized controlled trial.",
              study_design) ~ "No"
      )
    )


  # slctr -------------------------------------------------------------------
  # Not in JS or ND code

  slctr <-
    data |>
    filter(registry %in% reg_slctr) |>
    mutate(
      randomisation = case_when(
        grepl("Non-randomized",
              study_design) ~ "No",
        grepl("Randomized",
              study_design) ~ "Yes"
      )
    )


  # tctr --------------------------------------------------------------------
  # Not in JS or ND code

  tctr <-
    data |>
    filter(registry %in% reg_tctr) |>
    mutate(
      randomisation = case_when(
        grepl("Non-randomized",
              study_design) ~ "No",
        grepl("Randomized",
              study_design) ~ "Yes",
        grepl("N/A",
              study_design) ~ NA_character_
      )
    )


  # anzctr ------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L305-L334
  anzctr <-
    data |>
    filter(registry %in% reg_anzctr) |>
    mutate(
      control_arm = case_when(
        grepl("Assignment: Parallel|controlled trial",
              study_design) ~ "Yes",
        grepl("Assignment: Single group",
              study_design) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("Non-randomised",
              study_design) ~ "No",
        grepl("Randomised controlled trial",
              study_design) ~ "Yes"),
      blinding = case_when(
        grepl("Masking: Open",
              study_design) ~ "No",
        grepl("Masking: Blinded",
              study_design) ~ "Yes"),
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = NA,
      analyst_blind = NA,
      primary_purpose = case_when(
        grepl("Purpose: Treatment",
              study_design) ~ "Treatment",
        grepl("Purpose: Prevention",
              study_design) ~ "Prevention",
        grepl("Purpose: Diagnosis",
              study_design) ~ "Other"))


  # isrctn ------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L343-L368

  isrctn <-
    data |>
    filter(registry %in% reg_isrctn) |>
    mutate(
      randomisation = case_when(
        grepl("non-random", study_design,
              ignore.case = T) ~ "No",
        grepl("random", study_design,
              ignore.case = T) ~ "Yes"),
      control_arm = case_when(
        randomisation == "Yes" ~ "Yes",
        grepl("factorial-design", study_design,
              ignore.case = T) ~ "Yes",
        grepl("single-arm|historical control", study_design) ~ "No"),
      blinding = case_when(
        grepl("blind", study_design,
              ignore.case = T) ~ "Yes",
        grepl("open", study_design,
              ignore.case = T) ~ "No"),
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = NA,
      analyst_blind = NA,
      primary_purpose = case_when(
        grepl("Treatment", study_design) ~ "Treatment",
        grepl("Prevention", study_design) ~ "Prevention",
        grepl("\\(Other", study_design) ~ "Other"))


  # kct ---------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L374-L397

  cris <-
    data |>
    filter(registry %in% reg_cris) |>
    mutate(
      control_arm = case_when(
        grepl("Intervention Model : Parallel",
              study_design) ~ "Yes",
        grepl("Intervention Model : Single Group",
              study_design) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("RCT", study_design) ~ "Yes"),
      blinding = case_when(
        grepl("Masking : Open", study_design) ~ "No",
        grepl("Masking : Double", study_design) ~ "Yes"),
      subject_blind = case_when(
        grepl("Subject", study_design) ~ "Yes"),
      caregiver_blind = NA,
      investigator_blind = case_when(
        grepl("Investigator", study_design) ~ "Yes"),
      outcome_blind = case_when(
        grepl("Outcome Accessor", study_design) ~ "Yes"),
      primary_purpose = case_when(
        grepl("Primary Purpose : Treatment", study_design) ~ "Treatment",
        grepl("Primary Purpose : Prevention", study_design) ~ "Prevention",
        grepl("Primary Purpose : Supportive Care", study_design) ~ "Other"))


  # jprn --------------------------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L195-L225

  # define terms used to describe trials
  jprn_control <- "parallel|Cross-over|Crossover|placebo-controlled|randomized controlled trial"
  jprn_no_control <- "single arm|non-controlled|Uncontrolled|No control group"
  jprn_blind <- "double-blind|double-masked|double blind|single blind|observer-blind|single-blind|evaluator-blind"

  jprn <-
    data |>
    filter(registry %in% reg_jprn) |>
    mutate(
      control_arm = case_when(
        grepl(jprn_control, study_design, ignore.case = T) ~ "Yes",
        grepl(jprn_no_control, study_design, ignore.case = T) ~ "No"),
      randomisation = case_when(
        control_arm == "No" ~ "No",
        grepl("non-randomized",
              study_design, ignore.case = T) ~ "No",
        grepl("randomized|Random allocation",
              study_design, ignore.case = T) ~ "Yes"),
      blinding = case_when(
        grepl(jprn_blind, study_design, ignore.case = T) ~ "Yes",
        grepl("open", study_design, ignore.case = T) ~ "No"),
      subject_blind = NA,
      caregiver_blind = NA,
      investigator_blind = NA,
      outcome_blind = case_when(
        grepl("observer-blind|evaluator-blind",
              study_design, ignore.case = T) ~ "Yes"),
      analyst_blind = NA,
      primary_purpose = case_when(
        grepl("prevention purpose", study_design,
              ignore.case = T) ~ "Prevention",
        grepl("treatment purpose", study_design,
              ignore.case = T) ~ "Treatment"))


  # randomization keywords --------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L423-L454

  non_random <- c("Non-randomized", "Randomized: No", "nonrandomized",
                  "Quasi-randomized")

  random <- c("controlled-randomized",
              "Randomised,Permuted block randomization",
              "(?i)A randomised",
              "Simple randomization",
              "randomized, double-blind",
              "Randomised,Simple randomization",
              "Randomized clinical trial",
              "Randomized controlled trial",
              "a randomized, active-controlled",
              "participants randomly allocated",
              "randomized-controlled trial",
              "Randomization will be stratified",
              "randomized-controlled",
              "(?i), Randomized",
              "Randomized trial",
              "^Randomized",
              "Simple randomization",
              "randomized-controlled",
              ",Randomised",
              "randomised- controlled",
              "Phase 1 Randomized",
              "(?i)a randomized",
              "stratified-randomized",
              "A random controlled",
              "pilot randomized",
              "Prospective, Randomised"
  )


  # control arm keywords ----------------------------------------------------
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L562-L582

  control <- c("Parallel",
               "\\bcontrolled",
               "multi-arm",
               "multi arm",
               "two arm",
               "two-arm",
               "three arm",
               "three-arm",
               "Factorial",
               "Control: Placebo",
               "Control: Active",
               "randomized control trial"
  )

  non_control <- c("single-arm",
                   "single arm",
                   "uncontrolled",
                   "single group",
                   "Control: Historical"
  )

# combine registries ------------------------------------------------------
  out <-

    # Combine registries
    bind_rows(
      ct, drks, eu, irct, chictr, ctri, lbctr, ntr, pactr, repec, rpcec, slctr, tctr, anzctr, isrctn, cris, jprn,
      filter(data, registry %in% missing_registries)
  ) |>

    # If no control, can't be randomised, and if randomised, must have control
    # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L408-L415
    mutate(
      randomisation = case_when(
        control_arm == "No" ~ "No",
        TRUE ~ randomisation),
      control_arm = case_when(
        randomisation == "Yes" ~ "Yes",
        TRUE ~ control_arm)) |>

    # Search for any randomisationkeywords in titles and study design for trials missing randomization
    # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L456-L470
    mutate(randomisation = case_when(
      !is.na(randomisation) ~ randomisation,
      grepl(paste0(non_random, collapse = "|"),
            study_design) ~ "No",
      grepl(paste0(non_random, collapse = "|"),
            public_title) ~ "No",
      grepl(paste0(non_random, collapse = "|"),
            scientific_title) ~ "No",
      grepl(paste0(random, collapse = "|"),
            study_design) ~ "Yes",
      grepl(paste0(random, collapse = "|"),
            public_title) ~ "Yes",
      grepl(paste0(random, collapse = "|"),
            scientific_title) ~ "Yes")) |>

  # Search for any control arms keywords in titles and study design for trials missing randomization
  # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L584-L598
    mutate(randomisation = case_when(
      !is.na(randomisation) ~ randomisation,
      grepl(paste0(non_control, collapse ="|"), study_design,
            ignore.case = T) ~ "No",
      grepl(paste0(non_control, collapse ="|"), public_title,
            ignore.case = T) ~ "No",
      grepl(paste0(non_control, collapse ="|"), scientific_title,
            ignore.case = T) ~ "No",
      grepl(paste0(control, collapse ="|"), study_design,
            ignore.case = T) ~ "Yes",
      grepl(paste0(control, collapse ="|"), public_title,
            ignore.case = T) ~ "Yes",
      grepl(paste0(control, collapse ="|"), scientific_title,
            ignore.case = T) ~ "Yes")) |>

    # If no control, can't be randomised, and if randomised, must have control
    # https://github.com/worcjamessmith/COVID-trial-characteristics/blob/main/scripts/05_automated_extraction.R#L640-L647
    mutate(
      randomisation = case_when(
        control_arm == "No" ~ "No",
        TRUE ~ randomisation),
      control_arm = case_when(
        randomisation == "Yes" ~ "Yes",
        TRUE ~ control_arm))

}
