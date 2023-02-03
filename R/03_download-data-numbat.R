dir <- fs::dir_create(here::here("data", "raw", "numbat"))
loggit::set_logfile(fs::path(dir, "numbat-downloads.log"))

# Set global variables for numbat reference sets for phase 2 and 3
PHASE_2_REFSET <- "26"
PHASE_3_REFSET <- "31"


# Export reference sets and assignments -----------------------------------

for (phase in list(c(2, PHASE_2_REFSET), c(3, PHASE_3_REFSET))){

  # Download reference set (if not yet downloaded)
  refset_path <- glue::glue("{dir}/refset-{phase[1]}.csv")
  if (!fs::file_exists(refset_path)) {
    message(glue::glue("Downloading phase {phase[1]} reference set"))
    readr::read_tsv(glue::glue("https://numbat.bgcarlisle.com/direcct/references/referenceset_{phase[2]}.csv"), show_col_types = FALSE) |>
      readr::write_csv(refset_path)
  }

  # Download assignments
  message(glue::glue("Downloading phase {phase[1]} assignments"))
  readr::read_tsv(glue::glue("https://numbat.bgcarlisle.com/direcct/assignments/save-assignments.php?refset={phase[2]}"), show_col_types = FALSE)  |>
    dplyr::rename(source_register = title) |>
    dplyr::filter(form == "Results Search") |>
    dplyr::select(-form) |>
    readr::write_csv(glue::glue(fs::dir_create(fs::path(dir, Sys.Date())), "/assignments-{phase[1]}.csv"))
}


# Export extractions and reconciliations ----------------------------------

export_numbat <- function(url, dir){
  step <-
    dplyr::case_when(
      stringr::str_detect(url, "extraction") ~ "extraction",
      stringr::str_detect(url, "final") ~ "reconciliation"
    )

  type <-
    dplyr::case_when(
      stringr::str_detect(url, "form_5") ~ "trials",
      stringr::str_detect(url, "sub_registrations") ~ "registrations",
      stringr::str_detect(url, "table_keyword_search") ~ "keywords",
      stringr::str_detect(url, "sub_result") ~ "results"
    )

  phase <-
    dplyr::case_when(
      stringr::str_detect(url, paste0("refset_", PHASE_2_REFSET)) ~ "2",
      stringr::str_detect(url, paste0("refset_", PHASE_3_REFSET)) ~ "3"
    )

  message(glue::glue("Downloading {step} {type} for phase {phase}"))
  loggit::loggit("INFO", url, step = step, type = type, phase = phase)

  date <- as.Date(fs::path_file(url))
  dir <- fs::dir_create(fs::path(dir, date))
  readr::read_tsv(url, na = c("", "NA", "NULL", "0000-00-00"), show_col_types = FALSE) |>
    readr::write_csv(fs::path(dir, paste(step, type, phase, sep = "-"), ext = "csv"))
}

numbat_urls <- c(

  # Phase 2 Extractions
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063656-form_5-refset_26-extractions.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063704-sub_registrations-refset_26-sub-extraction.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063712-table_keyword_search-refset_26-table-extraction.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063719-sub_result-refset_26-sub-extraction.tsv",

  # Phase 2 Reconciliations
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063726-form_5-refset_26-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063732-sub_registrations-refset_26-sub-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063740-table_keyword_search-refset_26-table-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063746-sub_result-refset_26-sub-final.tsv",

  # Phase 3 Extractions
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063215-form_5-refset_31-extractions.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063225-sub_registrations-refset_31-sub-extraction.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063234-table_keyword_search-refset_31-table-extraction.tsv",
"https://numbat.bgcarlisle.com/direcct/export/2023-01-05_064844-sub_result-refset_31-sub-extraction.tsv",

  # Phase 3 Reconciliations
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063253-form_5-refset_31-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063301-sub_registrations-refset_31-sub-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063310-table_keyword_search-refset_31-table-final.tsv",
  "https://numbat.bgcarlisle.com/direcct/export/2023-01-05_063318-sub_result-refset_31-sub-final.tsv"
)

purrr::walk(numbat_urls, export_numbat, dir = dir)