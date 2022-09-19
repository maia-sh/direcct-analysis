# Download file to `dir` within "data" directory, if not already downloaded
download_file <- function(file_url, file_name, dir = "raw"){
  dir_path <- fs::dir_create(fs::path_wd("data", dir))
  file_path <- fs::path(dir_path, file_name)
  if (!fs::file_exists(file_path)) {download.file(file_url, file_path)}
}


# ICTRP -------------------------------------------------------------------

# ICTRP 3
download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/data/cleaned_ictrp_1jul2021.csv?raw=true",
  "2021-07-01_ictrp.csv"
)

# ICTRP 2
download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/blob/master/data/cleaned_ictrp_16Dec2020.csv?raw=true",
  "2020-12-16_ictrp.csv"
)


# Auto results (1,  2, 3) -------------------------------------------------

download_file(
  "https://raw.githubusercontent.com/ebmdatalab/covid19_results_reporting/master/data/final_auto_15Sept2020.csv",
  "2020-09-15_auto_results_1.csv"
)

download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/blob/master/data/final_auto_24Feb2021.csv?raw=true",
  "2021-02-24_auto_results_2.csv"
)

download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/data/final_auto_14Jul2021.csv?raw=true",
  "2021-07-14_auto_results_3.csv"
)


# Phase 1 Data ------------------------------------------------------------

download_file(
  "https://raw.githubusercontent.com/maia-sh/direcct/master/data/reporting/results.csv",
  "results_1.csv"
)

download_file(
  "https://raw.githubusercontent.com/maia-sh/direcct/master/data/reporting/registrations.csv",
  "registrations_1.csv"
)

download_file(
  "https://raw.githubusercontent.com/maia-sh/direcct/master/data/reporting/trials.csv",
  "trials_1.csv"
)

# OTHER -------------------------------------------------------------------

# Registry 2
download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/raw/master/data/registry_data/registry_data_clean.csv",
  "2021-01_registries.csv"
)

# Registry 3
download_file(
  "https://raw.githubusercontent.com/ebmdatalab/direcct-phase2-python/phase_3_testing/data/registry_data/registry_data_clean_july21.csv",
  "2021-07_registries.csv"
)

# Registry 2022-04
# Registry data for all registrations of all trials we thought would be included in 2022-04
# Does not include trials we excluded or trials we later included
download_file(
  "https://github.com/ebmdatalab/direcct-phase2-python/raw/phase_3_testing/data/registry_data/registry_data_clean_apr22.csv",
  "2022-04_registries.csv"
)

# Metacovid
download_file(
  "https://raw.githubusercontent.com/maia-sh/metacovid/main/data/metacovid-trials.csv",
  "metacovid-trials.csv"
)

# Qualtrics
download_file(
  "https://raw.githubusercontent.com/maia-sh/direcct/master/data/raw/qualtrics-results-search.csv",
  "qualtrics-results-search.csv"
)

# ND checked registry data for crp/d-dimer regexes in https://github.com/ebmdatalab/direcct-phase2-python/blob/phase_3_testing/data/ictrp_data/COVID19-web_1July2021.csv?raw=true --> "2022-05-16_crp-dimer-check.csv"
