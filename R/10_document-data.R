# Visualize relational data models and create codebook

library(dplyr)

dir_reporting <- here::here("data", "reporting")
dir_reporting_metadata <- fs::dir_create(fs::path(dir_reporting, "metadata"))

# Datamodel ---------------------------------------------------------------

# https://dm.cynkra.com/articles/howto-dm-df.html

# install.packages("dm")
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
# install.packages("rsvg")
library(dm)

# Read in all reporting dataframes
for(file in fs::dir_ls(dir_reporting, regexp = "*.csv")){
  assign(
    file |>
      fs::path_file() |>
      fs::path_ext_remove(),
    readr::read_csv(file)
  )
}
rm(file)

# Create data model
dm_direcct <-

  # Add tables
  dm(
    `2021-07-01_ictrp`,
    `2021-07_registries`,
    `2022-04_registries`,
    `extraction-info`,
    `registrations`,
    `results`,
    `arms`,
    `screening-trials`,
    `completion-dates`
  ) |>

  # Add primary keys
  dm_add_pk(`screening-trials`, id) |>
  dm_add_pk(`registrations`, trn) |>
  dm_add_pk(`extraction-info`, id) |> # could include trn_extracted
  dm_add_pk(`2021-07_registries`, trn) |>
  dm_add_pk(`2022-04_registries`, trn) |>

  # Add foreign keys
  dm_add_fk(`results`, `id`, `screening-trials`) |>
  dm_add_fk(`arms`, `id`, `screening-trials`) |>
  dm_add_fk(`registrations`, `id`, `screening-trials`) |>
  dm_add_fk(`extraction-info`, `id`, `screening-trials`) |>
  dm_add_fk(`2021-07-01_ictrp`, `trn`, `registrations`) |>
  dm_add_fk(`2021-07_registries`, `trn`, `registrations`) |>
  dm_add_fk(`2022-04_registries`, `trn`, `registrations`) |>
  dm_add_fk(`completion-dates`, `id`, `screening-trials`)

# Check constraints
# dm_examine_constraints(dm_direcct)

# Visualize dm
dm_img <-
  dm_draw(dm_direcct, view_type = "all")

# Export image
dm_img |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_pdf(fs::path(dir_reporting_metadata, "datamodel.pdf"),
                 width = 297.5,
                 height = 463
  )

dm_img |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_png(fs::path(dir_reporting_metadata, "datamodel.png"),
                 width = 297.5*6,
                 height = 463*6
  )

# Codebook ----------------------------------------------------------------

# TODO: add screening-trials
# TODO: change to function with metaprogramming

codebook <- bind_rows(
  tibble(
    table = "registrations",
    variable = colnames(registrations),
    type = tolower(purrr::map_chr(registrations, class))
  ),
  tibble(
    table = "results",
    variable = colnames(results),
    type = tolower(purrr::map_chr(results, class))
  ),
  tibble(
    table = "arms",
    variable = colnames(arms),
    type = tolower(purrr::map_chr(arms, class))
  ),
  tibble(
    table = "extraction-info",
    variable = colnames(`extraction-info`),
    type = tolower(purrr::map_chr(

      # Issue with POSIX so change to Date
      mutate(`extraction-info`,
             timestamp_finished = lubridate::as_date(timestamp_finished)),
      class))
  ),
  tibble(
    table = "2021-07-01_ictrp",
    variable = colnames(`2021-07-01_ictrp`),
    type = tolower(purrr::map_chr(`2021-07-01_ictrp`, class))
  ),
  tibble(
    table = "2021-07_registries",
    variable = colnames(`2021-07_registries`),
    type = tolower(purrr::map_chr(`2021-07_registries`, class))
  ),
  tibble(
    table = "2022-04_registries",
    variable = colnames(`2022-04_registries`),
    type = tolower(purrr::map_chr(`2022-04_registries`, class))
  ),

  tibble(
    table = "completion-dates",
    variable = colnames(`completion-dates`),
    type = tolower(purrr::map_chr(`completion-dates`, class))
  ),

  tibble(
    table = "screening-trials",
    variable = colnames(`screening-trials`),
    type = tolower(purrr::map_chr(`screening-trials`, class))
  )
) |>

  # Reset to POSIX
  mutate(type = if_else(variable == "timestamp_finished", "POSIX", type)) |>

  # Visually inspected to check for consistent classes

  # 20xx-xx_registries match so collapse
  mutate(table = stringr::str_remove(table, "202[12]-0[47]_(?=registries)")) |>
  distinct() |>

  # Collapse variables across tables, in alphabetical order
  arrange(table) |>
  group_by(variable, type) |>
  summarize(table = stringr::str_c(table, collapse = ";"), .groups = "drop") |>
  relocate(table, .before = 1) |>

  # Arrange by tables, with `id` and then other variables across tables first
  arrange(variable != "id", desc(stringr::str_detect(table, ";")), table)

description <- tribble(
  ~variable, ~description,
  "web_address",
  "",

  "registry",
  "",

  "trn",
  "",

  "id",
  "",

  "countries",
  "",

  "date_enrollement",
  "",

  "date_registration",
  "",

  "euctr_country",
  "",

  "phase",
  "",

  "public_title",
  "",

  "recruitment_status",
  "",

  "retrospective_registration",
  "",

  "study_type",
  "",

  "target_enrollment",
  "",

  "comments",
  "",

  "dual_coded",
  "",

  "incidental_screening",
  "",

  "incidental_screening_changes",
  "",

  "incidental_screening_comment",
  "",

  "soc",
  "",

  "dual_coded_intervention",
  "",

  "timestamp_finished",
  "",

  "trn_extracted",
  "",

  "ids_old",
  "",

  "resolved",
  "",

  "last_updated",
  "",

  "other_results_1",
  "",

  "other_results_2",
  "",

  "pcd",
  "",

  "potential_other_results",
  "",

  "rcd",
  "",

  "reg_results_status",
  "",

  "registry_scrape_date",
  "",

  "scd",
  "",

  "tabular_results",
  "",

  "trial_status",
  "",

  "date_completion",
  "",

  "date_completion_reported",
  "",

  "date_final_enrollment",
  "",

  "date_publication",
  "",

  "doi",
  "",

  "estimated_cd_enrollment",
  "",

  "followup_primary",
  "",

  "followup_secondary",
  "",

  "full_pub_group",
  "",

  "pmid",
  "",

  "pub_type",
  "",

  "search_engine",
  "",

  "search_type",
  "",

  "url",
  "",

  "type",
  "",

  "control_type",
  "",

  "placebo_plus_soc",
  "",

  "category",
  "",

  "intervention",
  "",

  "intervention_plus_soc",
  "",

  "date_completion_rcd_latest",
  "",

  "date_completion_rcd_latest_pre_cutoff",
  "",

  "date_completion_last_updated_exclude_missing_update_date",
  "",

  "date_completion_last_updated_prefer_euctr",
  "",

  "status_complete_last_updated_prefer_euctr",
  "",

  "date_completion_ictrp",
  "",

  "date_completion_22_21_last_updated_prefer_euctr",
  "",

  "date_completion_study_last_updated_prefer_euctr",
  "",

  "date_completion_results",
  ""
)


codebook_description <-
  codebook |>
  left_join(description, by = "variable")

readr::write_csv(codebook_description, fs::path(dir_reporting_metadata, "codebook.csv"))

# Create tables overview
# TODO: add n rows, (n rows per study?), description, (domain?)
tibble(
  table = c(
    "2021-07-01_ictrp",
    "2021-07_registries",
    "2022-04_registries",
    "arms",
    "completion-dates",
    "extraction-info",
    "registrations",
    "results",
    "screening-trials"
  )
)

# codebook |>
#   distinct(table) |>
#   filter(!stringr::str_detect(table, ";"))
