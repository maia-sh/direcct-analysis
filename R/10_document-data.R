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


# Tables ------------------------------------------------------------------
# Create tables overview

dm_tables <- names(dm_direcct)

tables_elements <-
  tibble(
    table = dm_tables,
    n_col = purrr::map_int(1:length(dm_tables), \(n_tbl) ncol(dm_direcct[[n_tbl]])),
    n_row = purrr::map_int(1:length(dm_tables), \(n_tbl) nrow(dm_direcct[[n_tbl]]))
  )

tables_description <-
  tibble(table = dm_tables, description = "")

tables <-
  tables_elements |>
  left_join(tables_description, by = "table")

readr::write_csv(tables, fs::path(dir_reporting_metadata, "tables.csv"))


# Codebook ----------------------------------------------------------------

codebook_elements <- bind_rows(
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

# All tables in datamodel should be in codebook
# Note: "2021-07_registries" and "2022-04_registries" collapsed into "registries" in codebook since same variables

codebook_tables <-
  codebook_elements |>
  distinct(table) |>
  filter(!stringr::str_detect(table, ";")) |>
  pull()

if (!setequal(
  stringr::str_subset(dm_tables, "registries", negate = TRUE),
  setdiff(codebook_tables, "registries")
)) {stop("There is a mismatch between tables in datamodel and codebook!")}


codebook_description <-
  tibble(distinct(codebook_elements, variable), description = "")

codebook <-
  codebook_elements |>
  left_join(codebook_description, by = "variable")

readr::write_csv(codebook, fs::path(dir_reporting_metadata, "codebook.csv"))
