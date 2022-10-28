
<!-- README.md is generated from README.Rmd. Please edit that file -->

# direcct-analysis

## data

The `data` directory has the following structure of subdirectories:

    ├── cleaned
    │   ├── registrations.csv
    │   ├── results.csv
    │   └── trials.csv
    ├── deduplicated
    │   ├── registrations.csv
    │   ├── results.csv
    │   └── trials.csv
    ├── manual
    ├── processed
    │   └── results.csv
    └── raw
        └── numbat

`raw` contains data downloaded from other sources by
`01_download-data-raw.R` and extractions in Numbat by
`02_download-data-numbat.R`.

`manual` contains any data manually added to the repository. Some
additional manual data is read in from Google Sheets.

`cleaned` contains data after cleaning, including tidying names to for
consistency across tables.

`deduplicated` contains data with unique database identifiers (`id`) in
all tables, after deduplicating cross-registrations.
