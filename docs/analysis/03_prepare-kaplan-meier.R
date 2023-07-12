# Create dataframe with info on each trial's earliest result per and across types

# Prep kaplan meier dataframes --------------------------------------------

prepare_km <- function(trials){

  trials_cd <- select(trials, id, date_completion)

  km_data_long <-

    # Create df with row per trial per pubtype
    bind_rows(
      mutate(trials_cd, pub_type = "any"),
      mutate(trials_cd, pub_type = "article"),
      mutate(trials_cd, pub_type = "preprint"),
      mutate(trials_cd, pub_type = "summary"),
      mutate(trials_cd, pub_type = "interim_any")
    )|>
    relocate(pub_type, .after = "id") |>
    arrange(id) |>

    # Add in results cutoff date to calculate time to publication/censoring
    mutate(date_cutoff = RESULTS_CUTOFF) |>

    # Add in results
    left_join(results_km_all, by = c("id", "pub_type")) |>

    # Change NA to FALSE for no results
    mutate(publication = tidyr::replace_na(publication, FALSE)) |>

    # Calculate time to publication/censoring
    mutate(
      time_publication =
        if_else(publication,
                lubridate::as.duration(date_completion %--% date_publication)/ ddays(1),
                lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
        )
    )

  km_data_wide <-
    km_data_long |>
    tidyr::pivot_wider(
      names_from = pub_type,
      names_glue = "{.value}_{pub_type}",
      values_from = ends_with("publication")
    )

}
