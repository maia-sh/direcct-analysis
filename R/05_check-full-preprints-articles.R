# We match preprints and articles for full results, which we did not explicitly do in our extractions. We do this via automation when possible (i.e., single full result, full results of only a single type, single medrxiv match with no additional full results) and otherwise manually.
# We use the medrxiv API to check for preprint-article matches, as well as to verify there are no missing articles in our extractions.

library(dplyr)
# install.packages("medrxivr")
library(medrxivr)

registrations <- readr::read_csv(here::here("data", "deduplicated", "registrations.csv"))
trials <- readr::read_csv(here::here("data", "deduplicated", "trials.csv"))
results <- readr::read_csv(here::here("data", "deduplicated", "results.csv"))

# Check preprint/article domains ------------------------------------------

domains_results <-
  results %>%
  select(id, pub_type, url) %>%
  mutate(
    domain =
      stringr::str_remove(url, "^https?://") %>%
      stringr::str_remove(., "^www.") %>%
      stringr::str_remove(., "\\.(com|org|io|gov|net).*")
  )

domains_preprints <-
  domains_results |>
  filter(stringr::str_detect(pub_type, "preprint")) %>%
  count(domain) %>%
  arrange(desc(n))

domains_articles <-
  domains_results |>
  filter(stringr::str_detect(pub_type, "article")) %>%
  count(domain) %>%
  arrange(desc(n))

# Check for medrxiv preprint/articles -------------------------------------

medrxiv_dois <-
  results |>
  filter(stringr::str_detect(url, "medrxiv")) |>
  distinct(doi)

# Get a copy of the medrxiv database using API, if not already downloaded
# Using the medrxiv API we checked for preprint-article matches on 17 September 2022. We added any missing articles to our database and noted the validated links.
# Note: snapshot from 2022-07-06 01:09

medrxiv_date <- "2022-09-17"
medrxiv_path <- here::here("data", "raw", paste0(medrxiv_date, "_medrxiv-snapshot.csv"))
medrxiv_data <-
  if(fs::file_exists(medrxiv_path)){
    readr::read_csv(medrxiv_path)
  } else {
    readr::write_csv(mx_api_content(to_date = medrxiv_date), medrxiv_path)
  }

# Check if any medrxiv dois missing from medrxiv data
missing_medrxiv_dois <-
  anti_join(medrxiv_dois, medrxiv_data, by = "doi") %>%
  semi_join(results, ., by = "doi")

if (nrow(missing_medrxiv_dois) > 0){
  stop("There are missing medrxiv dois from medrxiv data!")
}

# Get all publications of preprints
published_preprints <-
  semi_join(medrxiv_data, medrxiv_dois, by = "doi") |>
  distinct(preprint = doi, article  = published) |>
  mutate(
    article = na_if(article, "NA"),
    article = tolower(article)
  ) |>
  tidyr::drop_na(article) |>
  mutate(medrxiv_group = row_number(), .before = 1)

# How many preprints convert to articles?
nrow(published_preprints)

# What types of publications?
results |>
  semi_join(published_preprints, by = c("doi" = "preprint")) |>
  count(pub_type)

# Check whether any "converted" articles missing from extractions
missing_article <-
  published_preprints |>
  tidyr::pivot_longer(-medrxiv_group, values_to = "doi") |>
  anti_join(results, by = "doi") %>%
  semi_join(published_preprints, ., by = "medrxiv_group") |>
  left_join(select(results, id, username, doi), by = c("preprint" = "doi"))

if (nrow(missing_article) > 0){
  stop("There are missing article(s) 'converted' from preprint(s): ", missing_article$id)
}

# Prepare full results ----------------------------------------------------

full_results <-
  results |>

  # Limit to full results
  filter(stringr::str_detect(pub_type, "full")) |>

  # Add n full results, and n full results per type
  add_count(id, name = "n_results") |>
  add_count(id, pub_type, name = "n_results_type") |>
  relocate(n_results, n_results_type, .before = "id") |>
  arrange(id) |>

  # Add in comments
  left_join(select(trials, id, comments), by = "id")

# How many trials have how many full results of how many types?
count(full_results, n_results, n_results_type, name = "n_trials")

# Prepare automated full results publication groups -----------------------

# Use automated publication groups where possible; manually code remaining full results.
# Automated publication groups can be used for trials with:
# 1) 1 full result --> set group to 1
# 2) >1 result all of a single type --> set group to by earliest publication date
# 3) 1 full result pair in medrxiv and no additional full results --> set group to 1


## Trials with 1 full result ----------------------------------------------
# Set group to 1
single_full_result <-
  full_results |>
  filter(n_results == 1) |>
  mutate(pub_group =  1)


## Trials with >1 result all of a single type -----------------------------
# Set group by earliest publication date

# Note: tri02586 has 2 articles on same date
single_full_result_type <-
  full_results |>
  filter(n_results > 1) |>
  filter(n_results == n_results_type) |>

  # Arrange by publication date (earliest to latest) and add row number by trial
  arrange(id, date_publication) |>
  group_by(id) |>
  mutate(pub_group = row_number())


## Trials with medrxiv full results ---------------------------------------

# Add medrxiv info to relevant full results
medrxiv_full_result <-

  published_preprints |>
  tidyr::pivot_longer(-medrxiv_group, values_to = "doi") |>
  select(-name) %>%
  left_join(results, ., by = "doi") |>
  select(medrxiv_group, id, pub_type, doi, pmid, url, date_publication) |>

  # Limit to full results with medrxiv group
  filter(stringr::str_detect(pub_type, "full")) |>
  filter(!is.na(medrxiv_group)) |>

  # Add counts of medrxiv pairs per trial
  arrange(medrxiv_group) |>
  add_count(medrxiv_group, id, name = "n_pubs_medrxiv_group") |>
  add_count(id, name = "n_pubs_id") |>
  mutate(n_medrxiv_group_trial = n_pubs_id/n_pubs_medrxiv_group) |>

  # Add pub group based on medrxiv
  # If 1 full result pair, set group to 1
  # If >1 full result pair, set group to medrxiv_group
  mutate(pub_group = if_else(n_medrxiv_group_trial == 1, 1, medrxiv_group + 100))

# Trials with full results in medrxiv as well as *additional* results should be manually reviewed for publication group
trials_medrxiv_additional_full_results <-
  full_results |>

  # Remove medrxiv pairs
  anti_join(medrxiv_full_result, by = c("id", "pub_type", "doi")) |>

  # Limit to trials with same dois as medrxiv pairs
  semi_join(medrxiv_full_result, by = "id")

# Gather full results pub groups
full_results_pub_group_auto <-

  # Combine automated pub groups
  bind_rows(
    select(single_full_result, id, pub_type, doi, pmid, url, pub_group),
    select(single_full_result_type, id, pub_type, doi, pmid, url, pub_group),
    select(medrxiv_full_result, id, pub_type, doi, pmid, url, pub_group)
  ) |>

  # Check that there are no duplicates
  assertr::assert_rows(assertr::col_concat, assertr::is_uniq, everything()) %>%

  # Join automated pub type into full results and check there are no duplicates
  left_join(full_results, .) %>%
  assertr::verify(nrow(.) == nrow(full_results)) |>

  # Add flag for manual review, if:
  # >1 full result pair in medrxiv (indicated by pub_group > 100)
  # medrxiv results with additional non-medrxiv results
  # no automated group
  mutate(review_pub_group = if_else(
    pub_group > 100 | id %in% trials_medrxiv_additional_full_results$id | is.na(pub_group),
    TRUE, FALSE
  ))


# Check manual full results publication groups ----------------------------

# Prepare publications for manual pub group checks
full_results_to_code <-
  full_results_pub_group_auto |>
  filter(review_pub_group) |>
  select(
    "n_results", "n_results_type", "id", "trn", "username",
    "pub_type", "doi", "pmid", "url", "date_publication",  "comments",
    "pub_group", "review_pub_group"
  ) |>
  mutate(
    pub_group = tidyr::replace_na(as.character(pub_group), ""),
    pmid = tidyr::replace_na(as.character(pmid), "")
  ) |>
  mutate(notes = "")

readr::write_csv(full_results_to_code, here::here("data", "manual", "full-results-to-code.csv"))

# Use google identity (i.e., gmail) to access for google sheets
# Get google identity if locally stored as "google", if available
# Else ask user and store
google_id <-
  ifelse(
    nrow(keyring::key_list("google")) == 1,
    keyring::key_get("google"),
    keyring::key_set("google")
  )

message("Accessing googlesheets via: ", google_id)

# If new google identity, prompt user in web browser to authenticate
googlesheets4::gs4_auth(google_id)

# Get manual coded googlesheet
full_results_googlesheet <- "https://docs.google.com/spreadsheets/d/1_DjRGs2chS3eO9xnCBzjM5fQAzA2Fb_JgtFIlCDzcqk/"

full_results_pub_group_manual <-
  googlesheets4::read_sheet(full_results_googlesheet)

# Check for any uncoded, using url (since all results)
full_results_uncoded <-
  anti_join(
    full_results_to_code, full_results_pub_group_manual,
    by = c("id", "pub_type", "url")
  )

if (nrow(full_results_uncoded) > 0) {
  message("There are additional full results for preprint-article manual grouping!")
  googlesheets4::sheet_append(full_results_googlesheet, full_results_uncoded)
} else message("Preprint-article full results successfully manual grouped!")

# Check for any extra coded, using url (since all results)
full_results_extra_coded <-
  anti_join(
    full_results_pub_group_manual, full_results_to_code,
    by = c("id", "pub_type", "url")
  )

if (nrow(full_results_extra_coded) > 0) {
  stop("There are extra full results for preprint-article coded!")
}


# Prepare results with full results publication groups --------------------

# Combine automated and manual pub groups and check expected number of results
full_results_pub_group <-
  full_results_pub_group_auto |>
  filter(!review_pub_group) |>
  bind_rows(full_results_pub_group_manual) %>%
  assertr::verify(nrow(.) == nrow(full_results)) |>
  select(id, pub_type, url, full_pub_group = pub_group)

# Add full results pub groups into results
results_pub_group <-
  results |>
  left_join(full_results_pub_group, by = c("id", "pub_type", "url")) %>%
  assertr::verify(nrow(.) == nrow(results)) |>

  # full_pub_group should be non-NA if and only if full result
  pointblank::col_vals_not_null(
    full_pub_group,
    preconditions = ~ . %>% filter(stringr::str_detect(pub_type, "full"))
  ) |>
  pointblank::col_vals_null(
    full_pub_group,
    preconditions = ~ . %>% filter(!stringr::str_detect(pub_type, "full")),
  )

dir_processed <- fs::dir_create(here::here("data", "processed"))
readr::write_csv(results_pub_group, fs::path(dir_processed, "results.csv"))

# Explore pub groups ------------------------------------------------------
results_pub_group_full <-
  results_pub_group |>
  filter(stringr::str_detect(pub_type, "full"))

# TODO: Check with ND
# There are some (n = 5) trials with 2 preprints for 1 article
# In this case, we care only about the earliest preprint
results_pub_group_full |>
  janitor::get_dupes(id, full_pub_group, pub_type)

full_pub_groups <-
  results_pub_group_full |>

  mutate(pub_type = stringr::str_remove(pub_type, "full_results_")) |>
  select(id, full_pub_group, pub_type, date_publication) |>

  # Limit to earliest of each pub type per trial pub group
  dplyr::group_by(id, full_pub_group, pub_type) |>
  slice_min(order_by = date_publication, n = 1) |>
  ungroup() |>

  # Spread to row per pub group
  tidyr::pivot_wider(names_from = pub_type, values_from = date_publication)

# Check for preprints following articles --> none!
full_pub_groups |>
  add_row(
    id = "tri00000",
    full_pub_group = 1,
    journal_article = as.Date("2020-06-16"),
    preprint = as.Date("2020-10-09"),
    .before = 1
  ) |>
  filter(preprint > journal_article)
