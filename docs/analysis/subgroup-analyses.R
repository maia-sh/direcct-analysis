dir_sub <- fs::dir_create(here::here("data", "reporting", "subgroup-analyses"))

# 6-month blocks (i.e., Jan 2020-Jun 2020, July 2020-Dec 2020, & Jan 2021-Jun 2021)

# Prepare km dataset with semesters
km_semester <-
  km_main |>
  mutate(
    month_completion =
      lubridate::floor_date(date_completion, "month"),

    semester = case_when(
      month_completion < as.Date("2020-07-01") ~ 1,
      month_completion < as.Date("2021-01-01") ~ 2,
      month_completion < as.Date("2021-07-01") ~ 3,
    ),

    semester_fct = ordered(semester,
      labels = c("Jan 2020-Jun 2020", "July 2020-Dec 2020", "Jan 2021-Jun 2021")
    )
  )

readr::write_csv(km_semester, fs::path(dir_sub, "kaplan-meier-semester.csv"))

# Prepare reporting rates by semester
reporting_rates_semester <-
  km_semester |>
  add_count(semester, name = "n_trials") |>
  add_count(semester, publication_any, name = "n_results") |>
  filter(publication_any) |>
  distinct(semester, n_trials, n_results) |>
  mutate(p_results = n_results/n_trials)

reporting_rates_semester_1 <- filter(reporting_rates_semester, semester == 1)
reporting_rates_semester_2 <- filter(reporting_rates_semester, semester == 2)
reporting_rates_semester_3 <- filter(reporting_rates_semester, semester == 3)

# Visualize trials by semester
plot_trials_semester <-
  km_semester |>
  ggplot(aes(
    month_completion,
    fill = semester_fct,
    color = publication_any
  )) +
  geom_bar(linewidth = 1.5)

# TODO
# preprints earlier vs. later
# how many preprints in each block and whether converted to
# time to pub earlier vs later

# Prepare km preprint/article dataset with semesters
km_preprint_article_semester <-
  km_preprint_article_combined |>
  left_join(select(km_semester, id, month_completion, semester, semester_fct), by = "id")

readr::write_csv(km_preprint_article_semester, fs::path(dir_sub, "kaplan-meier-preprint-to-article-semester.csv"))

# Prepare preprint-to-article rates by semester
preprint_to_article_rates_semester <-
  km_preprint_article_semester |>
  distinct(id, .keep_all = TRUE) |>
  add_count(semester, name = "n_preprints") |>
  add_count(semester, publication_article, name = "n_articles") |>
  filter(publication_article) |>
  distinct(semester, n_preprints, n_articles) |>
  mutate(p_articles = n_articles/n_preprints)

preprint_to_article_rates_semester_1 <- filter(preprint_to_article_rates_semester, semester == 1)
preprint_to_article_rates_semester_2 <- filter(preprint_to_article_rates_semester, semester == 2)
preprint_to_article_rates_semester_3 <- filter(preprint_to_article_rates_semester, semester == 3)

# Prepare median time preprint-to-article by semester
time_preprint_article_semester <-
  km_preprint_article_semester |>
  filter(publication_preprint & publication_article) |>
  group_by(semester) |>
  summarise(
    median = median(time_preprint_article),
    iqr = IQR(time_preprint_article)
  )
time_preprint_article_semester_1 <- filter(time_preprint_article_semester, semester == 1)
time_preprint_article_semester_2 <- filter(time_preprint_article_semester, semester == 2)
time_preprint_article_semester_3 <- filter(time_preprint_article_semester, semester == 3)
