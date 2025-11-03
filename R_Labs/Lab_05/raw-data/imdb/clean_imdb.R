# %%
# https://developer.imdb.com/non-commercial-datasets/
library(tidyverse)
library(here)
basics <- read_tsv("~/Downloads/title.basics.tsv")
rating <- read_tsv("~/Downloads/title.ratings.tsv")

imdb <- basics |>
  inner_join(rating, by = "tconst") |>
  janitor::clean_names() |>
  select(
    -end_year,
    -original_title,
    title = primary_title,
    is_adult_movie = is_adult,
    release_year = start_year
  ) |>
  mutate(across(
    c(release_year, runtime_minutes, genres),
    \(x) na_if(x, "\\N")
  )) |>
  drop_na() |>
  filter(!str_detect(genres, "Short")) |>
  filter(title_type == "movie" | title_type == "tvMovie") |>
  filter(release_year >= 2010) |>
  mutate(
    release_year = parse_number(release_year),
    runtime_minutes = parse_number(runtime_minutes),
    is_tv_movie = title_type == "tvMovie",
    is_comedy = str_detect(genres, "Comedy"),
    is_drama = str_detect(genres, "Drama"),
    is_horror = str_detect(genres, "Horronr"),
    is_action = str_detect(genres, "Action"),
    is_documentary = str_detect(genres, "Documentary"),
    is_scifi = str_detect(genres, "Sci-Fi")
  ) |>
  select(
    title,
    release_year,
    average_rating,
    num_votes,
    runtime_minutes,
    everything()
  )

plot(
  average_rating ~ release_year,
  data = imdb,
  alp = 0.03
)

fs::dir_create(here("R_Labs/Lab_05/data/imdb/"))
write_csv(
  imdb,
  file = here("R_Labs/Lab_05/data/imdb/imdb.csv")
)

# %%
