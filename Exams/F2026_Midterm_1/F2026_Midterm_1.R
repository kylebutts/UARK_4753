# %%
library(tidyverse)
library(fixest)

# chocolate <- read_csv(
#   "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2022/2022-01-18/chocolate.csv"
# )
# chocolate <- chocolate |>
#   mutate(
#     cocoa_percent = parse_number(cocoa_percent)
#   )

spotify <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2020/2020-01-21/spotify_songs.csv"
)
spotify <- spotify |>
  mutate(genre = playlist_genre)

set.seed(20261020)
spotify_sample <- spotify |> slice_sample(prop = 0.04)

# %%
(p_rating_cocoa_percent <- ggplot() +
  geom_point(
    aes(x = danceability, y = track_popularity),
    data = spotify_sample
  ) +
  geom_smooth(
    aes(x = danceability, y = track_popularity),
    data = spotify_sample,
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#B3114B",
    linetype = "dashed",
    linewidth = 1.25
  ) +
  labs(
    x = "Spotify's `Danceability Index'",
    y = "Track Popularity (0 -- 100)"
  ) +
  kfbmisc::theme_kyle(base_size = 10, grid_minor = "h") +
  theme(axis.title = element_text(size = rel(1))))

kfbmisc::tikzsave(
  here::here(
    "Exams/F2026_Midterm_1/figures/plot_popularity_vs_danceability.pdf"
  ),
  plot = p_rating_cocoa_percent,
  width = 6,
  height = 3.2
)

# %%
feols(
  track_popularity ~ danceability,
  data = spotify,
  vcov = "hc1"
)

feols(
  track_popularity ~ i(genre, ref = "pop"),
  data = spotify,
  vcov = "hc1"
)
