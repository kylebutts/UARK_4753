# %%
# https://www.kaggle.com/datasets/reinerjasin/nba-2k25-player-complete-dataset?resource=download
library(tidyverse)
library(here)
raw <- here(
  "R_Labs/Lab_05/raw-data/nba_2k25_players/current_nba_players.csv"
) |>
  read_csv(show_col_types = FALSE)

# https://github.com/danielfernandoo07/nbacsv/blob/main/2023_nba_player_stats.csv
stats <- here(
  "R_Labs/Lab_05/raw-data/nba_2k25_players/2023_nba_player_stats.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names()

players <- raw |>
  select(
    name,
    team,
    nationality_1,
    jersey_number = jersey,
    position_1,
    position_2,
    archetype,
    height_ft = height_feet,
    weight_lb = weight_lbs,
    wingspan_ft = wingspan_feet,
    season_salary,
    years_in_the_nba,
    birthdate,
    # ratings
    rating_overall = overall,
    rating_close_shot = close_shot,
    rating_mid_range_shot = mid_range_shot,
    rating_three_point_shot = three_point_shot,
    rating_free_throw = free_throw,
    rating_shot_iq = shot_iq,
    rating_speed = speed,
    rating_agility = agility,
    rating_strength = strength,
    rating_stamina = stamina,
    rating_overall_durability = overall_durability,
    rating_pass_accuracy = pass_accuracy,
    rating_ball_handle = ball_handle,
    rating_pass_iq = pass_iq,
    rating_group_defense = group_defense,
    rating_interior_defense = interior_defense,
    rating_perimeter_defense = perimeter_defense,
    rating_steal = steal,
    rating_block = block,
    rating_offensive_rebound = offensive_rebound,
    rating_defensive_rebound = defensive_rebound
  ) |>
  mutate(
    birthdate = mdy(birthdate)
  )

players <- tidylog::left_join(
  players,
  stats |> select(-team),
  by = join_by(name == p_name)
)

fs::dir_create(here("R_Labs/Lab_05/data/nba_2k25_player_ratings/"))
write_csv(
  players,
  file = here("R_Labs/Lab_05/data/nba_2k25_player_ratings/players.csv")
)
