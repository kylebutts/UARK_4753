# %%
library(tidyverse)
library(rvest)
library(here)
library(tinyplot)

start_date <- ymd("2024-08-01")
end_date <- ymd("2024-11-04")
dates <- tibble(
  date = seq(start_date, end_date, by = "1 day")
)

# %%
# https://www.racetothewh.com/president/polls National Polls
polls_html <- read_html(here("R_Days/Day_4/raw/racetothewh_tab.html")) |>
  html_elements("tr")

# row <- polls_html[[1]]
# row <- polls_html[[3]]
parse_row <- function(row) {
  elements <- row |> html_elements("td")

  date <- elements[[1]] |>
    html_text() |>
    str_squish()

  # Dates
  start_date <- if_else(
    str_detect(date, "(\\w+) ([0-9]+) - ([0-9]+)"),
    # Nov 1 - 2
    str_replace(date, "(\\w+) ([0-9]+) - ([0-9]+)", "\\1 \\2"),
    # Oct 31 - Nov 2
    str_replace(date, "(\\w+) ([0-9]+) - (\\w+) ([0-9]+)", "\\1 \\2")
  )
  end_date <- if_else(
    str_detect(date, "(\\w+) ([0-9]+) - ([0-9]+)"),
    # Nov 1 - 2
    str_replace(date, "(\\w+) ([0-9]+) - ([0-9]+)", "\\1 \\3"),
    # Oct 31 - Nov 2
    str_replace(date, "(\\w+) ([0-9]+) - (\\w+) ([0-9]+)", "\\3 \\4")
  )
  date <- end_date

  # Add year if needed
  has_year <- str_detect(date, ", [0-9]{4}")
  date[!has_year] <- paste0(date[!has_year], ", 2024")

  # Parse date
  date <- mdy(date)

  pollster <- elements[[2]] |>
    html_text() |>
    str_squish()
  pollster_rating <- str_extract(
    pollster, "\\((A\\+|A|B\\+|B|B-|C\\+|C|C\\-|D|F)\\)$",
    group = 1
  )

  sample_str <- elements[[3]] |>
    html_text() |>
    str_squish()
  sample_size <- sample_str |>
    str_extract("([0-9]+)", group = 1) |>
    as.numeric()
  sample_type <- sample_str |> str_extract("(lv|rv)", group = 1)

  leader <- elements[[4]] |>
    html_text() |>
    str_squish()

  pct_harris <- elements[[5]] |>
    html_text() |>
    str_squish() |>
    readr::parse_number()
  pct_trump <- elements[[6]] |>
    html_text() |>
    str_squish() |>
    readr::parse_number()

  tibble(
    pollster,
    pollster_rating,
    start_date,
    end_date, 
    date,
    sample_size,
    sample_type,
    leader,
    pct_harris,
    pct_trump
  )
}

polls <- list_rbind(map(polls_html, parse_row))

polls <- polls |>
  filter(date >= .env$start_date) |>
  arrange(desc(date)) |>
  filter(pollster_rating %in% c("A+", "A", "A-", "B+", "B"))

# %%
# https://projects.fivethirtyeight.com/polls/president-general/2024/national/
polls_538 <- read_csv(here("R_Days/Day_4/raw/president_polls.csv")) |>
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    date = end_date
  ) |>
  # National polls
  filter(is.na(state)) |>
  # High-quality polls are negative pollscore
  filter(pollscore <= 0) |>
  # Subset to two-way races
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) |>
  summarize(
    .by = c(pollster, pollscore, start_date, end_date, date, sample_size, candidate_name),
    pct = max(pct)
  ) |>
  # Make wider
  mutate(candidate_name = case_when(
    candidate_name == "Kamala Harris" ~ "harris",
    candidate_name == "Donald Trump" ~ "trump"
  )) |>
  select(pollster, pollster_rating_538 = pollscore, start_date, end_date, date, sample_size, candidate_name, pct) |>
  pivot_wider(
    id_cols = c(pollster, pollster_rating_538, start_date, end_date, date, sample_size),
    names_from = candidate_name, values_from = "pct",
    names_prefix = "pct_"
  ) |>
  filter(start_date >= .env$start_date) |>
  arrange(desc(date))

# %%
# Process polls
avg_polls <- polls |>
  summarize(
    .by = date,
    pct_harris = weighted.mean(pct_harris, w = sample_size),
    pct_trump = weighted.mean(pct_trump, w = sample_size)
  ) |>
  # Make NAs explicit
  full_join(dates, by = "date") |>
  arrange(date) |>
  # Fill-in missing with "Last Observation Carried Forward"
  mutate(
    pct_harris = zoo::na.locf(pct_harris, na.rm = FALSE),
    pct_trump = zoo::na.locf(pct_trump, na.rm = FALSE)
  ) |>
  drop_na()

avg_polls_538 <- polls_538 |>
  summarize(
    .by = date,
    pct_harris = weighted.mean(pct_harris, w = sample_size),
    pct_trump = weighted.mean(pct_trump, w = sample_size)
  ) |>
  # Make NAs explicit
  full_join(dates, by = "date") |>
  arrange(date) |>
  # Fill-in missing with "Last Observation Carried Forward"
  mutate(
    pct_harris = zoo::na.locf(pct_harris, na.rm = FALSE),
    pct_trump = zoo::na.locf(pct_trump, na.rm = FALSE)
  ) |>
  drop_na()

# %% 
# Export data
write_csv(
  polls_538, here("R_Days/Day_4/data/all_polls_538.csv")
)
write_csv(
  avg_polls_538, here("R_Days/Day_4/data/daily_avg_polls_538.csv")
)

