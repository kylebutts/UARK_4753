library(tidyverse)
data(CPS1985, package = "AER")
CPS1985 |>
  as_tibble() |>
  select(-region) |>
  mutate(across(
    c(ethnicity, gender, occupation, sector, union, married),
    as.character
  )) |>
  mutate(
    female = gender == "female",
    union = union == "yes",
    married = married == "yes",
    hispanic = ethnicity == "hispanic",
  ) |>
  mutate(
    has_hs_degree = education >= 12,
    has_college_exp = education > 12,
    .after = education
  ) |>
  select(-gender, -ethnicity)
