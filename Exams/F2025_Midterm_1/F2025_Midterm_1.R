# %%
library(tidyverse)
library(fixest)
library(openintro)
data(starbucks, package = "openintro")
starbucks <- as_tibble(starbucks)

## Too few salad and parfait
## starbucks$type |> table()
starbucks <- starbucks |> filter(type != "salad", type != "parfait")

# %%
(p_fat_and_calories <- ggplot(aes(x = fat, y = calories), data = starbucks) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#B3114B",
    linetype = "dashed",
    linewidth = 1.25
  ) +
  labs(
    x = "Fat in Food Item (g)",
    y = "Calories in Food Item (g)"
  ) +
  kfbmisc::theme_kyle(base_size = 12, grid_minor = "h") +
  theme(axis.title = element_text(size = rel(1))))

kfbmisc::tikzsave(
  here::here(
    "Exams/F2025_Midterm_1/figures/plot_starbucks_food_calories_on_fat.pdf"
  ),
  width = 6,
  height = 3.2
)

# %%
feols(
  calories ~ fat,
  data = starbucks,
  vcov = "hc1"
)

feols(
  calories ~ fat + carb,
  data = starbucks,
  vcov = "hc1"
)

feols(calories ~ i(type), data = starbucks)
