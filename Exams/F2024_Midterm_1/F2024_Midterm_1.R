# %% 
library(tidyverse) 
library(fixest)
library(wooldridge)
data(lawsch85, package = "wooldridge")
lawsch85 <- as_tibble(lawsch85)

# Made up increase to 2015 dollars
lawsch85 <- lawsch85 |>
  mutate(
    salary = salary + 50000,
    region = case_when(
      north == 1 ~ "Midwest",
      south == 1 ~ "South",
      east == 1 ~ "Northeast", 
      west == 1 ~ "West"
    ), 
    top10 = (rank <= 10),
    top25 = (rank <= 25)
  ) |>
  filter(!is.na(salary))

# %% 
(p_rank_salary <- ggplot() + 
  geom_point(
    aes(x = rank, y = salary), 
    data = lawsch85
  ) + 
  geom_smooth(
    aes(x = rank, y = salary), 
    data = lawsch85,
    method = "lm", formula = y ~ x, se = FALSE,
    color = "#B3114B", linetype = "dashed", linewidth = 1.25
  ) + 
  geom_smooth(
    aes(x = rank, y = salary), 
    data = lawsch85,
    method = "lm", formula = y ~ poly(x, 4), se = FALSE,
    color = "#ffc517", linetype = "solid", linewidth = 1.25
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(prefix = "\\$")
  ) +
  scale_x_continuous(
    breaks = c(1, 50, 100, 150),
    labels = c("1", "50", "100", "150")
  ) +
  labs(
    x = "Rank of Law School",
    y = "Median Starting Salary"
  ) +
  kfbmisc::theme_kyle(base_size = 10, grid_minor = "h") + 
  theme(axis.title = element_text(size = rel(1)))
)

kfbmisc::tikzsave(
  here::here("Exams/F2024_Midterm_1/figures/plot_law_school_rank_and_salary.pdf"), 
  width = 6, height = 3.2
)

# %% 
feols(
  salary ~ rank, 
  data = lawsch85, vcov = "hc1"
)

feols(
  salary ~ i(region, ref = "Northeast"), 
  data = lawsch85, vcov = "hc1"
)

feols(salary ~ i(top25), data = lawsch85, vcov = "hc1")



