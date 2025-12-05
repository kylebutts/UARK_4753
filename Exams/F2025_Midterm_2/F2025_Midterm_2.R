# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(kfbmisc) ## remotes::install_github("kylebutts/kfbmisc")
library(slider)
library(patchwork)
fs::dir_create("Exams/F2025_Midterm_2/figures")

## https://pageviews.wmcloud.org/?project=en.wikipedia.org&platform=all-access&agent=user&redirects=1&range=all-time&pages=Formula_One
f1 <- tidylog::left_join(
  read_csv("Exams/F2025_Midterm_2/data/f1_year_specific.csv"),
  read_csv("Exams/F2025_Midterm_2/data/f1_wiki_pageviews.csv"),
) |>
  rename(date = Date) |>
  mutate(date = floor_date(date, "week", week_start = "Monday")) |>
  rowwise() |>
  mutate(
    views = sum(
      c_across(`2015 Formula One World Championship`:`Formula One`),
      na.rm = TRUE
    )
  ) |>
  ungroup()

f1 <- f1 |>
  mutate(date = floor_date(date, "week", week_start = "Monday")) |>
  summarize(
    .by = date,
    views = sum(views)
  ) |>
  mutate(log_views = log(views), month = month(date, label = TRUE))

## https://www.kaggle.com/datasets/jtrotman/formula-1-race-data
races <- read_csv("Exams/F2025_Midterm_2/data/f1_races.csv") |>
  filter(year >= 2015) |>
  pull(date)

race_weeks <- do.call(
  "c",
  (map(races, function(x) {
    seq(x - 7, x, "1 day")
  }))
)

f1 <- f1 |> mutate(is_race_week = +(date %in% race_weeks))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Classical Decomposition
ma_2_by_52 <- function(x) {
  ma_52 <- slider::slide_dbl(
    x,
    mean,
    .before = 25,
    .after = 26,
    .complete = TRUE
  )
  ma_2_by_52 <- slider::slide_dbl(
    ma_52,
    mean,
    .before = 1,
    .after = 0,
    .complete = TRUE
  )

  return(ma_2_by_52)
}

f1$T_hat <- ma_2_by_52(f1$views)
f1$views_detrended <- f1$views - f1$T_hat
est_seasonal <- feols(
  views_detrended ~ i(month(date)),
  f1
)
f1$S_hat <- predict(est_seasonal, sample = "original")

(p_decomposition <- ggplot(f1) +
  geom_line(
    aes(x = date, y = views),
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = T_hat, color = "A"),
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = T_hat + S_hat, color = "B"),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::kyle_color("magenta"),
      "B" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$\\hat{T}_t$",
      "B" = "$\\hat{T}_t + \\hat{S}_t$"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    y = NULL,
    x = NULL,
    color = NULL,
    title = "Weekly F1 Wikipedia Views"
  ) +
  kfbmisc::theme_kyle() +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_trend <- ggplot(f1) +
  geom_line(
    aes(x = date, y = views),
    linewidth = 0.7,
    color = tailwind_color("zinc-800")
  ) +
  geom_line(
    aes(x = date, y = T_hat, color = "A"),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::kyle_color("magenta"),
      "B" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$\\hat{T}_t$",
      "B" = "$\\hat{T}_t + \\hat{S}_t$"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    y = NULL,
    x = NULL,
    color = NULL,
    title = NULL
  ) +
  kfbmisc::theme_kyle(legend = "top"))

(p_seasonal <- ggplot(f1) +
  geom_line(
    aes(x = date, y = S_hat, color = "B", facet = "B"),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::kyle_color("magenta"),
      "B" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$\\hat{T}_t$",
      "B" = "$\\hat{S}_t$"
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ","),
    limits = c(-150000, 150000)
  ) +
  labs(
    y = NULL,
    x = NULL,
    color = NULL,
    title = NULL
  ) +
  kfbmisc::theme_kyle(legend = "top"))

(p_decomposition <- (p_trend / p_seasonal) +
  plot_layout(heights = c(2.25, 1), guides = "collect") &
  labs(color = NULL) &
  theme(
    legend.position = "bottom"
  ))

kfbmisc::tikzsave(
  "Exams/F2025_Midterm_2/figures/p_f1_decomposition.pdf",
  p_decomposition,
  width = 8,
  height = 5
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Time-series regression
(est <- feols(
  log_views ~ year(date) + i(month) + i(is_race_week),
  data = f1,
  vcov = "hc1"
))

rlang::with_options(
  print(est),
  digits = 2
)
##
## OLS estimation, Dep. Var.: log_views
## Observations: 544
## Standard-errors: Heteroskedasticity-robust
##                   Estimate Std. Error t value   Pr(>|t|)
## (Intercept)        -271.55      8.143   -33.3  < 2.2e-16 ***
## year(date)            0.14      0.004    34.8  < 2.2e-16 ***
## month::Feb            0.26      0.066     3.9 1.0985e-04 ***
## month::Mar            0.70      0.071     9.9  < 2.2e-16 ***
## month::Apr            0.42      0.059     7.1 4.6862e-12 ***
## month::May            0.42      0.058     7.2 1.8021e-12 ***
## month::Jun            0.33      0.059     5.6 3.4360e-08 ***
## month::Jul            0.54      0.055     9.7  < 2.2e-16 ***
## month::Aug            0.33      0.065     5.2 3.6351e-07 ***
## month::Sep            0.52      0.058     8.9  < 2.2e-16 ***
## month::Oct            0.50      0.057     8.7  < 2.2e-16 ***
## month::Nov            0.61      0.061     9.9  < 2.2e-16 ***
## month::Dec            0.34      0.091     3.8 1.6936e-04 ***
## is_race_week::TRUE    0.34      0.033    10.6  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## RMSE: 0.313208   Adj. R2: 0.719418
