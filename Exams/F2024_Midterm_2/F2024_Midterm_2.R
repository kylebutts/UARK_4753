# %%
library(tidyverse)
library(fixest)

# https://www.kaggle.com/datasets/julienjta/nyc-taxi-traffic
nyc_taxi <- "Exams/F2024_Midterm_2/data/nyc_taxi.csv" |>
  read_csv(show_col_types = FALSE) |>
  mutate(date = date(timestamp)) |>
  summarize(
    .by = date,
    max_n_taxis = max(n_taxis),
    n_taxis_at_noon = n_taxis[
      hour(timestamp) == 12 & minute(timestamp) == 0
    ],
    avg_n_taxis = round(mean(n_taxis), 0)
  ) |>
  mutate(
    day_of_week = wday(date, label = TRUE)
  ) |>
  filter(
    date < ymd("2015-01-20"),
    date >= ymd("2014-09-02")
  )

# %%
(p_raw_data <- ggplot() +
  annotate("rect",
    xmin = ymd("2014-12-24"), xmax = ymd("2015-01-02"), ymin = -Inf, ymax = Inf,
    fill = kfbmisc::tailwind_color("zinc-500"),
    alpha = 0.3
  ) +
  geom_point(
    aes(x = date, y = n_taxis_at_noon),
    data = nyc_taxi,
    size = 0.7
  ) +
  geom_line(
    aes(x = date, y = n_taxis_at_noon),
    data = nyc_taxi,
    linewidth = 0.5
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  labs(x = NULL, y = NULL) +
  kfbmisc::theme_kyle(grid_minor = "h")
)

kfbmisc::tikzsave(
  here::here("Exams/F2024_Midterm_2/figures/nyc_taxis_raw.pdf"),
  p_raw_data, width = 6, height = 3.2
)

# %%
est_daily_pattern <- feols(
  n_taxis_at_noon ~ i(day_of_week),
  data = nyc_taxi, vcov = "HC1"
)
est_daily_pattern




# %% 
# https://fred.stlouisfed.org/series/MSPUS
housing <- "Exams/F2024_Midterm_2/data/median_sales_price_us.csv" |>
  read_csv(show_col_types = FALSE) |>
  setNames(c("date", "median_sales_price"))


housing_recent <- housing |> filter(date >= ymd("2012-01-01"))

(p_median_sales_price <- ggplot() +
  geom_point(
    aes(x = date, y = median_sales_price),
    data = housing_recent,
    size = 0.7
  ) +
  geom_line(
    aes(x = date, y = median_sales_price),
    data = housing_recent,
    linewidth = 0.5
  ) +
  geom_smooth(
    aes(x = date, y = median_sales_price),
    data = housing_recent, method = "lm", formula = y ~ x, se = FALSE,
    linewidth = 0.7, color = kfbmisc::kyle_color("magenta")
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_minor_breaks = "1 year", 
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::label_currency(scale_cut = scales::cut_long_scale(), prefix = "\\$")
  ) +
  labs(x = NULL, y = NULL) +
  kfbmisc::theme_kyle(grid_minor = "v")
)

kfbmisc::tikzsave(
  here::here("Exams/F2024_Midterm_2/figures/median_us_housing_price_raw.pdf"),
  p_median_sales_price, width = 6, height = 3.2
)

