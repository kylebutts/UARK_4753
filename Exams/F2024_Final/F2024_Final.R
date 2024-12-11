# %%
library(tidyverse)
library(fixest)
library(here)

college <- here("R_Labs/Lab_4/data/college_scorecard/college_scorecard.csv") |>
  read_csv() |>
  mutate(
    mean_earnings_10yr_after = parse_number(mean_earnings_10yr_after),
    share_low_income = parse_number(share_low_income),
    private_for_profit = ownership == "Private for-profit"
  )

bitcoin <- read_csv("R_Labs/Lab_9/data/bitcoin_daily.csv")

# %% 
earnings_sat <- feols(
  mean_earnings_10yr_after ~ sat_math_median + sat_math_median^2,
  data = college, vcov = "HC1"
)
fitted <- data.frame(sat_math_median = seq(400, 780, by = 0.5))
fitted$earnings_hat <- predict(earnings_sat, newdata = fitted)

ggplot() + 
  geom_point(
    aes(x = sat_math_median, y = mean_earnings_10yr_after),
    data = college
  ) + 
  geom_line(
    aes(x = sat_math_median, y = earnings_hat),
    data = fitted , color = "blue", linewidth = 1.5
  )


# %%
feols(
  mean_earnings_10yr_after ~ share_low_income,
  data = college, vcov = "HC1"
)

feols(
  mean_earnings_10yr_after ~ i(hbcu) + share_low_income,
  data = college, vcov = "HC1"
)

# %%
(p_bitcoin <- ggplot() +
  geom_line(
    aes(x = date, y = price_close),
    data = bitcoin,
    linewidth = 0.5
  ) +
  scale_y_continuous(
    labels = scales::label_currency(
      scale_cut = scales::cut_long_scale(), prefix = "\\$"
    )
  ) +
  labs(x = NULL, y = NULL) +
  kfbmisc::theme_kyle(grid_minor = "v")
)

kfbmisc::tikzsave(
  here("Exams/F2024_Final/figures/bitcoin_raw.pdf"),
  p_bitcoin, width = 6, height = 3
)

