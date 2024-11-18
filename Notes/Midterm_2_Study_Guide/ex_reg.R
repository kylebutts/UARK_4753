library(tidyverse)
library(fixest)

df <- read_csv(
  "Notes/Midterm_2_Study_Guide/jewelry_sales.csv",
  show_col_types = FALSE
) |>
  setNames(c("date", "jewelry_sales")) |>
  filter(year(date) < 2008)

ggplot(df) + 
  geom_point(
    aes(x = date, y = jewelry_sales)
  ) + 
  geom_line(
    aes(x = date, y = jewelry_sales)
  ) +
  kfbmisc::theme_kyle()

feols(
  jewelry_sales ~ i(quarter(date)), df, vcov = "hc1"
)

(est_trend_and_season <- feols(
  jewelry_sales ~ year(date) + i(quarter(date)), df, vcov = "hc1"
))

# ggplot() + 
#   geom_line(aes(x = date, y = jewelry_sales), data = df) + 
#   geom_line(aes(x = df$date, y = predict(est_trend_and_season)), color = "red") 


# %% 
# Decomposition
ma_2_by_12 <- function(x) {
  ma_12 <- slider::slide_dbl(x, mean, .before = 5, .after = 6, .complete = TRUE)
  ma_2_by_12 <- slider::slide_dbl(ma_12, mean, .before = 1, .after = 0, .complete = TRUE)

  return(ma_2_by_12)
}

df$T_hat <- ma_2_by_12(df$jewelry_sales)
df$sales_detrended <- df$jewelry_sales - df$T_hat
est_seasonal <- feols(
  sales_detrended ~ i(month(date)), df
)
df$S_hat <- predict(est_seasonal, sample = "original")

(p_decomposition <- ggplot(df) + 
  geom_point(
    aes(x = date, y = jewelry_sales),
    size = 0.6
  ) + 
  geom_line(
    aes(x = date, y = jewelry_sales),
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
      "A" =  kfbmisc::kyle_color("magenta"),
      "B" =  kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$\\hat{T}_t$",
      "B" = "$\\hat{T}_t + \\hat{S}_t$"
    )
  ) +
  labs(
    y = NULL, x = NULL, color = NULL,
    title = "Jewelry Sales"
  ) +
  kfbmisc::theme_kyle() + 
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  )
)


kfbmisc::tikzsave(
 "Notes/Midterm_2_Study_Guide/figures/jewelry_decomposition.pdf",
 p_decomposition, width = 8, height = 4 
)



