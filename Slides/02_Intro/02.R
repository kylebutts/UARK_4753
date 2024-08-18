# %%
library(tidyverse)
library(ISLR)
library(ISLR2)
library(here)
library(kfbmisc)
library(patchwork)
library(fixest)
library(FNN)
library(tinytable)

advertising <- read_csv(here("Slides/02_Intro/data/Advertising.csv"))

colors <- c(
  "red_pink" = "#e64173",
  "turquoise" = "#20B2AA",
  "orange" = "#FFA500",
  "red" = "#fb6107",
  "blue" = "#3b3b9a",
  "green" = "#8bb174",
  "purple" = "#6A5ACD",
  "zinc50" = "#fafafa",
  "zinc100" = "#f4f4f5",
  "zinc200" = "#e4e4e7",
  "zinc300" = "#d4d4d8",
  "zinc400" = "#a1a1aa",
  "zinc500" = "#71717a",
  "zinc600" = "#52525b",
  "zinc700" = "#3f3f46",
  "zinc800" = "#27272a",
  "zinc900" = "#18181b",
  "zinc950" = "#09090b"
)


# %%
plot_sales_vs_tv <- advertising |>
  ggplot(aes(x = TV, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm, se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_radio <- advertising |>
  ggplot(aes(x = Radio, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm, se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_news <- advertising |>
  ggplot(aes(x = Newspaper, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm, se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_bivariate <- (plot_sales_vs_tv | plot_sales_vs_radio | plot_sales_vs_news)

kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/sales_bivariate.pdf"),
  plot_sales_bivariate,
  width = 8, height = 3.5
)

#' Highlighting error term $\varepsilon$
# %%
est_tv <- feols(Sales ~ TV, advertising)
advertising$Sales_hat <- predict(est_tv)

(plot_sales_tv_residual <- ggplot(data = advertising) +
  geom_abline(
    intercept = coef(est_tv)["(Intercept)"],
    slope = coef(est_tv)["TV"],
    color = "#5601A5"
  ) +
  geom_point(
    aes(x = TV, y = Sales),
    shape = 21
  ) +
  geom_segment(
    aes(x = TV, xend = TV, y = Sales, yend = Sales_hat),
    color = "#b84242"
  ) +
  ggtext::geom_textbox(
    x = 60, y = 22.5, color = "#b84242",
    label = "Error term, $\\varepsilon$, is difference between model $f(X)$ and observed $Y$",
    box.r = unit(0, "in"),
    width = unit(2.5, "in")
  ) +
  kfbmisc::theme_kyle(base_size = 12)
)

kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/sales_tv_error_term.pdf"),
  plot_sales_tv_residual,
  width = 8, height = 4
)





# %%
# Following Ed Rubin's Machine Learning slides
# https://github.com/edrubin/EC607S20?tab=readme-ov-file

# Generate data
n <- 1e3
set.seed(123)
dgp <- function(x) {
  y <- 0.3 * x^2 - 17
  y <- ifelse(y > 2.5, y, 5 - x)
  y <- abs(y)^0.7
  return(y)
}

tmp_df <- tibble(
  x = runif(n = n, min = -10, max = 10),
  er = rnorm(n = n, sd = 1.5),
  y_systematic = dgp(x),
  y = y_systematic + er
)
bins_10_x <- c(-10, quantile(tmp_df$x, seq(0.1, 0.9, by = 0.1)), 10)
tmp_df$x_bins_10 <- cut(tmp_df$x, bins_10_x, include.lowest = TRUE)

# Estimate
predictions <- tibble(
  x = seq(-10, 10, by = 0.01),
  x_bins_10 = cut(x, bins_10_x, include.lowest = TRUE)
)

predictions$y_hat_linear <-
  feols(y ~ x, tmp_df) |>
  predict(newdata = predictions)

predictions$y_hat_poly_4 <-
  feols(y ~ poly(x, 4), tmp_df) |>
  predict(newdata = predictions)

predictions$y_hat_knn_100 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 100
  )$pred

predictions$y_hat_knn_10 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 5
  )$pred

predictions$y_hat_knn_5 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 5
  )$pred

predictions$y_hat_bins_10 <- tmp_df |>
  feols(y ~ i(x_bins_10)) |>
  predict(newdata = predictions)

# %%
(plot_basic <-
  ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 19, alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 1, alpha = 0.2
  ) +
  labs(x = NULL, y = NULL, title = "Examples of $f$:") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ))


title_parts <- c(
  "\\color[HTML]{E64173}{Line}",
  "\\color[HTML]{20B2AA}{Polynomial $\\left( x^4 \\right)$}",
  "\\color[HTML]{3B3B9A}{Bins of $x$}",
  "\\color[HTML]{8bb174}{KNN of $x$}"
)

(plot_pred_1 <- plot_basic +
  geom_line(
    aes(x = x, y = y_hat_linear),
    data = predictions, linewidth = 1.5,
    color = colors["red_pink"]
  ) +
  labs(
    title = paste0("Examples of $f$: ", paste(title_parts[1], collapse = ", "))
  )

)

(plot_pred_2 <- plot_pred_1 +
  geom_line(
    aes(x = x, y = y_hat_poly_4),
    data = predictions, linewidth = 1.5,
    color = colors["turquoise"]
  ) +
  labs(
    title = paste0("Examples of $f$: ", paste(title_parts[1:2], collapse = ", "))
  )
)

(plot_pred_3 <- plot_pred_2 +
  geom_line(
    aes(x = x, y = y_hat_bins_10, group = x_bins_10),
    data = predictions, linewidth = 1.5,
    color = colors["blue"]
  ) +
  labs(
    title = paste0("Examples of $f$: ", paste(title_parts[1:3], collapse = ", "))
  )
)

(plot_pred_4 <- plot_pred_3 +
  geom_line(
    aes(x = x, y = y_hat_knn_100),
    data = predictions, linewidth = 1.5,
    color = colors["green"]
  ) +
  labs(
    title = paste0("Examples of $f$: ", paste(title_parts[1:4], collapse = ", "))
  )
)

# %%
(plot_dgp <-
  ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 19, alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 1, alpha = 0.2
  ) +
  geom_line(
    aes(x = x, y = y_systematic),
    data = tmp_df |> arrange(x),
    color = colors["zinc800"],
    linewidth = 1
  ) +
  labs(x = NULL, y = NULL, title = "\\color[HTML]{27272a}{True $f(x)$}") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ))

# %%
title_parts <- c(
  "\\color[HTML]{27272a}{True $f(x)$}",
  "\\color[HTML]{E64173}{Line}",
  "\\color[HTML]{20B2AA}{Somewhat flexible}",
  "\\color[HTML]{3b3b9a}{Highly flexible}"
)

(plot_overfitting <- ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 19, alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df, size = 1.5, shape = 1, alpha = 0.2
  ) +
  geom_line(
    aes(x = x, y = y_systematic),
    data = tmp_df |> arrange(x),
    color = colors["zinc800"],
    linewidth = 1, alpha = 0.6
  ) +
  labs(x = NULL, y = NULL, title = " ") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ) +
  geom_line(
    aes(x = x, y = y_hat_linear),
    data = predictions, linewidth = 1, alpha = 0.6,
    color = colors["red_pink"]
  ) +
  geom_line(
    aes(x = x, y = y_hat_knn_100),
    data = predictions, linewidth = 1, alpha = 0.6,
    color = colors["turquoise"]
  ) +
  geom_line(
    aes(x = x, y = y_hat_knn_5),
    data = predictions, linewidth = 1, alpha = 0.6,
    color = colors["blue"]
  ) +
  labs(
    title = paste0("", paste(title_parts, collapse = ", "))
  )
)


# %%
kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_raw.pdf"),
  plot = plot_basic, width = 8, height = 3.8
)

kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_pred_1.pdf"),
  plot = plot_pred_1, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_pred_2.pdf"),
  plot = plot_pred_2, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_pred_3.pdf"),
  plot = plot_pred_3, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_pred_4.pdf"),
  plot = plot_pred_4, width = 8, height = 3.8
)

kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_plot_dgp.pdf"),
  plot = plot_dgp, width = 8, height = 3.8
)
kfbmisc::tikzsave(
  here("Slides/02_Intro/figures/f_examples_overfitting.pdf"),
  plot = plot_overfitting, width = 8, height = 3.8
)


#' # Tables
# %%
set.seed(20240810)
mse_ex <- tibble(
  y = round(runif(5, 0, 10), digits = 1),
  eps = round(rnorm(5, 0, 0.4), digits = 2)
) |>
  mutate(y_hat = y + eps)

tab_mspe_example <- mse_ex |>
  select(y, y_hat, eps) |>
  rename(`$y_i$` = y, `$\\hat{y}_i$` = y_hat, `$\\hat{\\varepsilon}_i$` = eps) |>
  tt() |>
  format_tt(j = 3, fn = function(col) {
    sprintf("\\only<2>{%s}", col)
  }) |>
  theme_tt("striped") |>
  style_tt(tabularray_inner = "colsep = {1em}")

tab_mspe_example |>
  print("latex")

tab_mspe_example |>
  save_tt(here("Slides/02_Intro/tables/mspe_ex.tex"), overwrite = TRUE)

mspe_caculation_sum <- sprintf(
  "\\frac{1}{%s} \\left( %s \\right)",
  nrow(mse_ex),
  paste(sprintf("%s^2", mse_ex$eps), collapse = " + ")
)
mspe_calculation_string <- sprintf(
  "\\begin{align*}\n  \\text{MSPE} &= %s \\\\\n  &= %s\n\\end{align*}",
  mspe_caculation_sum,
  mean(mse_ex$eps^2)
)
cat(mspe_calculation_string,
  file = here("Slides/02_Intro/inputs/mspe_calculation_ex.tex")
)


#' ## Kinds of data
#'
#' Cross-sectional:
#' NYC schools average SAT scores and demographic information
# %%
nyc_sat <- here("Slides/02_Intro/data/nyc_sat.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names()

tab_nyc_sat <- nyc_sat |>
  select(school_id, avg_sat_math = average_score_sat_math, pct_white = percent_white, pct_black = percent_black) |>
  # select(`School ID`, Borough, `Average Score (SAT Math)`, `Percent White`) |>
  _[3:8, ] |>
  tt() |>
  theme_tt("striped") |>
  format_tt(escape = TRUE) |>
  style_tt(tabularray_inner = "colsep = {1em}")

print(tab_nyc_sat, "latex")
tab_nyc_sat |>
  save_tt(here("Slides/02_Intro/tables/data_types_ex_cs.tex"), overwrite = TRUE)

#' Time-series:
# Hourly usage of a bike sharing program in Washington, DC.
# %%
ht <- function(data) {
  rbind(head(data, n = 4), tail(data, n = 3))
}

data(Bikeshare, package = "ISLR2")
head(Bikeshare)

Bikeshare <- Bikeshare |>
  mutate(
    date = lubridate::as_date("2018-12-31") + day,
    month = month(date),
    day = day(date),
    hour = hr
  ) |>
  select(month, day, hour, holiday, weathersit, temp, atemp, hum, windspeed, casual, registered, bikers)

tab_bikeshare <- Bikeshare |>
  select(month, day, hour, bikers, temp) |>
  ht() |>
  tt() |>
  theme_tt("striped") |>
  format_tt(i = 4, fn = \(x) "$\\vdots$") |>
  style_tt(tabularray_inner = "colsep = {1em}")

print(tab_bikeshare, "latex")
tab_bikeshare |>
  save_tt(here("Slides/02_Intro/tables/data_types_ex_ts.tex"), overwrite = TRUE)



# %%
data(Fund, package = "ISLR2")
Fund <- Fund |>
  as_tibble() |>
  mutate(month = cur_group_rows(), .before = 1) |>
  pivot_longer(
    cols = starts_with("Manager"),
    names_prefix = "Manager",
    names_to = "fund_manager",
    names_transform = as.numeric,
    values_to = "return"
  ) |>
  select(fund_manager, month, return) |>
  arrange(fund_manager, month)

tab_fund <- Fund |>
  ht() |>
  tt() |>
  theme_tt("striped") |>
  format_tt(
    j = "return", fn = scales::label_percent(scale = 1, suffix = "\\%")
  ) |>
  format_tt(i = 4, fn = \(x) "$\\vdots$") |>
  format_tt(i = 0, escape = TRUE) |>
  style_tt(tabularray_inner = "colsep = {1em}")

print(tab_fund, "latex")
tab_fund |>
  save_tt(here("Slides/02_Intro/tables/data_types_ex_panel.tex"), overwrite = TRUE)






# %%
