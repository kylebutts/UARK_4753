# %%
library(tidyverse)
library(here)
library(fs)
library(fixest)
library(patchwork)

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
nyc_sat <- here::here("Slides/03_Bivariate_Regression/data/nyc_sat.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names() |>
  filter(
    !is.na(percent_tested), !is.na(average_score_sat_math)
  ) |>
  mutate(percent_tested = as.numeric(gsub("%", "", percent_tested)))

fit <- lm(average_score_sat_reading ~ average_score_sat_math, data = nyc_sat)

nyc_sat$residuals <- residuals(fit)
nyc_sat$predicted <- predict(fit)

nyc_sat_x_axis <- scale_x_continuous(
  name = "Average SAT Math Score",
  limits = c(300, 800),
  expand = expansion(0, 0)
)

nyc_sat_y_axis <- scale_y_continuous(
  name = "Average SAT Reading Score",
  limits = c(300, 800),
  expand = expansion(0.1, 0)
)

(nyc_scatter <- nyc_sat |>
  ggplot(
    aes(x = average_score_sat_math, y = average_score_sat_reading)
  ) +
  geom_point(shape = 1, alpha = 0.8, stroke = 0.25) +
  nyc_sat_x_axis +
  nyc_sat_y_axis +
  kfbmisc::theme_kyle(base_size = 14)
)


(reg_line_of_best_fit <- nyc_sat |>
  ggplot(
    aes(x = average_score_sat_math, y = average_score_sat_reading)
  ) +
  geom_point(shape = 1, alpha = 0.8, stroke = 0.25) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    linewidth = 1.2, color = colors["orange"]
  ) +
  nyc_sat_x_axis +
  nyc_sat_y_axis +
  kfbmisc::theme_kyle(base_size = 14)
)

(reg_resids <- nyc_sat |>
  ggplot(aes(x = average_score_sat_math, y = average_score_sat_reading)) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    color = colors["zinc400"]
  ) +
  geom_segment(
    aes(xend = average_score_sat_math, yend = predicted),
    color = colors["red"],
  ) +
  geom_point(shape = 1, alpha = 0.8, stroke = 0.25) +
  annotate(
    "text",
    x = 640, y = 510,
    label = "Prediction Error for this unit",
    size = 3.5, hjust = 0
  ) +
  annotate(
    "curve",
    x = 637, y = 505, xend = 616, yend = 506,
    curvature = -0.465,
    arrow = arrow(48L, unit(0.05, "inches"), "last", "closed")
  ) +
  nyc_sat_x_axis +
  nyc_sat_y_axis +
  kfbmisc::theme_kyle(base_size = 14)
)

# %%
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_raw_data.pdf"),
  nyc_scatter,
  width = 8, height = 3.5
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_reg_line_of_best_fit.pdf"),
  reg_line_of_best_fit,
  width = 8, height = 3.5
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_resids.pdf"),
  reg_resids,
  width = 8, height = 3.5
)


#' A couple lines with different residuals
# %%
plot_resids <- function(nyc_sat, b0, b1) {
  nyc_sat$predicted <-
    b0 + b1 * nyc_sat$average_score_sat_math

  mse <- with(nyc_sat, mean((average_score_sat_reading - predicted)^2))
  mse_label <- sprintf("MSE$(b_0, b_1)$ = %0.0f", mse)

  ggplot() +
    geom_segment(
      mapping = aes(
        x = average_score_sat_math, y = average_score_sat_reading,
        xend = average_score_sat_math, yend = predicted
      ),
      data = nyc_sat,
      color = colors["red"],
    ) +
    geom_point(
      mapping = aes(x = average_score_sat_math, y = average_score_sat_reading),
      data = nyc_sat,
      shape = 1, alpha = 0.8, stroke = 0.25
    ) +
    geom_abline(intercept = b0, slope = b1) +
    # annotate(
    #   "label",
    #   x = 310, y = 680,
    #   label = mse_label,
    #   size = rel(6), hjust = 0, label.size = 0
    # ) +
    labs(title = mse_label) +
    nyc_sat_x_axis +
    nyc_sat_y_axis +
    kfbmisc::theme_kyle(base_size = 14) +
    theme(
      plot.title = element_text(
        face = "plain", margin = margin(b = 20)
      )
    )
}

mspe_ex_1 <- plot_resids(nyc_sat, b0 = 180, b1 = 0.8)
mspe_ex_2 <- plot_resids(nyc_sat, b0 = 180, b1 = 0.53)
mspe_ex_3 <- plot_resids(nyc_sat, b0 = 70, b1 = 0.86)
fit <- lm(average_score_sat_reading ~ average_score_sat_math, data = nyc_sat)
mspe_ex_ols <- plot_resids(
  nyc_sat,
  b0 = coef(fit)["(Intercept)"],
  b1 = coef(fit)["average_score_sat_math"]
)

kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_ex_mspe_1.pdf"),
  mspe_ex_1,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_ex_mspe_2.pdf"),
  mspe_ex_2,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_ex_mspe_3.pdf"),
  mspe_ex_3,
  width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_ex_mspe_ols.pdf"),
  mspe_ex_ols,
  width = 8, height = 4.2
)

# %%
# OLS by hand
fit <- lm(average_score_sat_reading ~ average_score_sat_math, data = nyc_sat)

cov_x_y <- cov(nyc_sat$average_score_sat_reading, nyc_sat$average_score_sat_math)
var_x <- var(nyc_sat$average_score_sat_math)
x_bar <- mean(nyc_sat$average_score_sat_math)
y_bar <- mean(nyc_sat$average_score_sat_reading)

sat_statistics <- sprintf(
  "
  $\\cov{X, y} = %0.2f$\n
  $\\var{X} = %0.2f$\n
  $\\bar{X} = %0.2f$\n
  $\\bar{y} = %0.2f$\n",
  cov_x_y, var_x, x_bar, y_bar
)
cat(sat_statistics, file = here("Slides/03_Bivariate_Regression/inputs/nyc_sat_reg_statistics.tex"))

sat_solution <- sprintf(
  "$\\beta_1 = \\cov{X, y} / \\var{X} = %0.2f / %0.2f = %0.2f",
  cov_x_y, var_x, cov_x_y / var_x
)
cat(sat_solution, file = here("Slides/03_Bivariate_Regression/inputs/nyc_sat_reg_solution.tex"))

# %%
nyc_sat_w_outlier <- nyc_sat |>
  mutate(important = FALSE) |>
  add_row(average_score_sat_math = 740, average_score_sat_reading = 340, important = TRUE)

(reg_line_outlier <- nyc_sat_w_outlier |>
  ggplot(
    aes(x = average_score_sat_math, y = average_score_sat_reading)
  ) +
  geom_point(shape = 1, alpha = 0.8, stroke = 0.25) +
  geom_smooth(
    data = nyc_sat,
    method = "lm", formula = y ~ x, se = FALSE,
    linewidth = 1.2, color = colors["orange"]
  ) +
  geom_smooth(
    data = nyc_sat_w_outlier,
    method = "lm", formula = y ~ x, se = FALSE,
    linewidth = 1.2, color = colors["blue"]
  ) +
  annotate(
    "curve",
    x = 724, y = 402, xend = 736, yend = 346,
    arrow = arrow(30L, unit(6, "pt"), "last", "closed")
  ) +
  annotate(
    "label",
    x = 723, y = 420, label = "Outlier Added",
    size = 5.6, label.size = 0
  ) +
  nyc_sat_x_axis +
  nyc_sat_y_axis +
  kfbmisc::theme_kyle(base_size = 14)
)

kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/nyc_sat_outlier.pdf"),
  reg_line_outlier,
  width = 8, height = 3.5
)



#' # Regression Inference
# %%
dgp <- function(n = 100, sd_eps = 1.5) {
  df <- tibble(
    x = rnorm(n, 1, sd = 1)
  )
  df$eps <- rnorm(nrow(df), 0, sd = sd_eps)
  df$y <- df$x * 1 + df$eps
  return(df)
}
gen_reg <- function(n = 100) {
  df <- dgp(n = n)
  coef(feols(y ~ x, data = df))
}
coef_to_ggplot_line <- function(coef, alpha = 0.05) {
  geom_abline(
    intercept = coef["(Intercept)"],
    slope = coef["x"],
    linewidth = 1.2, color = colors["purple"],
    alpha = alpha
  )
}

set.seed(20240915)
df <- dgp(n = 100)
fit <- coef(feols(y ~ x, data = df))

reg_samples <- lapply(1:100, function(i) {
  gen_reg(n = 100)
})

(plot_orig_reg <- ggplot() +
  geom_point(
    aes(x = x, y = y),
    data = df,
    shape = 1
  ) +
  geom_abline(
    intercept = fit["(Intercept)"],
    slope = fit["x"],
    linewidth = 1.2, color = colors["purple"]
  ) +
  labs(
    title = "Original Sample",
    x = NULL, y = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "plain", margin = margin(b = 10)
    )
  ))

plot_extra_samples_1 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  labs(title = "Original Sample + 1 Extra Sample")

plot_extra_samples_2 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  labs(title = "Original Sample + 2 Extra Samples")

(plot_extra_samples_3 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[3]], alpha = 0.25) +
  labs(title = "Original Sample + 3 Extra Samples"))

(plot_extra_samples_5 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[3]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[4]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[5]], alpha = 0.25) +
  labs(title = "Original Sample + 5 Extra Samples"))

ggplot_reg_lines <- lapply(reg_samples, function(coef) coef_to_ggplot_line(coef, alpha = 0.05))

(plot_extra_samples_100 <- plot_orig_reg +
  ggplot_reg_lines +
  labs(title = "Original Sample + 100 Extra Samples"))

kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/ex_inference_orig_reg.pdf"),
  plot_orig_reg,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/ex_inference_extra_sample_1.pdf"),
  plot_extra_samples_1,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/ex_inference_extra_sample_2.pdf"),
  plot_extra_samples_2,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/ex_inference_extra_sample_5.pdf"),
  plot_extra_samples_5,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/ex_inference_extra_sample_100.pdf"),
  plot_extra_samples_100,
  width = 8, height = 4
)




#' # R^2 comparisons
# %%
dgp <- function(sd_eps) {
  df <- tibble(
    x = rnorm(100, 1, sd = 1)
  )

  df$eps <- rnorm(100, 0, sd = sd_eps)
  df$y <- df$x * 1 + df$eps

  return(df)
}

plot_reg_and_rsquared <- function(sd_eps) {
  df <- tibble(
    x = rnorm(100, 1, sd = 1)
  )
  df$eps <- rnorm(100, 0, sd = sd_eps)
  df$y <- df$x * 1 + df$eps
  df$y <- scale(df$y, center = TRUE, scale = TRUE)

  est <- feols(y ~ x, data = df)
  r2 <- r2(est, type = "r2")
  r2_title <- sprintf("$R^2 = %0.3f$", r2)

  ggplot() +
    geom_point(
      aes(x = x, y = y),
      data = df
    ) +
    geom_smooth(
      aes(x = x, y = y),
      data = df,
      method = "lm", formula = y ~ x, se = FALSE,
      linewidth = 1.2, color = colors["orange"]
    ) +
    labs(title = r2_title, x = NULL, y = NULL) +
    kfbmisc::theme_kyle(base_size = 10) +
    scale_x_continuous(n.breaks = 8, labels = NULL) +
    scale_y_continuous(n.breaks = 8, labels = NULL) +
    theme(
      plot.title = element_text(
        face = "plain", margin = margin(b = 5)
      )
    )
}

set.seed(20240915)
p1 <- plot_reg_and_rsquared(sd_eps = 0.25)
p2 <- plot_reg_and_rsquared(sd_eps = 0.6)
p3 <- plot_reg_and_rsquared(sd_eps = 2)
p4 <- plot_reg_and_rsquared(sd_eps = 4)
(plot_r2_comparison <-
  (p1 + p2 + p3 + p4) +
  plot_annotation(title = "Comparisons of $R^2$") +
  plot_layout(byrow = TRUE)
)

kfbmisc::tikzsave(
  here("Slides/03_Bivariate_Regression/figures/r2_comparisons.pdf"),
  plot_r2_comparison,
  width = 8, height = 4.5
)
