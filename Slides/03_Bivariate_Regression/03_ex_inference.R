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
