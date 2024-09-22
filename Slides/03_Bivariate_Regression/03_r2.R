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
