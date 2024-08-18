# %%
library(tidyverse)
library(kfbmisc)
library(AER)
library(fixest)

#' Housing expenditure line of best fit
# %%
slope <- 0.35
intercept <- 400
fake_data <- tibble(
  income = seq(0, 5000, by = 500),
  housing_expenditure = intercept + slope * income
)

(plot_line_of_best_fit <- ggplot(
  fake_data, aes(x = income, y = housing_expenditure)
) +
  geom_blank() +
  geom_abline(
    slope = slope,
    intercept = intercept,
    linewidth = 2
  ) +
  scale_x_continuous(
    limits = c(0, 5000),
    labels = scales::label_dollar(prefix = "\\$")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_dollar(prefix = "\\$")
  ) +
  labs(
    x = "Monthly Income", y = "Housing Expenditure"
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

kfbmisc::tikzsave(
  here::here("Review Notes/Algebra/figures/line_housing.pdf"),
  plot_line_of_best_fit,
  width = 6, height = 3.5
)

# %%
# From https://github.com/maibennett/sta235/
b0 <- 6
b1 <- 8
b2 <- -4
fake_data <- tibble(
  age = seq(25, 65, by = 0.001),
  wage = b0 + b1 * age + b2 * age^2
)

(plot_quadratic <- ggplot(
  fake_data, aes(x = age, y = wage)
) +
  geom_line(
    linewidth = 2
  ) +
  labs(
    x = "Worker's Age", y = "Worker's Wage"
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)


kfbmisc::tikzsave(
  here::here("Review Notes/Algebra/figures/ex_quadratic.pdf"),
  plot_quadratic,
  width = 6, height = 3.5
)

# %%
# Following https://github.com/maibennett/sta235/

data("CPS1985")

CPS1985 <- as_tibble(CPS1985)
# Make look more like 2024 dollars
CPS1985$wage <- 5 + CPS1985$wage

est_linear <- feols(wage ~ 1 + age, CPS1985)
est_quad <- feols(wage ~ 1 + age + I(age^2), CPS1985)

CPS1985$wage_hat_linear <- predict(est_linear)
CPS1985$wage_hat_quad <- predict(est_quad)


(plot_wage_linear <- ggplot(CPS1985) +
  geom_point(
    aes(x = age, y = wage),
    alpha = 0.4
  ) +
  geom_line(
    aes(x = age, y = wage_hat_linear),
    linewidth = 2.5, color = "#BF3984"
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    expand = c(0, 0), labels = scales::label_dollar(prefix = "\\$")
  ) +
  labs(x = "Age", y = "Wage") +
  kfbmisc::theme_kyle(base_size = 14)
)
(plot_wage_quad <- ggplot(CPS1985) +
  geom_point(
    aes(x = age, y = wage),
    alpha = 0.4
  ) +
  geom_line(
    aes(x = age, y = wage_hat_quad),
    linewidth = 2.5, color = "#BF3984"
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    expand = c(0, 0), labels = scales::label_dollar(prefix = "\\$")
  ) +
  labs(x = "Age", y = "Wage") +
  kfbmisc::theme_kyle(base_size = 14)
)

(plot_wage_combined <- ggplot(CPS1985) +
  geom_blank() +
  geom_line(
    aes(x = age, y = wage_hat_linear),
    linewidth = 2.5, color = "#BF3984"
  ) +
  geom_line(
    aes(x = age, y = wage_hat_quad),
    linewidth = 2.5, color = "#5601A5"
  ) +
  scale_y_continuous(
    limits = c(9.5, 16.5),
    expand = c(0, 0), labels = scales::label_dollar(prefix = "\\$")
  ) +
  labs(x = "Age", y = "Wage") +
  kfbmisc::theme_kyle(base_size = 14)
)

kfbmisc::tikzsave(
  here::here("Review Notes/Algebra/figures/wage_linear.pdf"),
  plot_wage_linear,
  width = 6, height = 3
)
kfbmisc::tikzsave(
  here::here("Review Notes/Algebra/figures/wage_quadratic.pdf"),
  plot_wage_quad,
  width = 6, height = 3
)
kfbmisc::tikzsave(
  here::here("Review Notes/Algebra/figures/wage_models.pdf"),
  plot_wage_combined,
  width = 6, height = 3
)
