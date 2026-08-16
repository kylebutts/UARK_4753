# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(kfbmisc) ## remotes::install_github("kylebutts/kfbmisc")
library(slider)
library(patchwork)
fs::dir_create("Exams/F2025_Midterm_2/figures")

## Two subgroup means ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# credit_card <- read_csv(
#   "https://vincentarelbundock.github.io/Rdatasets/csv/AER/CreditCard.csv"
# )
ratings <- read_csv(
  "https://vincentarelbundock.github.io/Rdatasets/csv/AER/TeachingRatings.csv"
)

## 268 male and 195 female instructors
table(ratings$gender)

collapse::qsu(ratings$eval, g = ratings$gender)

feols(eval ~ i(gender), data = ratings)
## OLS estimation, Dep. Var.: eval
## Observations: 463
## Standard-errors: IID
##              Estimate Std. Error  t value  Pr(>|t|)
## (Intercept)  3.901026   0.039330 99.18747 < 2.2e-16 ***
## gender::male 0.168004   0.051695  3.24994 0.0012388 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
diamonds <- read_csv(
  "https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/diamonds.csv"
) |>
  mutate(
    cut = fct(
      cut,
      levels = c("Fair", "Good", "Very Good", "Premium", "Ideal")
    ),
    carat_bin = case_when(
      carat <= 0.5 ~ "x <= 0.5",
      carat <= 1 ~ "0.5 < x <= 1",
      carat <= 2 ~ "1 < x <= 2",
      .default = "x >= 2",
    ),
    carat_bin = fct(
      carat_bin,
      levels = c(
        "x <= 0.5",
        "0.5 < x <= 1",
        "1 < x <= 2",
        "x >= 2"
      )
    )
  )

(est_lin <- feols(
  price ~ i(cut) + carat,
  data = diamonds,
  vcov = "HC1"
))

(est_bin <- feols(
  price ~ i(cut) + i(carat_bin),
  data = diamonds,
  vcov = "HC1"
))

etable(est_lin, est_bin)


## Time-series ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fred <- read_csv(
  "https://fred.stlouisfed.org//graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&stacking=normal&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=UCOGNO,NATURALGAS&scale=left,left&cosd=2000-01-01,2000-01-01&coed=2025-09-01,2025-09-01&line_color=%230073e6,%2310847c&link_values=false,false&line_style=solid,dash&mark_type=none,none&mw=3,3&lw=3,3&ost=-99999,-99999&oet=99999,99999&mma=0,0&fml=a,a&fq=Monthly,Monthly&fam=avg,avg&fgst=lin,lin&fgsnd=2020-02-01,2020-02-01&line_index=1,2&transformation=lin,lin&vintage_date=2025-12-06,2025-12-06&revision_date=2025-12-06,2025-12-06&nd=1992-02-01,2000-01-01"
) |>
  rename(
    date = observation_date,
    consumer_good_production = UCOGNO,
    natural_gas = NATURALGAS
  )

central_park_temp <- read_csv(
  "Exams/F2025_Final/data/nyc_central_park_monthly_temp.csv"
) |>
  rename(date = Date, avg_temp = Value) |>
  mutate(
    date = ymd(sprintf("%s-%s-01", str_sub(date, 1, 4), str_sub(date, 5, 6)))
  )

fred <- fred |>
  full_join(central_park_temp, by = "date") |>
  filter(!is.na(avg_temp) & !is.na(natural_gas))

(p_gas <- ggplot() +
  geom_line(
    aes(x = date, y = natural_gas),
    data = fred,
    color = kfbmisc::kyle_color("blue")
  ) +
  labs(x = NULL, y = NULL, title = "US Natural Gas Consumption") +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(0, 0)
  ) +
  kfbmisc::theme_kyle())

(p_temp <- ggplot() +
  geom_line(
    aes(x = date, y = avg_temp),
    data = fred,
    color = kfbmisc::kyle_color("purple")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Avg. Temperature in Central Park, NYC (Fahrenheit)"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(0, 0)
  ) +
  kfbmisc::theme_kyle())

(p_combined <- p_gas / p_temp)

kfbmisc::tikzsave(
  "Exams/F2025_Final/figures/p_natural_gas_and_temp.pdf",
  p_combined,
  width = 8,
  height = 5
)

(est <- feols(
  natural_gas ~ date +
    i(month(date, label = TRUE)) +
    avg_temp,
  data = fred,
  vcov = "hc1"
))

rlang::with_options(
  print(est),
  digits = 2
)

## OLS estimation, Dep. Var.: natural_gas
## Observations: 129
## Standard-errors: Heteroskedasticity-robust
##                                Estimate Std. Error t value   Pr(>|t|)
## (Intercept)                     1245.00    1.5e+02    8.33 1.9168e-13 ***
## date                               0.16    7.6e-03   21.05  < 2.2e-16 ***
## month(date, label = TRUE)::Feb  -410.92    5.2e+01   -7.93 1.5862e-12 ***
## month(date, label = TRUE)::Mar  -448.72    6.0e+01   -7.45 1.9206e-11 ***
## month(date, label = TRUE)::Apr  -705.47    7.1e+01   -9.98  < 2.2e-16 ***
## month(date, label = TRUE)::May  -624.59    9.2e+01   -6.79 5.0866e-10 ***
## month(date, label = TRUE)::Jun  -374.43    1.1e+02   -3.32 1.2172e-03 **
## month(date, label = TRUE)::Jul    22.44    1.3e+02    0.17 8.6704e-01
## month(date, label = TRUE)::Aug   -46.41    1.3e+02   -0.36 7.2256e-01
## month(date, label = TRUE)::Sep  -434.92    1.1e+02   -4.01 1.0648e-04 ***
## month(date, label = TRUE)::Oct  -589.90    8.4e+01   -7.04 1.4664e-10 ***
## month(date, label = TRUE)::Nov  -511.13    6.5e+01   -7.85 2.4482e-12 ***
## month(date, label = TRUE)::Dec  -181.61    4.4e+01   -4.11 7.5305e-05 ***
## avg_temp                         -22.64    2.8e+00   -8.11 6.2811e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
