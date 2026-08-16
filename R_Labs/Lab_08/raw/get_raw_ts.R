# %% 
library(tidyverse)
library(tsibble)
library(here)
library(fixest)
library(timeSeriesDataSets)

# %% 
decompose_monthly <- function(df, y, date = "date") { 
  df <- df |> arrange(!!date)
  ma_12 <- slider::slide_dbl(df[[y]], mean, .before = 5, .after = 6, .complete = TRUE)
  df$T_hat <- slider::slide_dbl(ma_12, mean, .before = 1, .after = 0, .complete = TRUE)

  df[[paste0(y, "_detrended")]] <- df[[y]] - df$T_hat
  
  fml <- fixest::xpd(~ i(month(date)), lhs = paste0(y, "_detrended"))
  est <- feols(
    fml, data = df
  )
  df$S_hat <- predict(est, sample = "original")
  df$resid <- df[[y]] - df$T_hat - df$S_hat
  df[[paste0(y, "_detrended")]] <- NULL
  return(df)
}

# %% 
# book_sales is in million of dollars
book_sales <- here("R_Labs/Lab_8/raw/book_store_sales_million.csv") |> 
  read_csv(show_col_types = FALSE) |>
  setNames(c("date", "sales"))

book_sales <- book_sales |>
  decompose_monthly("sales", "date")

plot(
  x = book_sales$date, y = book_sales$sales, type = "l", lwd = 2,
  xlab = NA, ylab = "Average book_sales Price"
)
lines(x = book_sales$date, y = book_sales$T_hat, col = "orange", lwd = 1.5)
lines(x = book_sales$date, y = book_sales$T_hat + book_sales$S_hat, col = "blue", lwd = 1.5)

# %%
# https://fred.stlouisfed.org/series/CUSR0000SETG01
airline <- here("R_Labs/Lab_8/raw/airline_price.csv") |> 
  read_csv(show_col_types = FALSE) |>
  setNames(c("date", "price"))

airline <- airline |>
  decompose_monthly("price", "date")

plot(
  x = airline$date, y = airline$price, type = "l", lwd = 2,
  xlab = NA, ylab = "Average Airline Price"
)
lines(x = airline$date, y = airline$T_hat, col = "orange", lwd = 1.5)
lines(x = airline$date, y = airline$T_hat + airline$S_hat, col = "blue", lwd = 1.5)

# %%
# https://fred.stlouisfed.org/series/CUSR0000SETG01
candy <- here("R_Labs/Lab_8/raw/candy_production.csv") |> 
  read_csv(show_col_types = FALSE) |>
  setNames(c("date", "q_manufactured"))

candy <- candy |>
  decompose_monthly("q_manufactured", "date")

plot(
  x = candy$date, y = candy$q_manufactured, type = "l", lwd = 2,
  xlab = NA, ylab = "Quantity of Candy Manufactured"
)
lines(x = candy$date, y = candy$T_hat, col = "orange", lwd = 1.5)
lines(x = candy$date, y = candy$T_hat + candy$S_hat, col = "blue", lwd = 1.5)

# %% 
# https://www.eia.gov/electricity/data/state/
# EIA-861M Monthly Electric Power Industry Report (released: 10/28/2024)
elec <- readxl::read_xlsx(
  here("R_Labs/Lab_8/raw/eia_electricity_demand.xlsx"), skip = 2
) |> 
  select(
    year = Year, month = Month, state = State, 
    residential_demand = `Megawatthours...6`,
    residential_price = `Cents/kWh...8`,
    total_demand = `Megawatthours...22`,
    total_price = `Cents/kWh...24`
  ) |>
  drop_na() |>
  mutate(
    date = ymd(sprintf("%s-%s-01", year, month)),
    .before = year
  ) |>
  summarize(.by = date, 
    residential_demand = sum(residential_demand),
    total_demand = sum(total_demand),
    residential_expenditure = sum(residential_demand * residential_price),
    total_expenditure = sum(total_demand * total_price)
  ) |>
  mutate(
    residential_price = residential_expenditure / residential_demand,
    total_price = total_expenditure / total_demand
  ) |>
  select(-ends_with("expenditure"))

elec <- elec |>
  decompose_monthly("residential_price", "date")

plot(
  x = elec$date, y = elec$residential_price, type = "l", lwd = 2,
  xlab = NA, ylab = "Price of Electricity"
)
lines(x = elec$date, y = elec$T_hat, col = "orange", lwd = 1.5)
lines(x = elec$date, y = elec$T_hat + elec$S_hat, col = "blue", lwd = 1.5)



# %% 
write_csv(
  book_sales |> select(date, sales), 
  here("R_Labs/Lab_8/data/us_book_sales.csv")
)
write_csv(
  airline |> select(date, price), 
  here("R_Labs/Lab_8/data/us_airline_prices.csv")
)
write_csv(
  candy |> select(date, q_manufactured), 
  here("R_Labs/Lab_8/data/candy_manufacturing.csv")
)
write_csv(
  elec |> select(date, price = residential_price), 
  here("R_Labs/Lab_8/data/electricity_prices.csv")
)
write_csv(
  elec |> select(date, q_demanded = residential_demand), 
  here("R_Labs/Lab_8/data/electricity_demand.csv")
)

