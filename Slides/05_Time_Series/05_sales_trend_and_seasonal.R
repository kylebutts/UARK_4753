# %% 
library(tidyverse)
library(here)
library(fixest)

# Data from https://www.kaggle.com/datasets/soumyadiptadas/products-sales-timeseries-data?resource=download
df <- data.frame(
  date = ymd("2019-08-16") + 1:100,
  sales = c(266, 264, 317, 390, 440, 436, 493, 203, 308, 388, 385, 482, 523, 522, 238, 392, 374, 440, 525, 503, 609, 360, 426, 479, 525, 536, 586, 625, 338, 434, 494, 548, 636, 613, 661, 427, 488, 585, 508, 667, 662, 676, 445, 442, 549, 607, 592, 621, 747, 397, 572, 636, 718, 792, 769, 770, 530, 609, 548, 594, 812, 883, 930, 526, 644, 571, 677, 733, 732, 777, 620, 601, 844, 737, 901, 843, 1097, 713, 772, 756, 799, 801, 764, 1068, 709, 647, 840, 865, 1026, 999, 952, 600, 843, 893, 881, 975, 1141, 881, 741, 854)
)

ggplot(df) + 
  geom_line(aes(x = date, y = sales))

df$t <- df$date - min(df$date)

df$sales_hat <- feols(
  sales ~ i(wday(date, label = TRUE)),
  data = df
) |>
  predict()

ggplot(df) + 
  geom_line(aes(x = date, y = sales)) +
  geom_line(
    aes(x = date, y = sales_hat), 
    color = "red"
  )

