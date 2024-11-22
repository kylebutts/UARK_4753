# install.packages("janitor")
library(janitor)
# install.packages("fixest")
library(fixest)

nyc <- read.csv("data/nyc_sat.csv")
nyc <- clean_names(nyc)

plot(latitude ~ longitude, data = nyc)
# Looks like NYC :-)

# Calcualate regression by hand 
# nyc <- nyc[, c("average_score_sat_math", "average_score_sat_reading")]
# nyc <- na.omit(nyc) # need to drop NAs manually

ybar <- mean(nyc$average_score_sat_reading)
xbar <- mean(nyc$average_score_sat_math)
var_x <- var(nyc$average_score_sat_math)
cov_x_y <- cov(nyc$average_score_sat_math, nyc$average_score_sat_reading)

beta_1_hat <- cov_x_y / var_x
beta_0_hat <- ybar - xbar * beta_1_hat

beta_0_hat + xbar * beta_1_hat

# Plot  
# plot(
#   y = nyc$average_score_sat_reading,
#   x = nyc$average_score_sat_math
# )
plot(
  average_score_sat_reading ~ average_score_sat_math,
  data = nyc
)
abline(
  a = beta_0_hat, b = beta_1_hat, 
  col = "cyan", lwd = 2
)

# Regression using R
lm(
  average_score_sat_reading ~ average_score_sat_math,
  data = nyc
)

# Using fixest package
est <- feols(
  average_score_sat_reading ~ average_score_sat_math,
  data = nyc
)

# Predict yhat
nyc$yhat <- predict(est)

plot(
  average_score_sat_reading ~ average_score_sat_math, data = nyc
)
points(
  yhat ~ average_score_sat_math, data = nyc, 
  col = "orange"
)

# Predictions by hand
nyc$yhat - (beta_0_hat + beta_1_hat * nyc$average_score_sat_math)

