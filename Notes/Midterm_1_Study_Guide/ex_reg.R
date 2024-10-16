library(fixest)
feols(mpg ~ i(cyl), mtcars)

feols(
  mpg ~ hp + wt + i(cyl),
  data = mtcars
)

cps <- haven::read_dta("/Users/kylebutts/Documents/Mixtape-Sessions/Courses/Causal-Inference-1/Lab/Lalonde/cps_controls.dta")
cps$annual_earnings <- cps$re78
cps$hs_degree <- as.numeric(cps$education >= 12)

feols(
  annual_earnings ~ age,
  data = cps
)
