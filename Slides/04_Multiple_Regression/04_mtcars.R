library(fixest)

feols(
  mpg ~ i(am) + hp + i(am, hp, ref = 0), mtcars, vcov = "HC1"
)




