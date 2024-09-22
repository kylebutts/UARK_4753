# %% 
get_expr_as_text <- function(x) {
  sq <- rlang::quo_squash(rlang::enquo(x))
  str <- paste(as.character(sq)[-1], collapse = "\n")
  return(str)
}

render_tcblisting <- function(x) {
  sq <- rlang::quo_squash(rlang::enquo(x))
  str <- paste(as.character(sq)[-1], collapse = "\n")
  cat(str)

  
  tf <- tempfile(fileext = ".R")
  cat(str, file = tf)
  styler::style_file(path = tf, strict = TRUE, base_indention = 0L)

  # Run reprex
  reprex <- reprex::reprex(
    input = tf, 
    venue = "r", render = TRUE, advertise = FALSE, tidyverse_quiet = TRUE, html_preview = FALSE
  )

  reprex <- paste0(reprex, collapse = "\n")

  out_str <- sprintf("\\begin{codeblock}\n%s\n\\end{codeblock}", reprex)
  clipr::write_clip(out_str)
}

# %% 
cat(render_tcblisting(x = {
  library(fixest)
  feols(mpg ~ 1, data = mtcars)
  mean(mtcars$mpg)
}))

cat(render_tcblisting(x = {
  library(fixest)
  feols(mpg ~ i(am), data = mtcars)

  mean(mtcars[mtcars$am == 1, ]$mpg)
  mean(mtcars[mtcars$am == 0, ]$mpg)
}))

cat(render_tcblisting(x = {
  library(fixest)
  feols(mpg ~ i(cyl), data = mtcars)
}))
cat(render_tcblisting(x = {
  library(fixest)
  feols(mpg ~ i(cyl, ref = 6), data = mtcars)
}))

cat(render_tcblisting(x = {
  library(fixest)
  feols(mpg ~ 0 + i(cyl), data = mtcars)
}))

# %% 
cat(render_tcblisting(x = {
  cbind(1, with(mtcars, fixest:::i(cyl)))
}))

# %% 
cat(render_tcblisting(x = {
  library(fixest)
  feols(
    log(hp) ~ i(cyl), data = mtcars
  )
}))

cat(render_tcblisting(x = {
  library(fixest)
  feols(
    log(mpg) ~ log(hp), data = mtcars
  )
}))


