## ----include = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# This page runs no chunk besides this one. The lavaan gate is kept so that a
# chunk added later, which fits the model, is skipped when lavaan (which
# circumplex only Suggests) is not installed.
has_lavaan <- requireNamespace("lavaan", quietly = TRUE)
knitr::opts_chunk$set(eval = has_lavaan)
# Output that follows the console width wraps at 77 columns, so with its "#> "
# prefix it fits in 80. Text printed with cat() does not wrap.
options(width = 77)

## ----setup, eval = TRUE----------------------------------------------------
library(circumplex)

