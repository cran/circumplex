## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup, message=FALSE-----------------------------------------------------
library(circumplex)

## -----------------------------------------------------------------------------
instruments()

## -----------------------------------------------------------------------------
csip

## -----------------------------------------------------------------------------
summary(ipipipc)

## -----------------------------------------------------------------------------
anchors(ipipipc)

## -----------------------------------------------------------------------------
norms(ipipipc)

## -----------------------------------------------------------------------------
scales(ipipipc, items = TRUE)

## -----------------------------------------------------------------------------
raw_iipsc

## -----------------------------------------------------------------------------
ips_iipsc <- ipsatize(data = raw_iipsc, items = 1:32, append = FALSE)
print(ips_iipsc)

## -----------------------------------------------------------------------------
round(rowMeans(raw_iipsc, na.rm = TRUE), 2)
round(rowMeans(ips_iipsc, na.rm = TRUE), 2)

## -----------------------------------------------------------------------------
scales(iipsc)

## -----------------------------------------------------------------------------
scale_scores <- score(
  data = raw_iipsc,
  items = 1:32,
  instrument = iipsc,
  append = FALSE
)
print(scale_scores)

## -----------------------------------------------------------------------------
inst <- Filter(
  function(x) inherits(x, "circumplex_instrument"),
  mget(
    utils::data(package = "circumplex")$results[, "Item"],
    envir = as.environment("package:circumplex"),
    ifnotfound = list(NULL)
  )
)
samples <- do.call(rbind, lapply(inst, function(x) x$Norms[[2]]))

n_instruments <- length(inst)
n_samples <- nrow(samples)
n_college <- sum(grepl("college|undergraduate", samples$Population))
n_small <- sum(samples$Size < 300)
n_standardization <- sum(samples$Kind == "standardization")
n_unsourced <- sum(samples$Kind == "unsourced")

## -----------------------------------------------------------------------------
norms(iipsc)

## -----------------------------------------------------------------------------
z_scales <- norm_standardize(
  data = scale_scores,
  scales = 1:8,
  instrument = iipsc,
  sample = 1,
  append = FALSE
)
print(z_scales)

