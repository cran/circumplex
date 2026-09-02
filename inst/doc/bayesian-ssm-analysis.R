## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(12345)
library(ggplot2)

## ----setup--------------------------------------------------------------------
library(circumplex)

## ----known-direction----------------------------------------------------------
# Truth: e = 1, a = 2, d = 90 degrees
theta <- as.numeric(octants()) * pi / 180
scores <- 1 + 2 * cos(theta - pi / 2)
fit <- lm(scores ~ cos(theta) + sin(theta))
x_hat <- coef(fit)[["cos(theta)"]]
y_hat <- coef(fit)[["sin(theta)"]]
d_hat <- atan2(y_hat, x_hat) * 180 / pi
round(c(x = x_hat, y = y_hat, d = d_hat), 6)
stopifnot(isTRUE(all.equal(d_hat, 90)))          # atan2(y, x): correct
stopifnot(!isTRUE(all.equal(atan2(x_hat, y_hat) * 180 / pi, 90)))  # swapped

## ----data-prep----------------------------------------------------------------
data("jz2017")
set.seed(12345)
sub <- jz2017[sample(nrow(jz2017), 200), ]
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
theta <- as.numeric(octants()) * pi / 180
dat <- data.frame(
  id = rep(seq_len(200), times = length(scales)),
  cos_theta = rep(cos(theta), each = 200),
  sin_theta = rep(sin(theta), each = 200),
  score = unlist(sub[scales], use.names = FALSE)
)
head(dat)

## ----brms-fit, eval = FALSE---------------------------------------------------
# library(brms)
# bfit <- brm(
#   score ~ cos_theta + sin_theta + (1 | id),
#   data = dat,
#   prior = set_prior("normal(0, 1)", class = "b"),
#   chains = 4, iter = 2000, cores = 4, seed = 12345
# )
# draws <- as.matrix(bfit,
#                    variable = c("b_Intercept", "b_cos_theta", "b_sin_theta"))

## ----load-draws---------------------------------------------------------------
draws <- readRDS("bayesian_ssm_draws.rds")
dim(draws)
head(round(draws, 3))

## ----adapter------------------------------------------------------------------
res <- ssm_draws(draws, type = "parameters")
summary(res)

## ----induced-prior, fig.width = 5, fig.height = 3-----------------------------
x_prior <- rnorm(10000, 0, 1)
y_prior <- rnorm(10000, 0, 1)
a_prior <- sqrt(x_prior^2 + y_prior^2)
ggplot(data.frame(a = a_prior), aes(x = a)) +
  geom_histogram(bins = 60, fill = "grey35") +
  labs(
    x = "Amplitude implied by the priors on x and y",
    y = "Prior draws",
    title = "Rayleigh-shaped induced prior on amplitude"
  ) +
  theme_minimal()
round(c(prior_median = median(a_prior), prior_mass_below_0.1 =
          mean(a_prior < 0.1)), 3)

