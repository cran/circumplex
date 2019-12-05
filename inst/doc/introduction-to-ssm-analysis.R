## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(12345)
library(ggforce)
library(kableExtra)
library(ggplot2)
library(tibble)
library(dplyr)
library(knitr)

## ----setup--------------------------------------------------------------------
library(circumplex)

## ----model, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
angles <- c(90, 135, 180, 225, 270, 315, 360, 45)
alabel <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Create plot ------------------------------------------------------------------

ggplot2::ggplot() +
  # Require plot to be square and remove default styling
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  # Expand the axes multiplicatively to fit the labels
  ggplot2::scale_x_continuous(expand = c(0.10, 0)) +
  ggplot2::scale_y_continuous(expand = c(0.10, 0)) +
  # Draw line segments corresponding to the octants
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0,
      y = 0,
      xend = 5 * cos(angles[c(1, 3, 5, 7)] * pi / 180),
      yend = 5 * sin(angles[c(1, 3, 5, 7)] * pi / 180)
    ),
    color = "gray60",
    size = 1
  ) +
  # Draw inner labels for the octant angles
  ggplot2::geom_label(
    aes(
      x = 3 * cos(angles * pi / 180),
      y = 3 * sin(angles * pi / 180),
      label = sprintf("%d\u00B0", angles)
    ),
    size = 5,
    color = "black",
    label.size = NA,
    hjust = "center",
    vjust = "center"
  ) +
  # Draw the circle
  ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 5),
    color = "gray50", size = 1.5
  ) +
  # Draw outer labels for octant abbreviations
  ggplot2::geom_label(
    ggplot2::aes(
      x = 5.1 * cos(angles * pi / 180),
      y = 5.1 * sin(angles * pi / 180),
      label = alabel
    ),
    size = 5,
    color = "black",
    label.size = NA,
    hjust = "outward",
    vjust = "outward"
  )

## ----column, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
data("jz2017")
rmat <- jz2017 %>% 
  dplyr::select(NARPD, PA:NO) %>% 
  cor(method = "pearson")
r <- rmat[2:9, 1]

# Format data for plotting
dat_r <- tibble::tibble(
  Scale = factor(c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")),
  r = r
)

# Create column plot
ggplot2::ggplot(dat_r, ggplot2::aes(x = Scale, y = r)) +
  ggplot2::geom_hline(yintercept = 0, size = 1.25, color = "darkgray") +
  ggplot2::geom_col(position = ggplot2::position_dodge(.9), fill = "red") +
  ggplot2::scale_y_continuous(
    limits = c(-0.02, 0.5)
  ) +
  ggplot2::labs(title = "Scores") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(size = 1.0),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## ----path, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
dat_r <- tibble::tibble(
  Scale = factor(
    c("LM", "PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
    levels = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  ),
  est = r[c(7, 1:8)],
  Angle = c(0, octants())
) %>% 
  dplyr::arrange(Angle)

# Plot correlations as connected point ranges with 95% CI ranges
ggplot2::ggplot(dat_r, ggplot2::aes(x = Angle, y = est)) +
  ggplot2::geom_hline(yintercept = 0, size = 1.25, color = "darkgray") +
  ggplot2::geom_point(size = 3, color = "red") +
  ggplot2::geom_path(size = 1.25, color = "red") +
  ggplot2::geom_label(aes(label = Scale), nudge_y = 0.075) +
  ggplot2::scale_x_continuous(
    limits = c(0, 360),
    breaks = c(0, octants()),
    expand = c(0.05, 0),
    labels = function(x) sprintf("%.0f\U00B0", x)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-0.02, 0.5)
  ) +
  ggplot2::labs(title = "Scores by Angle") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(10, 30, 10, 10), "points"),
    panel.grid.major = ggplot2::element_line(size = 1.0),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## ----curve, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
# Calculate SSM parameters
sp <- ssm_parameters(r, octants())

# Create function for SSM cosine model
f <- function(x) {
  sp$Elev + sp$Ampl * cos((x - sp$Disp) * pi / 180)
}

# Plot correlations along with SSM cosine model
ggplot2::ggplot(dat_r, ggplot2::aes(x = Angle, y = est)) +
  ggplot2::geom_hline(yintercept = 0, size = 1.25, color = "darkgray") +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_path(size = 1.25) +
  ggplot2::stat_function(fun = f, size = 2, color = "red") +
  ggplot2::scale_x_continuous(
    limits = c(0, 360),
    breaks = c(0, octants()),
    expand = c(0.01, 0),
    labels = function(x) sprintf("%.0f\U00B0", x)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-0.02, 0.5)
  ) +
  ggplot2::labs(title = "Cosine Curve Estimated by SSM") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(10, 30, 10, 10), "points"),
    panel.grid.major = ggplot2::element_line(size = 1.0),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## ----residuals, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
# Plot correlations as path, SSM cosine model, and differences
ggplot2::ggplot(dat_r, ggplot2::aes(x = Angle, y = est)) +
  ggplot2::geom_hline(yintercept = 0, size = 1.25, color = "darkgray") +
  ggplot2::stat_function(fun = f, size = 2, color = "gray20") +
  ggplot2::geom_point(size = 5.5, color = "black") +
  ggplot2::geom_path(size = 1.25, color = "black") +
  ggplot2::geom_segment(
    ggplot2::aes(x = Angle, xend = Angle, y = est, yend = f(Angle)),
    size = 4, linetype = "solid", color = "red"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 360),
    breaks = c(0, octants()),
    expand = c(0.01, 0),
    labels = function(x) sprintf("%.0f\U00B0", x)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-0.02, 0.475)
  ) +
  ggplot2::labs(title = sprintf("Fit = %.2f", sp[[6]])) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(10, 30, 10, 10), "points"),
    panel.grid.major = ggplot2::element_line(size = 1.0),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## ----elev, echo = FALSE, out.width = "100%"-----------------------------------
knitr::include_graphics("VIG1-e.gif")

## ----ampl, echo = FALSE, out.width = "100%"-----------------------------------
knitr::include_graphics("VIG1-a.gif")

## ----disp, echo = FALSE, out.width = "100%"-----------------------------------
knitr::include_graphics("VIG1-d.gif")

## ----jz2017-------------------------------------------------------------------
data("jz2017")
print(jz2017)

## ----iipsc, echo = FALSE, fig.width = 7.5, fig.height = 4, out.width = "100%"----
angles <- c(90, 135, 180, 225, 270, 315, 360, 45)
flabel <- c(
  "Domineering",
  "Vindictive",
  "Cold",
  "Socially\nAvoidant",
  "Nonassertive",
  "Easily\nExploited",
  "Overly\nNurturant",
  "Intrusive"
)
alabel <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Create plot ------------------------------------------------------------------

ggplot2::ggplot() +
  # Require plot to be square and remove default styling
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  # Expand both axes multiplicatively to fit the labels
  ggplot2::scale_x_continuous(expand = c(0.30, 0)) +
  ggplot2::scale_y_continuous(expand = c(0.10, 0)) +
  # Draw line segments corresponding to the octants
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0,
      y = 0,
      xend = 5 * cos(angles * pi / 180),
      yend = 5 * sin(angles * pi / 180)
    ),
    color = "gray60",
    size = 0.5
  ) +
  # Draw inner labels for the octant abbreviations
  ggplot2::geom_label(
    ggplot2::aes(
      x = 3.75 * cos(angles * pi / 180),
      y = 3.75 * sin(angles * pi / 180),
      label = alabel
    ),
    size = 5,
    color = "gray40",
    label.size = NA,
    hjust = "center",
    vjust = "center"
  ) +
  # Draw inner labels for the octant angles
  ggplot2::geom_label(
    ggplot2::aes(
      x = 2 * cos(angles * pi / 180),
      y = 2 * sin(angles * pi / 180),
      label = sprintf("%d\u00B0", angles)
    ),
    size = 4,
    color = "gray50",
    label.size = NA,
    hjust = "center",
    vjust = "center"
  ) +
  # Draw the circle
  ggforce::geom_circle(
    ggplot2::aes(x0 = 0, y0 = 0, r = 5),
    color = "gray50", 
    size = 1.5
  ) +
  # Draw outer labels for the octant names
  ggplot2::geom_label(
    ggplot2::aes(
      x = 5.1 * cos(angles * pi / 180),
      y = 5.1 * sin(angles * pi / 180),
      label = flabel
    ),
    size = 5,
    color = "black",
    label.size = NA,
    hjust = "outward",
    vjust = "outward"
  )

## -----------------------------------------------------------------------------
data("iipsc")
jz2017s <- standardize(
  .data = jz2017,
  scales = c(PA, BC, DE, FG, HI, JK, LM, NO),
  angles = c(90, 135, 180, 225, 270, 315, 360, 45),
  instrument = iipsc,
  sample = 1
)
print(jz2017s)

## ----analyze------------------------------------------------------------------
results <- ssm_analyze(
  .data = jz2017s,
  scales = c(PA_z, BC_z, DE_z, FG_z, HI_z, JK_z, LM_z, NO_z),
  angles = c(90, 135, 180, 225, 270, 315, 360, 45)
)

## ----summary1a----------------------------------------------------------------
summary(results)

## ----summary1b----------------------------------------------------------------
results2 <- ssm_analyze(jz2017s, PA_z:NO_z, octants())
summary(results2)

## ----table1, echo = FALSE-----------------------------------------------------
ssm_table(results2, render = FALSE) %>%
  kable(caption = circumplex:::dcaption(results2)) %>%
  kable_styling(full_width = TRUE, font_size = 14)

## ----plot1, fig.width = 7.2, fig.height = 4, out.width = "100%"---------------
ssm_plot(results2)

## ----summary2-----------------------------------------------------------------
results3 <- ssm_analyze(jz2017, PA:NO, octants(), measures = NARPD)
summary(results3)

## ----table2, echo = FALSE-----------------------------------------------------
ssm_table(results3, render = FALSE) %>%
  kable(caption = circumplex:::dcaption(results3)) %>%
  kable_styling(full_width = TRUE, font_size = 14)

## ----plot2, fig.width = 7.5, fig.height = 4, out.width = "100%"---------------
ssm_plot(results3)

