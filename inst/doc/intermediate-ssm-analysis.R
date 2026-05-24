## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ggplot2)
library(ggforce)
library(kableExtra)
set.seed(12345)

## ----setup--------------------------------------------------------------------
library(circumplex)

## ----group--------------------------------------------------------------------
data("jz2017")
results <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(), 
  grouping = "Gender"
)
summary(results)

## ----group_table, echo = FALSE------------------------------------------------
ssm_table(results, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----group_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"----------
ssm_plot_circle(results)

## ----group_plot2, fig.width = 7.5, fig.height = 4, out.width = "100%"---------
ssm_plot_curve(results)

## ----measures-----------------------------------------------------------------
results2 <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(),
  measures = c("NARPD", "ASPD")
)
summary(results2)

## ----measures_table, echo = FALSE---------------------------------------------
ssm_table(results2, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results2)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----measures_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"-------
ssm_plot_circle(results2)

## ----measures_plot2, fig.width = 7.5, fig.height = 4, out.width = "100%"------
ssm_plot_curve(results2)

## ----general------------------------------------------------------------------
results3 <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(), 
  grouping = "Gender", 
  measures = 10:12)
summary(results3)

## ----general_table, echo = FALSE----------------------------------------------
ssm_table(results3, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results3)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----general_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"--------
ssm_plot_circle(results3)

## ----general_plot2, fig.width = 7.5, fig.height = 4, out.width = "100%"-------
ssm_plot_circle(results3, drop_lowfit = TRUE)

## ----general_plot3, fig.width = 7.5, fig.height = 4, out.width = "100%"-------
ssm_plot_curve(results3)

## ----general_plot4, fig.width = 7.5, fig.height = 4, out.width = "100%"-------
ssm_plot_curve(results3, drop_lowfit = TRUE)

## ----model_contrast-----------------------------------------------------------
results4 <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(), 
  grouping = "Gender", 
  contrast = TRUE
)
summary(results4)

## ----model_table, echo = FALSE------------------------------------------------
ssm_table(results4, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results4)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----model_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"----------
ssm_plot_contrast(results4)

## ----measure_contrast---------------------------------------------------------
results5 <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(), 
  measures = c("NARPD", "ASPD"),
  contrast = TRUE
)
summary(results5)

## ----measure_contrast_table, echo = FALSE, results = "asis"-------------------
ssm_table(results5, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results5)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----measure_contrast_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"----
ssm_plot_contrast(results5)

## ----group_contrast-----------------------------------------------------------
results6 <- ssm_analyze(
  data = jz2017, 
  scales = PANO(), 
  angles = octants(), 
  measures = "BORPD", 
  grouping = "Gender", 
  contrast = TRUE
)
summary(results6)

## ----group_contrast_table, echo = FALSE, results = "asis"---------------------
ssm_table(results6, render = FALSE) %>% 
  knitr::kable(caption = circumplex:::dcaption(results6)) %>% 
  kableExtra::kable_styling(full_width = TRUE, font_size = 12)

## ----group_contrast_plot, fig.width = 7.5, fig.height = 4, out.width = "100%"----
ssm_plot_contrast(results6)

## ----taxonomy, echo = FALSE---------------------------------------------------
msr <- c("FALSE", "FALSE", "FALSE", "TRUE",  "TRUE",  "TRUE",  "TRUE")
grp <- c("FALSE", "TRUE",  "TRUE",  "FALSE", "FALSE", "TRUE",  "TRUE")
ctr <- c("FALSE", "FALSE", "TRUE",  "FALSE", "TRUE",  "FALSE", "TRUE")
tab <- data.frame(
  Usage = c(
    "Examine overall mean profile",
    "Examine groups' mean profiles",
    "Compare groups' mean profiles",
    "Examine variables' correlation profiles",
    "Compare variables' correlation profiles",
    "Examine groups' correlation profiles",
    "Compare groups' correlation profiles"
  ),
  measures = msr,
  grouping = grp,
  contrast = ctr
)
knitr::kable(tab, escape = FALSE) %>% 
  column_spec(1, width = "3in") %>% 
  add_header_above(c("", "Arguments Needed" = 3))

