
<!-- README.md is generated from README.Rmd. Please edit that file -->

# helios <img src="man/figures/helios_hex.png" align="right" height="200" style="float:right; height:200px;">

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/helios/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrc-ide/helios/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> In ancient Greek religion and mythology, Helios is the god who
> personifies the Sun.

`helios` is a stochastic individual-based infectious disease
transmission model that simulates the effect of far ultra-violet light
emitters as a method for curtailing disease transmission. `helios` is
built using the [`individual`](https://github.com/mrc-ide/individual)
package developed at the MRC Centre for Global Infectious Disease
Analysis.

## Installation

You can install the development version of `helios` from GitHub using:

``` r
devtools::install_github("mrc-ide/helios")
```

`helios` depends on the development version of the `individual` package
(version 0.1.18 or later), which `install_github()` installs
automatically. To install it separately, use:

``` r
devtools::install_github("mrc-ide/individual")
```

## Vignettes

- [Get started](https://mrc-ide.github.io/helios/articles/helios.html)
  gives an introduction to use of the package
- [Technical model
  description](https://mrc-ide.github.io/helios/articles/model.html)
  gives a more detailed description of the model

## Working reports

- **May 2024**: [Assessing far UVC interventions with an individual
  based infectious disease
  model](https://mrc-ide.github.io/helios/articles/blueprint.html)
  introduces the `helios` package and demonstrates its application in
  simulating installation of far UVC interventions and their impact on
  disease dynamics
- **July 2024**: [Update to “assessing far UVC interventions with an
  individual based infectious disease
  model”](https://mrc-ide.github.io/helios/articles/blueprint-july.html)
  describes two updates to our initial model
- **September 2025**: [The Impact of Far UVC Interventions on the Burden
  of a Respiratory
  Virus](https://mrc-ide.github.io/helios/articles/blueprint-final-report.html)

### Static reports

Working reports live in `vignettes/articles/` as static articles.
pkgdown renders them without running the model, `R CMD check` ignores
them, and they keep showing the results as published even after the
model changes. Each one notes the commit it was generated from in a
comment below its header.

To create a static article, with `report` set to the name of the `.Rmd`
and `out` to your main checkout’s `vignettes/articles`:

``` r
report <- "my-report"
out <- normalizePath("vignettes/articles")
knitr::opts_knit$set(
  base.dir = out,                   # save figures next to the article
  rmarkdown.pandoc.to = "html",
  bookdown.internal.label = TRUE    # keep figure labels so bookdown numbers figures
)
knitr::opts_chunk$set(fig.path = paste0(report, "_files/figure-html/"))
knitr::knit(paste0(report, ".Rmd"), output = file.path(out, paste0(report, ".Rmd")))
```

Finally, add `<!-- Generated from mrc-ide/helios@<commit> -->` below the
YAML header, check the output for printed local file paths, and link the
article in `_pkgdown.yml`.
