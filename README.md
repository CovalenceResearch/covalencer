
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covalencer <a href="https://covalenceresearch.github.io/covalencer/"><img src="man/figures/logo.png" align="right" height="139" alt="covalencer website" /></a>

<!-- badges: start -->
<!-- badges: end -->

`covalencer` contains various R functions in use at Covalence Research
for health economic modeling, literature synthesis, medical
communication, or just plain old data wrangling. Perhaps most notably,
it provides access to the Covalence ggplot theme and associated color
palettes.

## How to install

You can install the development version of `covalencer` from
[GitHub](https://github.com/CovalenceResearch/covalencer) using:

``` r
# install.packages("pak")
pak::pkg_install("CovalenceResearch/covalencer")
```

## How to use

Please note that the package is very much a work in progress, with
things likely to break (deliberately and not so deliberately). Use the
package at your own risk.

### ggplot theme

Apply `theme_covalence()` to an existing `ggplot` object, like any other
theme:

``` r
library(ggplot2)
library(covalencer)

ggplot(data = diamonds, aes(x = price, y = carat)) +
  geom_point(aes(color = color), alpha = 0.5) +
  scale_colour_covalence_discrete(palette = "complete") +
  facet_wrap(vars(cut)) +
  theme_covalence()
```

<img src="man/figures/README-example-1.png" width="90%" />

``` r

ggplot(data = diamonds, aes(x = cut, y = color)) +
  geom_tile(aes(fill = depth)) +
  scale_fill_covalence_sequential() +
  theme_covalence() +
  theme(
    panel.grid.major = element_blank()
  )
```

<img src="man/figures/README-example-2.png" width="90%" />
