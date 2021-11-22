
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R-package: foodvirus

<!-- badges: start -->

[![R-CMD-check](https://github.com/sofpn/EURL-verguide/workflows/R-CMD-check/badge.svg)](https://github.com/sofpn/EURL-verguide/actions)
<!-- badges: end -->

Foodvirus contains an application, EURLVerguide, that determines
performance characteristics during verification or in-house validation
of PCR based methods in food virology.

The package is developed and maintained by the EU reference laboratory
(EURL) for foodborne viruses.

## Installation

To install foodvirus, start R (version 4.1 or higher) and enter:

``` r
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")
devtools::install_github("sofpn/foodvirus", build_vignettes = TRUE)
```

## Start the application

Enter the following code to start the EURLVerguide application:

``` r
foodvirus::runEURLVerguide()
```

## More information

Please see the package vignette “EURLVerguide-instructions” for more
information on how to use the application. It is loaded by
`browseVignettes("foodvirus")`.

## Citation

To cite foodvirus, please use the information listed under
`citation("foodvirus")`.
