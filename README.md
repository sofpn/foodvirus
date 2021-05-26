
<!-- README.md is generated from README.Rmd. Please edit that file -->

## EURL verguide

This repository contains the source code for the R Shiny application
EURL-verguide.

## Launching the app

The application can be launched in two ways:

  - Online, at: <https://sofpn.shinyapps.io/EURL-verguide>
  - Via R on your computer, by running the following code from the R
    console:

<!-- end list -->

``` r
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("EURL-verguide", "sofpn", ref = "main")
```
