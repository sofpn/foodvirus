
<!-- README.md is generated from README.Rmd. Please edit that file -->

## EURL verguide

This is the code for the Shiny application ‘EURL-verguide’.

Note that the application is under development, and the output and
format may change over time.

## Launching the app

The application is available online at:
<https://sofpn.shinyapps.io/EURL-verguide>

If you have R installed, you can also launch it directly from your
computer, by pasting and running the following code from the R console:

``` r
if (!requireNamespace("shiny", quietly = TRUE))
    install.packages("shiny")
shiny::runGitHub('EURL-verguide', 'sofpn', ref = 'main')
```
