README
================
Laura Mathews
11/16/2020

# Packages to load

``` r
#Install and load packages

packages <- c("tidyverse", "readr", "caret", "DBI", "lubridate", "DT", "knitr", "shiny", "shinydashboard", "ggplot2", "rmarkdown", "plotly", "dendextend")

# lapply(packages, FUN = install.packages)

lapply(packages, FUN = require, character.only = TRUE)
```

# Run the shiny app from github

``` r
# shiny::runGitHub(repo = "lauraem93/Project3", ref = "master", subdir = "FinalProject3")
```
