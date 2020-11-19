README
================
Laura Mathews
11/16/2020

# Code needed to change to readme.md:

#render("Project3-ReadMe.md", output_file = "README.md")

# Packages to load

```{r}
#Install and load packages
packages <- c("tidyverse", "readr", "caret", "DBI", "lubridate", "DT", "knitr", "shiny", "shinydashboard", "ggplot2", "rmarkdown", "plotly")

lapply(packages, FUN = install.packages)

lapply(packages, FUN = require, character.only = TRUE)
```

# Run app from github
```{r}
shiny::runGitHub(repo = "lauraem93/Project3", ref = "master", subdir = "FinalProject3")
```
