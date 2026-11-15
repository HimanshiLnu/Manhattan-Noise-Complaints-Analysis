# Manhattan Residential Noise Complaints

A team project for Computational Math and Stats at Yeshiva University. We looked at when residential noise complaints happen in Manhattan using NYC 311 data.

## Question

How do the hour of the day, the day of the week and the season change the number of residential noise complaints?

## What we did

- Turned each complaint record into counts by hour, weekday and season.
- Made charts of complaints by hour, by weekday, by season and a heatmap of weekday against hour.
- Fit a Poisson regression on the complaint counts.
- Built a Shiny dashboard with overview, charts and regression tabs.

This is an observational study. It shows links between time and complaint counts. It does not show that time causes them.

## What we found

Complaints are more common in the late evening and on weekends, especially Saturday and Sunday. The regression shows that hour, weekday and season are all statistically linked to complaint counts.

## Data

NYC Open Data, [311 Service Requests from 2010 to Present](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9).

`dataset_raw.csv` holds 65,335 Manhattan residential noise complaints from January 01, 2025 to December 31, 2025. I kept only the columns the code uses. Street addresses and other location details were removed, and latitude and longitude are rounded to 2 decimals.

## Files

- `manhattan_noise_analysis.Rmd`: the full report with code
- `Manhattan-Noise-Report.pdf`: the report as a PDF
- `Manhattan-Noise-Presentation.pptx`: our presentation
- `app.R`: the Shiny dashboard
- `dataset_raw.csv`: the complaint data

## How to run

Install the R packages first:

```r
install.packages(c("readr", "dplyr", "lubridate", "ggplot2", "tidyr", "knitr",
                   "shiny", "shinydashboard", "tidyverse", "janitor", "broom", "scales"))
```

Then knit `manhattan_noise_analysis.Rmd` in RStudio to rebuild the report. To start the dashboard, open `app.R` and click Run App, or run:

```r
shiny::runApp("app.R")
```

Keep `dataset_raw.csv` in the same folder as the code.

## Team

Team project with 1 other student.
