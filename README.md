# DEFEATCorona

## Overview

This repository contains the implementation of the proposed threshold-aware ARM/FISM framework for the analysis of Likert-scale questionnaire data. The framework enables multi-threshold binarization, association rule mining, and interactive visualization for exploratory analysis.

## Implementation

The framework is implemented in the open-source programming language R (version 4.5.2).

* The interactive web application is developed using the shiny package, enabling dynamic user input and real-time visualization.
* Association rules and frequent itemsets are mined using the arules package, which implements the Apriori algorithm.
* Visualizations are generated using highcharter and ggplot2 for interactive and static plots.

## Requirements

Please ensure the following R packages are installed:
```R
install.packages(c(
  "readxl",
  "arules",
  "arulesViz",
  "highcharter",
  "stringr",
  "shiny",
  "shinydashboard",
  "shinycssloaders",
  "ggplot2",
  "DT",
  "dplyr"
))
```

## Data

Due to ethical and privacy constraints, the original datasets cannot be publicly shared.
Example or synthetic datasets are provided to demonstrate the workflow and enable reproducibility.

## Reproducibility

The repository includes scripts and example data to reproduce the main analysis steps and figures presented in the manuscript.
