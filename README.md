
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stabiot

<!-- badges: start -->

[![R-CMD-check](https://github.com/kaigu1990/stabiot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kaigu1990/stabiot/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `stabiot` is to assist statisticians and programmers in
using R functions and methods to oversee the outputs often produced by
SAS from vendors in clinical trials. The data sets would be ADaM format
preferably, but they do not have to follow the CDISC standard.

In order to ensure the quality and accuracy of the results, I prefer to
wrap mature R package rather than rebuild the statistical methods. For
now the completed sections are listed as shown below.

- Simulation of sample size determination by Bayesian.
- Summarize Least-squares Means from models, such as ANCOVA and MMRM.
- Computing response rate, odds ratio with or without stratification,
  and corresponding confidence interval.

## Installation

You can install the development version of `stabiot` like so:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("kaigu1990/stabiot")
```
