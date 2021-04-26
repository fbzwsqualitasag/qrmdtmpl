
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qrmdreport

<!-- badges: start -->
<!-- badges: end -->

The website for qrmdreport is available at:
<https://fbzwsqualitasag.github.io/qrmdtmpl>. The goal of qrmdreport is
to provide templates for project reports for Qualitas AG.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fbzwsqualitasag/qrmdtmpl")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qrmdtmpl)
## basic example code
rmarkdown::draft(file = 'report1', package = 'qrmdtmpl', template = 'quagprojectreport', create_dir = TRUE)
```
