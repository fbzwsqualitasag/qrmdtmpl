
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
draft_quagprojectreport(ps_path = "example_quagprojectreport",
                        pl_repl_value = list(title = "Example Project Report",
                                             author = "Peter von Rohr",
                                             date   = "2021-08-27",
                                             output_format = "pdf_document"))
```
