
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `HERON` - *HE*lpers for *R*iver *O*bservatio*N*

<!-- badges: start -->

[![R-CMD-check](https://github.com/lter/HERON/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lter/HERON/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of HERON is to provide a suite of helpful supplementary
functions for workflows involving the `EGRET` and `SiZer` R packages.

## Installation

You can install the development version of HERON from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lter/HERON")
```

## `HERON` Functions

Here are the functions currently included in `HERON`:

-   **`sizer_slice`**: Extract the `SiZer`-identified slope for a
    specified bandwidth and returns this information as a dataframe

-   **`sizer_aggregate`**: Aggregate `SiZer`-identified slope types
    across all bandwidths and return this information as a dataframe

-   **`sizer_plot`**: Creates base R plot of `SiZer` object with
    horizontal lines at specified bandwidths
